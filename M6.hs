{-# LANGUAGE RecordWildCards #-}
module Diablo.M6 where

import Data.Traversable (traverse)
import Prelude hiding (repeat, elem)

type Damage     = Double -- ^ Averaged damage
type Chance     = Double -- ^ Chance (0-1)
type Multiplier = Double -- ^ True multiplier (>1)
type SkillBonus = Double -- ^ (Additive) skill multiplier bonus (>0)
type Targets    = Double -- ^ Average number of targets hit by something
type Time       = Double -- ^ Seconds
type Distance   = Double -- ^ Ingame units (yards)
type Count      = Double -- ^ Averaged count of something (eg. targets)
type Frames     = Int    -- ^ One unit of ingame 60 FPS time

-- Elements available in the game
data Element = Cold | Fire | Physical | Lightning | Poison
  deriving (Show, Eq, Ord, Enum)

-- | A “snapshot” of gear stats, multipliers and enabled skills
data Stats = Stats
  { weaponDmg  :: !Damage
  , dexterity  :: !Multiplier
  , critChance :: !Chance
  , critDamage :: !Multiplier

  -- True skill multipliers
  , coldMul, fireMul, physicalMul, lightningMul, poisonMul :: !Multiplier
  , eliteMul, sentryMul, rocketMul, grenadeMul, skillMul   :: !Multiplier
  , zei'sMul, trappedMul, cullMul :: !Multiplier

  -- Additive extra skill bonuses
  , elementalDmg, chakDmg, multishotDmg, clusterDmg, impDmg, compDmg :: !SkillBonus

  -- Not a true multiplier, additive with elements
  , petDmg :: !SkillBonus

  -- Multiplier on BL due to tick count. Ideally this should also be a
  -- property of targets as well as travel speed, but this is an open problem
  -- for now.
  , ballTicks :: !Multiplier

  -- Things related to sentries and players
  , sentryStats :: !SentryStats
  }
  deriving Show



-- | Available skills and runes

data Skill
  = Elemental EleRune
  | Chakram ChakRune
  | Multishot MultiRune
  | Cluster ClustRune
  | Impale ImpRune
  | Sentry SenRune
  | Companion ComRune
  deriving (Show, Eq)

data EleRune   = BL  | FA  | IA  | LB | NT  deriving (Show, Eq, Ord, Enum)
data ChakRune  = TC  | S   | RD  | B  | SC  deriving (Show, Eq, Ord, Enum)
data MultiRune = FaW | BF  | SF  | FB | A   deriving (Show, Eq, Ord, Enum)
data ClustRune = DA  | SS  | M   | CB | LfB deriving (Show, Eq, Ord, Enum)
data ImpRune   = I   | ChB | O   | R  | GW  deriving (Show, Eq, Ord, Enum)
data SenRune   = ST  | IB  | CoT | PS | GT  deriving (Show, Eq, Ord, Enum)
data ComRune   = SpC | BaC | BoC | FC | WC  deriving (Show, Eq, Ord, Enum)

-- Encapsulated data for a type of skill hit effect
data Hit = Hit
  { hitMult    :: !Multiplier
  , hitElem    :: !Element
  , hitPattern :: !HitPattern
  , hitLength  :: !HitLength
  , hitType    :: !HitType
  , hitSkill   :: !Skill
  }

-- | Types of hit patterns
data HitPattern
  -- | Abilities that hit a guaranteed number of targets, like homing rockets
  --   or normal targeted abilities (the same target can't be hit twice)
  = HitExact Count
  -- | Abilities that hit any number of targets within a given radius
  | HitRadius Distance
  -- | Abilities that travel in a straight line and hit anything they touch,
  --   within a radius
  | HitLine Distance
  -- | Abilities that hit everything (like Multishot)
  | HitEverything
  -- | Abilities that pierce or split up to hit additional enemies behind the
  --   initial target
  | HitSplit Count
  -- | Abilities that ricochet to a number of targets within a given radius
  | HitRicochet Count Distance
  -- | Chakrams that follow a fixed path
  | HitPath ChakRune
  deriving Show

data HitLength = Instant | DoT Time         deriving Show
data HitType   = Normal  | Rocket | Grenade deriving Show

simple :: Multiplier -> Element -> Skill -> Hit
simple m e = Hit m e (HitExact 1) Instant Normal

-- Base skill hit types and coefficients
skillHits :: Skill -> [Hit]
skillHits skill = map ($ skill) $ hits skill
 where
  hits (Elemental r) = case r of
    BL -> [ Hit 3.00 Lightning (HitLine  10) Instant Normal ]
    FA -> [ Hit 3.30 Cold      (HitSplit 10) Instant Normal ]
    IA -> [ simple 3.00 Fire, Hit 3.15 Fire (HitRadius 10) (DoT 2) Normal ]
    LB -> [ simple 3.00 Lightning ]
    -- No idea about what this range actually is
    NT -> [ Hit 3.00 Physical (HitLine 3) Instant Normal ]

  hits (Chakram r) = case r of
    TC -> replicate 2 $ Hit 2.20 Fire (HitPath TC) Instant Normal
    S  -> [ Hit 5.00 Poison    (HitLine 10)   Instant Normal ] -- Estimate
    RD -> [ Hit 3.80 Physical  (HitPath RD)   Instant Normal ]
    B  -> [ Hit 4.00 Lightning (HitPath B)    Instant Normal ]
    SC -> [ Hit 2.00 Physical  (HitRadius 10) (DoT 1) Normal ]

  hits (Multishot r) = case r of
    FaW -> [ Hit 3.60 Lightning HitEverything  Instant Normal ]
    BF  -> [ Hit 3.60 Cold      HitEverything  Instant Normal
           , Hit 2.00 Cold      (HitRadius 15) Instant Normal ]
    SF  -> [ Hit 3.60 Physical  HitEverything  Instant Normal ]
    FB  -> [ Hit 4.60 Physical  HitEverything  Instant Normal ]
    A   -> [ Hit 3.60 Fire      HitEverything  Instant Normal
           , Hit 3.00 Fire      (HitExact 3)   Instant Rocket ]

  hits (Cluster r) = case r of
    DA  ->   Hit 5.50 Lightning (HitRadius 10) Instant Normal : grenades Lightning
    SS  -> [ Hit 5.50 Physical  (HitRadius 10) Instant Normal
           , Hit 6.00 Physical  (HitExact 3)   Instant Rocket ]
    M   -> [ Hit 5.50 Cold      (HitRadius 10) Instant Normal
           , Hit 4.50 Cold      (HitExact 5)   Instant Rocket ]
    CB  -> [ Hit 5.25 Fire      (HitRadius 10) Instant Normal
           , Hit 5.25 Fire      (HitLine 6)    Instant Grenade ]
    LfB ->   Hit 7.70 Fire      (HitRadius 10) Instant Normal : grenades Fire
    where grenades e = replicate 4 $ Hit 2.20 e (HitRadius 6) Instant Grenade

  hits (Impale r) = case r of
    I   -> [ simple 7.50 Physical ]
    ChB -> [ simple 7.50 Fire, Hit 5.00 Fire (HitExact 1) (DoT 2) Normal ]
    O   -> [ Hit 7.50 Cold      (HitLine 5)        Instant Normal ] -- Estimate
    R   -> [ Hit 7.50 Lightning (HitRicochet 2 20) Instant Normal ]
    -- How exactly do crits get calculated?
    GW  -> error "Not implemented: Grievous Wounds"

  -- Extra damage from sentry only
  hits (Sentry r) = case r of
    ST  -> [ Hit 1.20 Fire (HitExact 1) Instant Rocket ]
    -- To avoid headaches, let's just assume CoT hits everything since we're
    -- math gods and can come up with the perfect patterns on the fly
    CoT -> [ Hit 3.00 Physical HitEverything (DoT 1) Normal ]
    _   -> []

  -- Extra damage from companion hits
  hits (Companion r) =
    let single = [ simple 1.00 Physical ]
        -- What is the true range on these?
        aoe    = [ Hit 1.00 Physical (HitRadius 3) Instant Normal ]
    in case r of
         BaC -> single; BoC -> single; FC -> single ++ single
         SpC -> aoe; WC -> aoe


-- | Compute a single skill hit's actual damage
hitDamage :: Stats -> Hit -> Damage
hitDamage Stats{..} Hit{..} = base * dex * crit * elite * elem * sentry * skill * ctw * bott * zei's
  where base   = weaponDmg * hitMult * case hitType of
                                         Normal  -> 1
                                         Rocket  -> rocketMul
                                         Grenade -> grenadeMul
        dex    = dexterity
        -- Weighted average of non-crit and crit
        crit   = (1 - critChance) + (critChance * critDamage)
        elite  = eliteMul
        elem   = petDmg' + case hitElem of
                             Cold      -> coldMul
                             Fire      -> fireMul
                             Physical  -> physicalMul
                             Lightning -> lightningMul
                             Poison    -> poisonMul
          -- Enforcer damage doesn't apply to Spitfire Turret's rockets,
          -- possible bug in Blizzard's implementation.
          where petDmg' = case hitSkill of Sentry ST -> 0; _ -> petDmg
        sentry = case hitSkill of Companion _ -> 1; _ -> sentryMul
        skill  = skillMul + case hitSkill of
                              Elemental _ -> elementalDmg
                              Chakram   _ -> chakDmg
                              Multishot _ -> multishotDmg
                              Cluster   _ -> clusterDmg
                              Impale    _ -> impDmg
                              Companion _ -> compDmg
                              Sentry    _ -> 0 -- Sentry damage is already
                                               -- included as separate mult
        ctw    = cullMul
        bott   = trappedMul
        zei's  = zei'sMul



-- | Possible models of target placement

data TargetModel
  = Clumped Count          -- ^ The dream: Everything standing on a single spot
  | Radius  Count Distance -- ^ Spread evenly out evenly within a radius
  | Line    Count Distance -- ^ Standing in a perfect line of given length

count :: TargetModel -> Count
count (Clumped n  ) = n
count (Radius  n _) = n
count (Line    n _) = n

boss :: TargetModel
boss = Clumped 1

-- | Calculate how many targets will be hit on average for a given combination
intersect :: HitPattern -> TargetModel -> Multiplier
intersect (HitExact m)  tm = min (count tm) m
intersect HitEverything tm = count tm

intersect p (Clumped n) = case p of
  HitRadius _ -> n
  HitLine _ -> n
  HitSplit m -> min n (1+m)
  HitRicochet m _ -> min n (1+m)
  HitPath _ -> n

intersect p (Radius n d) = case p of
  HitRadius r | (d <= r)  -> n
              -- One target is always hit, some average fraction of the rest
              -- gets hits, based on the area coverage
              | otherwise -> 1 + (r*r)/(d*d) * (n-1)
  -- Very strongly simplified, just go by the ratio of areas. I tried thinking
  -- about the exact math for this computational probability problem for
  -- about half an hour before deciding it was probably not worth it.
  HitLine w -> 1 + (2*d*w)/(pi*d*d) * (n-1)
  -- It seems like FA is capable of hitting pretty much everything even in
  -- most suboptimal conditions, so let's just be generous here
  HitSplit m -> min n (1+m)
  -- Average number of targets inside ricochet range, up to a cap
  HitRicochet m r -> 1 + min m (intersect (HitRadius r) (Radius n d))
  -- ???
  HitPath _ -> error "Not implemented: HitPath inside a Radius"

intersect p (Line n d) = case p of
  HitRadius r | (d <= r)  -> n
              | otherwise -> 1 + (2*r)/d * (n-1)
  HitLine _ -> n
  -- For these models, we assume we always hit the enemy at the front.
  HitSplit m -> min n (1+m)
  HitRicochet m r -> 1 + r/d * min m (n-1)
  -- This works like a line, except it goes a fixed distance and may not hit
  -- anything at all. Confusing ability, though.
  HitPath B | (d <= 75) -> n
            | otherwise -> 75/d * n
  HitPath _ -> error "Not implemented: HitPath inside a Line"

-- | Timeline of events that happen during combat.
type Timeline a = [(Time, a)]

-- Let's re-invent FRP because dependencies are heavy

-- | Merge two timelines together (left-biased)
merge :: Timeline a -> Timeline a -> Timeline a
merge xs [] = xs
merge [] ys = ys
merge ls@(x@(t,_):xs) rs@(y@(t',_):ys)
  | t <= t'   = x : merge xs rs
  | otherwise = y : merge ls ys

-- | Offset timestamps in a timeline by a given delay
delay :: Time -> Timeline a -> Timeline a
delay d = map $ \(t,a) -> (d+t,a)

-- | Split a timeline into two portions at a given time. Events that fall on
--   the boundary are included in the left result.
split :: Time -> Timeline a -> (Timeline a, Timeline a)
split t = span ((<=t).fst)

-- | Sum all events over all the entire timeline
summarize :: Num a => Timeline a -> a
summarize = sum . map snd

-- | Repeat an event indefinitely at given interval (>0)
repeat :: Time -> a -> Timeline a
repeat d e = map (\t -> (t,e)) $ iterate (+d) 0

-- | Compute the total actual hits each skill produces
computeHits :: Timeline Skill -> Timeline Hit
computeHits = clipDots . concatMap (traverse skillHits)
  where clipDots = id -- It looks like DoT effects just stack on top of
                      -- each other, at least for IA and ChB, but more testing
                      -- may be warranted. For now, no adjustment needs to be
                      -- made for DoTs clipping.

-- | Compute the actual damage dealt at each hit event, based on stats and
--   target properties
computeDamage :: Stats -> TargetModel -> Timeline Hit -> Timeline Damage
computeDamage stats tm = map (fmap sim)
  where sim :: Hit -> Damage
        sim h = hitDamage stats h             -- Base damage being dealt
                * intersect (hitPattern h) tm -- Multiplier due to target types
                * case hitType h of
                    -- All of these effects are pretty random, so let's just
                    -- assume only about half of them hit. This probably needs
                    -- to be tuned more properly, but a quick test of LfB
                    -- suggests that only about 2 grenades ever hit their
                    -- intended target.
                    Grenade -> 0.5
                    _ -> 1
                * case hitSkill h of
                    Elemental BL -> ballTicks stats
                    _ -> 1

-- | Compute the combat length needed for a given amount of damage to be dealt
computeLength :: Damage -> Timeline Damage -> Time
computeLength hp td = head [ t | (t,d) <- scanl f (0,0) td, d >= hp ]
  where f (_,d) (t,d') = (t,d+d')

-- | Normalize damage by summarizing events within certain timeframes
normalize :: Time -> Timeline Damage -> Timeline Damage
normalize d = go 0
  where go _ [] = []
        go t td = let (l,r) = split (t+d) td in (t, summarize l) : go (t+d) r



-- Simulation of sentry rotations and cooldowns

type SkillSet = [(Skill, Frames)]
data SentryStats = SentryStats
  { sentryRune   :: !SenRune  -- ^ Sentry's active rune
  , sentrySkills :: !SkillSet -- ^ Sentry's configured skillset
  , sentryCD     :: !Time     -- ^ Cooldown on the Sentry ability
  , sentryMax    :: !Int      -- ^ Maximum number of sentries
  , sentryLife   :: !Frames   -- ^ How many frames the sentry is available for
  , sentrySpeed  :: !Frames   -- ^ Frames per sentry attack animation
  } deriving Show

-- | Produce a rotation of sentry abilities
rotate :: SentryStats -> Timeline Skill
rotate SentryStats{..} = merge (go 0 initial) chains
  where go f ss = let (s,ss') = pick f ss
                      t       = fromIntegral f/60
                      rocket  = [(t,Sentry ST) | ST <- [sentryRune]]
                  in  (t,s) : rocket ++ go (f+sentrySpeed) ss'
        -- Everything is ready initially
        initial = [ (s,cd,0) | (s,cd) <- sentrySkills ]
                    -- No ability is ready
        pick _ [] = error "Not implemented: Non-spender bolts"
        pick t (a@(s,cd,n):ss)
          -- An ability is ready, use it immediately; increment its cooldown
          -- time and leave other abilities unchanged
          | t >= n = (s, (s,cd,t+cd) : ss)
          -- An ability is on cooldown, keep looking
          | otherwise = fmap (a:) $ pick t ss
        -- With chains of torment, generate an additional tick per second
        chains = case sentryRune of CoT -> repeat 1 (Sentry CoT); _ -> []

-- | Produce a rotation for all sentries combined, offset by their cooldowns
rotateAll :: SentryStats -> Timeline Skill
rotateAll s@SentryStats{..} = foldr merge [] $ take sentryMax ss
  where ss = iterate (delay sentryCD) $ rotate s

-- Example rotation based on frostfire at 4.15
frostfire :: SkillSet
frostfire = [(Cluster M, 132), (Multishot A, 48), (Elemental FA, 0)]


-- | Simulate an actual combat timeline

simulate :: Stats -> TargetModel -> Timeline Damage
simulate stats tm = computeDamage stats tm $ computeHits rotation
  where rotation = rotateAll (sentryStats stats)

-- Compute eDPS over a timeframe
edps :: Time -> Timeline Damage -> Damage
edps t td = summarize (fst $ split t td) / t


-- Example stats for testing

exampleStats :: Stats
exampleStats = Stats
  { weaponDmg = 1
  , dexterity = 1 + 10000/100
  , critChance = 0.5
  , critDamage = 1 + 5
  , coldMul = 1 + 0.40
  , fireMul = 1
  , physicalMul = 1
  , lightningMul = 1
  , poisonMul = 1
  , petDmg = 0 + 0.30
  , eliteMul = 1 + 0.30
  , sentryMul = 1 + 0.45
  , rocketMul = 1 + 1.00
  , grenadeMul = 1
  --               SA     WC     MfD    SB     BBV    MC     P      GoET
  , skillMul = 1 + 0.00 + 0.30 + 0.20 + 0.30 + 0.30 + 0.20 + 0.15 + 0.10
  , clusterDmg = 0
  , elementalDmg = 0.30
  , chakDmg = 0
  , multishotDmg = 0
  , impDmg = 0
  , compDmg = 0
  , zei'sMul = 1 + 0.30
  , cullMul = 1 + 0.20
  , trappedMul = 1 + 0.30
  , ballTicks = 7
  , sentryStats = SentryStats
    --                  Gem      Shoulders  Paragon    Quiver
    { sentryCD = 6 * (1-0.125) * (1-0.08) * (1-0.1) * (1-0.08)
    , sentryMax = 5
    , sentrySkills = frostfire
    , sentrySpeed = 12
    , sentryLife = 60
    , sentryRune = ST
    }
  }
