{-# LANGUAGE RecordWildCards #-}
module Diablo.M6 where

import Data.Traversable

type Damage     = Double -- ^ Averaged damage
type Chance     = Double -- ^ Chance (0-1)
type Multiplier = Double -- ^ True multiplier (>1)
type SkillBonus = Double -- ^ (Additive) skill multiplier bonus (>0)
type Targets    = Double -- ^ Average number of targets hit by something
type Time       = Double -- ^ Seconds
type Distance   = Double -- ^ Ingame units (yards)
type Count      = Double -- ^ Averaged count of something (eg. targets)

-- Elements available in the game
data Element = Cold | Fire | Physical | Lightning | Poison
  deriving (Show, Eq, Ord, Enum)

-- | A “snapshot” of gear stats and multipliers
data Stats = Stats
  { weaponDmg  :: Damage
  , dexterity  :: Multiplier
  , critChance :: Chance
  , critDamage :: Multiplier

  -- True skill multipliers
  , coldMul, fireMul, physicalMul, lightningMul, poisonMul :: Multiplier
  , eliteMul, sentryMul, rocketMul, grenadeMul, skillMul   :: Multiplier
  , zei'sMul, trappedMul, cullMul :: Multiplier

  -- Additive extra skill bonuses
  , elementalDmg, chakDmg, multishotDmg, clusterDmg, impDmg :: SkillBonus

  -- Not a true multiplier, additive with elements
  , petDmg :: SkillBonus
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
  deriving (Show, Eq)

data EleRune   = BL  | FA  | IA  | LB | NT  deriving (Show, Eq, Ord, Enum)
data ChakRune  = TC  | S   | RD  | B  | SC  deriving (Show, Eq, Ord, Enum)
data MultiRune = FaW | BF  | SF  | FB | A   deriving (Show, Eq, Ord, Enum)
data ClustRune = DA  | SS  | M   | CB | LfB deriving (Show, Eq, Ord, Enum)
data ImpRune   = I   | ChB | O   | R  | GW  deriving (Show, Eq, Ord, Enum)
data SenRune   = ST  | IB  | CoT | PS | GT  deriving (Show, Eq, Ord, Enum)

-- Encapsulated data for a type of skill hit effect
data Hit = Hit
  { hitMult    :: Multiplier
  , hitElem    :: Element
  , hitTargets :: TargetCap
  , hitLength  :: HitLength
  , hitType    :: HitType
  , hitSkill   :: Skill
  }

-- TODO: Consider abilities with differing ranges?
data TargetCap = NoCap   | Cap Integer deriving Show
data HitLength = Instant | DoT Time    deriving Show
data HitType   = Normal  | Rocket | Grenade deriving Show

single :: TargetCap
single = Cap 1

simple :: Multiplier -> Element -> Skill -> Hit
simple m e = Hit m e single Instant Normal

-- Base skill hit types and coefficients
skillHits :: Skill -> [Hit]
skillHits skill = map ($ skill) $ hits skill
 where
  hits (Elemental r) = case r of
    IA -> [ simple 3.00 Fire, Hit 3.15 Fire NoCap (DoT 2) Normal ]
    LB -> [ simple 3.00 Lightning ]
    NT -> [ Hit 3.00 Physical NoCap Instant Normal ]
    -- Splits up to hit 11 targets. This may be too idealistic
    FA -> [ Hit 3.30 Cold (Cap 11) Instant Normal ]
    -- How often does it tick per target?
    BL -> error "Not implemented: Ball Lightning"

  hits (Chakram r) = case r of
    TC -> replicate 2 $ Hit 2.20 Fire NoCap Instant Normal
    S  -> [ Hit 5.00 Poison    NoCap Instant Normal ]
    RD -> [ Hit 3.80 Physical  NoCap Instant Normal ]
    B  -> [ Hit 4.00 Lightning NoCap Instant Normal ]
    SC -> [ Hit 2.00 Physical  NoCap (DoT 1) Normal ]

  hits (Multishot r) = case r of
    FaW -> [ Hit 3.60 Lightning NoCap   Instant Normal ]
    BF  -> [ Hit 3.60 Cold      NoCap   Instant Normal
           , Hit 2.00 Cold      NoCap   Instant Normal ]
    SF  -> [ Hit 3.60 Physical  NoCap   Instant Normal ]
    FB  -> [ Hit 4.60 Physical  NoCap   Instant Normal ]
    A   -> [ Hit 3.60 Fire      NoCap   Instant Normal
           , Hit 3.00 Fire      (Cap 3) Instant Rocket ]

  hits (Cluster r) = case r of
    DA  ->   Hit 5.50 Lightning NoCap   Instant Normal : grenades Lightning
    SS  -> [ Hit 5.50 Physical  NoCap   Instant Normal
           , Hit 6.00 Physical  (Cap 3) Instant Rocket ]
    M   -> [ Hit 5.50 Cold      NoCap   Instant Normal
           , Hit 4.50 Cold      (Cap 5) Instant Rocket ]
    LfB ->   Hit 7.70 Fire      NoCap   Instant Normal : grenades Fire
    -- How many targets hit per grenade?
    CB  -> error "Not implemented: Cluster Bomb"
    -- Note: This assumes every grenade hits the target, which doesn't seem to
    -- be guaranteed. It's probably best to ignore the non-rocket runes for now
    -- to be on the safe side.
    where grenades e = replicate 4 $ Hit 2.20 e NoCap Instant Grenade

  hits (Impale r) = case r of
    I   -> [ simple 7.50 Physical ]
    ChB -> [ simple 7.50 Fire, Hit 5.00 Fire single (DoT 2) Normal ]
    O   -> [ Hit 7.50 Cold      NoCap   Instant Normal ]
    R   -> [ Hit 7.50 Lightning (Cap 3) Instant Normal ]
    -- How exactly do crits get calculated?
    GW  -> error "Not implemented: Grievous Wounds"

  -- Extra damage from sentry only
  hits (Sentry r) = case r of
    ST  -> [ Hit 1.20 Fire single Instant Rocket ]
    CoT -> [ Hit 3.00 Physical NoCap (DoT 1) Normal ]
    _   -> []



-- | Compute a single skill hit's actual damage
hitDamage :: Stats -> Hit -> Damage
hitDamage Stats{..} Hit{..} = base * dex * crit * elem * sentry * skill * ctw * bott * zei's
  where base   = weaponDmg * hitMult * case hitType of
                                         Normal  -> 1
                                         Rocket  -> rocketMul
                                         Grenade -> grenadeMul
        dex    = dexterity
        -- Weighted average of non-crit and crit
        crit   = (1 - critChance) + (critChance * critDamage)
        elem   = petDmg' + case hitElem of
                             Cold      -> coldMul
                             Fire      -> fireMul
                             Physical  -> physicalMul
                             Lightning -> lightningMul
                             Poison    -> poisonMul
          -- Enforcer damage doesn't apply to Spitfire Turret's rockets,
          -- possible bug in Blizzard's implementation.
          where petDmg' = case hitSkill of Sentry ST -> 0; _ -> petDmg
        sentry = sentryMul
        skill  = skillMul + case hitSkill of
                              Elemental _ -> elementalDmg
                              Chakram   _ -> chakDmg
                              Multishot _ -> multishotDmg
                              Cluster   _ -> clusterDmg
                              Impale    _ -> impDmg
                              Sentry    _ -> 1 -- Sentry damage is already
                                               -- included as separate mult
        ctw    = cullMul
        bott   = trappedMul
        zei's  = zei'sMul



-- | Possible models of target placement

data TargetModel
  = Clumped         -- ^ The dream: Everything standing on a single spot
  | Radius Distance -- ^ Spread evenly out evenly within a radius
  | Line   Distance -- ^ Standing in a perfect line of given length

-- | Calculate how many targets will be hit on average for a given combination
intersect :: Count -> TargetModel -> HitPattern -> Count
intersect n _ (HitExact m)  = min n m
intersect n _ HitEverything = n

intersect n Clumped p = case p of
  HitRadius _ -> n
  HitLine _ -> n
  HitSplit m -> min n m
  HitRicochet m _ -> min n (1+m)
  HitPath _ -> n

intersect n (Radius d) p = case p of
  HitRadius r | (d <= r)  -> n
              -- One target is always hit, some average fraction of the rest
              -- gets hits, based on the area coverage
              | otherwise -> 1 + (r*r)/(d*d) * (n-1)
  -- Very strongly simplified, just go by the ratio of areas. I tried thinking
  -- about the exact math for this computational probability problem for
  -- about half an hour before deciding it was probably not worth it.
  HitLine w -> 1 + (2*d*w)/(pi*d*d) * (n-1)
  -- I don't really know how to estimate this in a satisfying way.
  HitSplit _ -> error "Not implemented: HitSplit inside a Radius"
  -- Average number of targets inside ricochet range, up to a cap
  HitRicochet m r -> 1 + min m (intersect n (Radius d) (HitRadius r))
  -- ???
  HitPath _ -> error "Not implemented: HitPath inside a Radius"

intersect n (Line d) p = case p of
  HitRadius r -> 1 + (2*r)/d * (n-1)
  HitLine _ -> n
  -- For these models, we assume we always hit the enemy at the front.
  HitSplit m -> min n m
  HitRicochet m r -> 1 + r/d * (n-1)
  -- This works like a line, except it goes a fixed distance and may not hit
  -- anything at all. Confusing ability, though.
  HitPath B | (d <= 75) -> n
            | otherwise -> 75/d * n
  HitPath _ -> error "Not implemented: HitPath inside a Line"


-- | Sentry rotations and skill timelines

type Timeline a = [(Time, a)]

-- Trivial timeline, just shoot elemental arrow 5x per second
test :: Timeline Skill
test = zip [0,0.2..] $ repeat (Elemental FA)

computeHits :: Timeline Skill -> Timeline Hit
computeHits = clipDots . concatMap (traverse skillHits)
  where clipDots = id -- It looks like DoT effects just stack on top of
                      -- each other, at least for IA and ChB, but more testing
                      -- may be warranted. For now, no adjustment needs to be
                      -- made for DoTs clipping.

computeDamage :: Stats -> Targets -> Timeline Hit -> Timeline Damage
computeDamage stats = map . fmap . hitDamage stats
-- TODO: Implement dynamic buffs?


-- Example stats for testing

stats :: Stats
stats = Stats
  { weaponDmg = (1274+2111)/2 / 1000000
  , dexterity = 1 + 10339/100
  , critChance = 1
  , critDamage = 1 + 5.11
  , coldMul = 1 + 0.15
  , fireMul = 1
  , physicalMul = 1
  , lightningMul = 1
  , poisonMul = 1
  , petDmg = 0 + 0.2880
  , eliteMul = 1
  , sentryMul = 1 + 0.44
  , rocketMul = 1 + 1.00
  , grenadeMul = 1
  , skillMul = 1 + 0.20
  , clusterDmg = 0
  , elementalDmg = 0.29
  , chakDmg = 0
  , multishotDmg = 0
  , impDmg = 0
  , zei'sMul = 1
  , cullMul = 1 + 0.20
  , trappedMul = 1 + 0.2910
  }
