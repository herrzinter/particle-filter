{-# LANGUAGE CPP
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , DataKinds
           , TemplateHaskell
           , PartialTypeSignatures
           , OverloadedStrings #-}


{-|
    Module      : Particles
    Description :
    Copyright   : (c) Christoph Weyer, 2016
    License     : GPL-3
    Maintainer  : herrzinter@yav.in
    Stability   : experimental
    Portability : POSIX
-}


module Particles
    (
    -- *
        Particle (..)
    -- *
    ,   predict
    ,   update
    ,   resample
    ,   filterParticles
    -- *
    ,   Model (..)
    ) where


import Data.List (sortOn, replicate)
import Data.Aeson (ToJSON, object, (.=), toJSON)
import Control.Lens (makeLenses, over, view, set)
import Control.Monad (foldM)
import Control.Monad.Random (RandomGen, Rand)
import GHC.TypeLits (KnownNat)
import Numeric.LinearAlgebra.Static (R, extract)
import Numeric.LinearAlgebra (toList)



type Weight = Double -- ^ Scalar Weight of Particle

-- | Particle with state dimension *state*
data Particle state
    = Particle  {   -- ^ State hypothesis
                    _state   :: R state
                    -- ^ How likely is the current state hypothesis?
                ,   _weight  :: Weight
                }
                deriving (Show)

instance (KnownNat s) => ToJSON (Particle s)
  where
    toJSON (Particle s w) = object
        [   "state"     .= (toList . extract $ s)
        ,   "weight"    .= w
        ]

$(makeLenses ''Particle)


-- | Model Functions
data Model randomGen state input output -- ^ Model Dimensions
    = Model
        {   -- ^ Prediction - Produce a noisy prediction from input and state
            _prediction     :: R input -> R state -> Rand randomGen (R state)
            -- ^ Likelihood - How likely is it that model produced output
        ,   _likelihood     :: R input -> R output -> R state -> Double
        }

$(makeLenses ''Model)


predict
    :: (RandomGen g, KnownNat s, KnownNat u)
    => Model g s u _ -> R u -> [Particle s]
    -> Rand g [Particle s]
predict (Model prediction _) u = sequence . fmap predictParticle
      where
        predictParticle (Particle s w) = do
            s' <- prediction u s
            return $ Particle s' w


update
    :: Model _ s u y -> R u -> R y -> [Particle s]
    -> [Particle s]
update (Model _ likelihood) u y = normalize . fmap updateParticle
  where
    -- Update the weight of a particle by multiplying the likelihood of the
    -- the current state
    updateParticle (Particle s w) = Particle s (w * likelihood u y s)

    -- Normalize the particle weights to their sum
    normalize particles = fmap (weight `over` (/ c)) particles
      where
        c = sum $ map (view weight) particles


-- | Resample a list of particles ie. create a list of equally weighted
-- particles from a list of not equally weighted particles. The weight of
-- a particle corresponds to its probability. If the probability is high, a
-- lot of particles are created from this particle.
resample :: [Particle s] -> [Particle s]
resample ps = concat pss'
  where
    -- Resample the particles ps ordered by their weight
    (_, pss') = foldr resampleParticle (0, []) (sortOn (view weight) ps)

    -- New weight w' is equal among all resampled particles
    w' = 1 / (fromIntegral $ length ps)

    -- Create new particles, if depending on the accumulated weight/probability
    resampleParticle (Particle s w) (wacc, ps) = (wacc'', ps' : ps)
      where
        wacc' = wacc + w
        -- The new number of particles n equals how often the normalized
        -- weight w' fits to the accumulated one
        n = round (wacc' / w')
        -- Subtract the weight of the new particles from the accumulator
        wacc'' = wacc' - (fromIntegral n) * w'
        -- Create n new particles based on the state s of the current
        -- particle
        ps' = replicate n (Particle s w')

filterParticles
    :: (RandomGen g, KnownNat s, KnownNat u, KnownNat y)
    => Model g s u y -> R u -> R y -> [Particle s]
    -> Rand g [Particle s]
filterParticles model u y particles = do
    particles' <- predict model u particles
    return $ update model u y particles'
