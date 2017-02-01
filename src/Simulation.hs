{-# LANGUAGE MultiParamTypeClasses
           , ScopedTypeVariables
           , DataKinds
           , OverloadedStrings #-}


{-|
    Module      : Simulation
    Description :
    Copyright   : (c) Christoph Weyer, 2016
    License     : GPL-3
    Maintainer  : herrzinter@yav.in
    Stability   : experimental
    Portability : POSIX
-}


module Simulation
    (
    -- *
        Simulation (..)
    ,   SimulationPair (..)
    ,   simulate
    ) where


import Control.Monad.Random (RandomGen, Rand)
import GHC.TypeLits (KnownNat)
import Numeric.LinearAlgebra.Static (R (..), extract)
import Numeric.LinearAlgebra (toList)
import Data.Aeson (ToJSON, object, (.=), toJSON)

import Particles (Model, Particle (..), filterParticles, resample)


data Simulation randomGen state input output =
    Simulation
        {   _stepSimulation     :: R state -> Rand randomGen (R input, R state)
        ,   _measureSimulation  :: R state -> Rand randomGen (R output)
        }


newtype SimulationPair s = SimulationPair (R s, [Particle s])


instance (KnownNat s) => ToJSON (SimulationPair s)
  where
     toJSON (SimulationPair (s, ps)) = object
        [  "state"      .= (toList . extract $ s)
        ,   "particles" .= toJSON ps
        ]


simulate
    :: (RandomGen g, KnownNat s, KnownNat u, KnownNat y)
    => Simulation g s u y -> Model g s u y -> SimulationPair s -> Integer
    -> Integer
    -> Rand g [(SimulationPair s)]
simulate (Simulation stepSimulation measure) model (SimulationPair pair) isteps iresample = do
    pairss <- simulate' [] pair isteps iresample
    return $ fmap (\pair -> SimulationPair pair) (concat pairss)
  where
    simulate' pairs pair isteps iresample
        | isteps >= 0 = do
            ((s', ps') : pairs') <- steps [] pair iresample
            let ps'' = resample ps'
            let isteps' = isteps - iresample
            simulate' (pairs' : pairs) (s', ps'') isteps' iresample
        | otherwise = return ([pair] : pairs)

    steps pairs pair isteps
        | isteps > 0 = do
            pair' <- step pair
            steps (pair' : pairs) pair' (isteps - 1)
        | otherwise = return $ pairs

    step (s, ps) = do
        (u, s') <- stepSimulation s
        y <- measure s'
        ps' <- filterParticles model u y ps
        return $ (s', ps')

