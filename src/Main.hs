{-# LANGUAGE MultiParamTypeClasses
           , ScopedTypeVariables
           , DataKinds
           , OverloadedStrings #-}

module Main where

import Prelude hiding (writeFile)
import Control.Monad.Random (evalRandIO, getRandom, RandomGen, Rand)
import GHC.TypeLits (KnownNat)
import Numeric.LinearAlgebra.Static (L (..), matrix, R (..), vector, vec4,
                                     fromList, extract, (<.>), (#>),
                                     randomVector, RandDist (..))
import Numeric.LinearAlgebra (toList)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO (writeFile)

import Particles (Particle (..), Model (..), resample, filterParticles)
import Simulation (Simulation (..), SimulationPair (..), simulate)


-- State space system matrices
a =         matrix [0, 0, 1, 0,
                    0, 0, 0, 1,
                    0, 0, 0, 0,
                    0, 0, 0, 0] :: L 4 4
b =         matrix [0, 0,
                    0, 0,
                    1, 0,
                    0, 1]       :: L 4 2
c =         matrix [1, 0, 0, 0,
                    0, 1, 0, 0] :: L 2 4


-- Map of the field
xw1 = 5
xw2 = 15
xw3 = 20
xw4 = 30

yw1 = 0
yw2 = 30


-- Create
xRobotStep x vx | x < xw1                             = (xw1, -vx)
                | xw2 < x && x < xw2 + (xw3 - xw2)/2  = (xw2, -vx)
                | xw2 + (xw3 - xw2)/2 <= x && x < xw3 = (xw3, -vx)
                | xw4 < x                             = (xw4, -vx)
                | otherwise                           = (  x,  vx)

yRobotStep y vy | y > yw2   = (yw2, -vy)
                | y < yw1   = (yw1, -vy)
                | otherwise = (  y,  vy)


-- Create a model for the particle filter
model :: (RandomGen g) => Model g 4 2 2
model = Model prediction likelihood
  where
    prediction u s = do
        seed <- getRandom
        let n = 0.05 * randomVector seed Gaussian :: R 4

        let [x, y, vx, vy] = toList . extract $ s + a #> s + b #> u + n

        let (x', vx') = xRobotStep x vx
        let (y', vy') = yRobotStep y vy

        return $ vector [x', y', vx', vy']

    gaussian m s x = exp (-( xm^2 / s ) )
      where
        xm = x - m

    sigma = 1

    likelihood :: R 2 -> R 2 -> R 4 -> Double
    likelihood _ y s =
        let [sx, sy, _, _] = toList $ extract s
            [yx, yy] = toList $ extract y
            yl = gaussian sy sigma yy
        in case [] of
            _ | yy < yw1 || yw2 < yy -> 0.00000001
              | xw1 < sx && sx < xw2 -> yl * gaussian sx sigma (xw2 - yx)
              | xw3 < sx && sx < xw4 -> yl * gaussian sx sigma (xw4 - yx)
              | otherwise            -> 0.00000001


simulation :: RandomGen g => Simulation g 4 2 2
simulation = Simulation simulateRobot measureRobot
  where
    -- Move the robot one step, return its new state and the moving direction
    simulateRobot s = do
        seed <- getRandom
        let u = randomVector seed Gaussian * 0.25

        let [x, y, vx , vy] = toList . extract $ s + a #> s + b #> u

        let (x', vx') = xRobotStep x vx
        let (y', vy') = yRobotStep y vy

        let s' = vector [x', y', vx', vy']

        return (u, s')

    -- Create a noisy measurement from the robot state
    measureRobot s = do
        let [yx, yy] = toList $ extract (c #> s)
        let yx' = if yx < xw2 then xw2 - yx else xw4 - yx

        seed <- getRandom
        return $ (fromList [yx', yy] :: R 2) + 1 * randomVector seed Gaussian



-- Create particles from field data

ny = yw2 - yw1
nx = 29

m = 2
w = 1 / (nx - 1) / (2 * ny + 1) / (m ^ 2)

ps0 = [ Particle (vec4 (x/m) (y/m) 1 (-1)) w | x <- [1 .. nx*m], y <- [1 .. ny*m] ]

-- Create initial state
s0 = vector [25, 5, 1, -1] :: R 4


pair0 = SimulationPair (s0, ps0)

main = do
    pairs <- evalRandIO $ simulate simulation model pair0 100 5

    -- Reorder data
    let pairs' = pair0 : reverse pairs :: [SimulationPair 4]

    writeFile "simulation.json" (encodeToLazyText pairs')
