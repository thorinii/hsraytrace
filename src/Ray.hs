{-# LANGUAGE BangPatterns #-}
module Ray (
  Ray(Ray),
  ray_pointAtTime,
  makeRay
) where

import Prelude as P
import Vector as V


data Ray = Ray { ray_base :: !Vec3
               , ray_direction :: !Vec3 }
  deriving (Show)

ray_pointAtTime :: Ray -> Float -> Vec3
ray_pointAtTime (Ray base dir) time = base `V.add` (dir `scalarmult` time)


makeRay :: Float -> Int -> Int -> Float -> Float -> Ray
makeRay fovX rangeX rangeY xIndex yIndex =
  let !w = fromIntegral rangeX
      !h = fromIntegral rangeY
      !plateDistance = w / (2 * tan (fovX / 2))
      !platePoint = Vec3 (xIndex - w/2) (yIndex - h/2) (plateDistance)
      !direction = V.normalize $ platePoint `V.sub` (Vec3 0 0 0)
  in Ray (Vec3 0 0 0) direction
