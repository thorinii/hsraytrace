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


makeRay :: Int -> Int -> Int -> Int -> Float -> Ray
makeRay rangeX rangeY xIndex yIndex pixelsPerUnit =
  let x = (fromIntegral (xIndex - rangeX `P.div` 2)) / pixelsPerUnit
      y = (fromIntegral (yIndex - rangeY `P.div` 2)) / pixelsPerUnit
  in Ray (Vec3 x y 0) (Vec3 0 0 1)
