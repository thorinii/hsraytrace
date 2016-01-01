module Intersection (
  Intersection(Intersection),
  intersect
) where

import Prelude as P
import Vector as V
import Ray
import Shape


data Intersection = Intersection { intersection_point :: !Vec3 }
  deriving (Show)


-- Minimum ray distance for an intersection
epsilon :: Float
epsilon = 0.001


intersect :: Ray -> Shape -> Maybe Intersection
-- Algorithm from https://github.com/LWJGL/lwjgl3-wiki/wiki/2.6.1.-Ray-tracing-with-OpenGL-Compute-Shaders-%28Part-I%29#intersection-testing
intersect ray@(Ray base dir) (Box box_min box_max) =
  let tMin = (box_min `V.sub` base) `V.div` dir
      tMax = (box_max `V.sub` base) `V.div` dir
      time1 = V.min tMin tMax
      time2 = V.max tMin tMax
      timeNear = P.max (V.x time1) (P.max (V.y time1) (V.z time1))
      timeFar = P.min (V.x time2) (P.min (V.y time2) (V.z time2))
      hit = timeNear > epsilon && timeNear < timeFar
      point = ray_pointAtTime ray timeNear
  in if hit
     then Just $ Intersection point
     else Nothing

intersect (Ray base dir) (Translate inner translation) =
  let translatedRay = Ray (base `V.sub` translation) dir
  in intersect translatedRay inner
