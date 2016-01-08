{-# LANGUAGE BangPatterns #-}
module Intersection (
  Intersection(Intersection),
  intersect,
  rotateVector
) where

import Prelude as P
import Data.Maybe (fromMaybe)

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
  let !tMin = (box_min `V.sub` base) `V.div` dir
      !tMax = (box_max `V.sub` base) `V.div` dir
      !time1 = V.min tMin tMax
      !time2 = V.max tMin tMax
      !timeNear = P.max (V.x time1) (P.max (V.y time1) (V.z time1))
      !timeFar = P.min (V.x time2) (P.min (V.y time2) (V.z time2))
      !hit = timeNear > epsilon && timeNear < timeFar
      point = ray_pointAtTime ray timeNear
  in if hit
     then Just $ Intersection point
     else Nothing

intersect ray@(Ray base _) (GroupPair a b) =
  let !iaM = intersect ray a
      !ibM = intersect ray b
      !infinity = 1.0/0.0
      d :: Maybe Intersection -> Maybe Float
      d m = fmap (\(Intersection p) -> V.mag (p `V.sub` base)) m
      !distanceA = fromMaybe infinity (d iaM)
      !distanceB = fromMaybe infinity (d ibM)
  in if distanceA < distanceB
     then iaM
     else ibM

intersect (Ray base dir) (Translate inner translation) =
  let !translatedRay = Ray (base `V.sub` translation) dir
  in intersect translatedRay inner

intersect (Ray base dir) (Rotate inner x y z) =
  let !newBase = rotateVector (-x) (-y) (-z) base
      !newDir = rotateVector (-x) (-y) (-z) dir
      !rotatedRay = Ray newBase newDir
  in intersect rotatedRay inner

rotateVector :: Float -> Float -> Float -> Vec3 -> Vec3
rotateVector ax ay az (Vec3 vx vy vz) =
  let !cs_x = cos ax
      !sn_x = sin ax
      !cs_y = cos ay
      !sn_y = sin ay
      !cs_z = cos az
      !sn_z = sin az

      !vx' = vx
      !vy' = vy * cs_x - vz * sn_x
      !vz' = vy * sn_x + vz * cs_x

      !vx'' = vx' * cs_y - vz' * sn_y
      !vy'' = vy'
      !vz'' = vx' * sn_y + vz' * cs_y

      !vx''' = vx'' * cs_z - vy'' * sn_z
      !vy''' = vx'' * sn_z + vy'' * cs_z
      !vz''' = vz''
  in Vec3 vx''' vy''' vz'''
