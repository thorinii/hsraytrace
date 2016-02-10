{-# LANGUAGE BangPatterns #-}
module Intersection (
  Intersection(Intersection),
  intersect,
  rotateVector,
  marchToIntersection
) where

import Prelude as P
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust)

import Vector as V
import Ray
import Shape
import Voxel (Vec3i(Vec3i), VoxelGrid(VoxelGrid), indexIntoVoxelGrid, cellAt, marchTillEscape)


data Intersection = Intersection { intersection_point :: !Vec3
                                 , intersection_normal :: !Vec3 }
  deriving (Show)


-- Minimum ray distance for an intersection
epsilon :: Float
epsilon = 0.001


{-# INLINE intersect #-}

intersect :: Ray -> Shape -> Maybe Intersection
-- -- Algorithm from https://github.com/LWJGL/lwjgl3-wiki/wiki/2.6.1.-Ray-tracing-with-OpenGL-Compute-Shaders-%28Part-I%29#intersection-testing
-- intersect ray@(Ray base dir) (Box box_min box_max) =
--   let !tMin = (box_min `V.sub` base) `V.div` dir
--       !tMax = (box_max `V.sub` base) `V.div` dir
--       !time1 = V.min tMin tMax
--       !time2 = V.max tMin tMax
--       !timeNear = P.max (V.x time1) (P.max (V.y time1) (V.z time1))
--       !timeFar = P.min (V.x time2) (P.min (V.y time2) (V.z time2))
--       !hit = timeNear > epsilon && timeNear < timeFar
--       point = ray_pointAtTime ray timeNear
--   in if hit
--      then Just $ Intersection point
--      else Nothing


intersect ray@(Ray base _) (GroupPair a b) =
  let !iaM = intersect ray a
      !ibM = intersect ray b
      !infinity = 1.0/0.0
      d :: Maybe Intersection -> Maybe Float
      d m = fmap (\(Intersection p _) -> V.mag (p `V.sub` base)) m
      !distanceA = fromMaybe infinity (d iaM)
      !distanceB = fromMaybe infinity (d ibM)
  in if distanceA < distanceB
     then iaM
     else ibM

intersect (Ray base dir) (Translate inner translation) =
  let !translatedRay = Ray (base `V.sub` translation) dir
      !result = intersect translatedRay inner
      !mapped = case result of
                  Just (Intersection p n) -> Just $ Intersection (p `V.add` translation) n
                  otherwise -> Nothing
  in mapped

intersect (Ray base dir) (Rotate inner x y z) =
  let !newBase = rotateVector (-x) (-y) (-z) base
      !newDir = rotateVector (-x) (-y) (-z) dir
      !rotatedRay = Ray newBase newDir
      !result = intersect rotatedRay inner
      !mapped = case result of
                  Just (Intersection p n) -> Just $ Intersection (rotateVector x y z p) n
                  otherwise -> Nothing
  in mapped


intersect ray@(Ray base dir) (VoxelBox box_min box_max grid) =
  let !tMin = (box_min `V.sub` base) `V.div` dir
      !tMax = (box_max `V.sub` base) `V.div` dir
      !time1 = V.min tMin tMax
      !time2 = V.max tMin tMax
      !timeNear = P.max (V.x time1) (P.max (V.y time1) (V.z time1))
      !timeFar = P.min (V.x time2) (P.min (V.y time2) (V.z time2))
      !hit = timeNear > epsilon && timeNear < timeFar
      point = ray_pointAtTime ray timeNear
  in if hit
     then marchToIntersection grid point dir
     else Nothing
     
{-# INLINE marchToIntersection #-}
marchToIntersection :: VoxelGrid -> Vec3 -> Vec3 -> Maybe Intersection
marchToIntersection grid point dir@(Vec3 dx dy dz) =
  foldl' (\hit p ->
            if isJust hit then hit
            else let cell@(Vec3i cx cy cz) = cellAt p dir
                 in case (indexIntoVoxelGrid grid cell) of
                     True ->
                       let (Vec3 px py pz) = p
                           !normal =
                             if (fromIntegral cx) == px then
                               Vec3 (if dx > 0 then -1 else 1) 0 0
                             else if (fromIntegral cy) == py then
                               Vec3 0 (if dy > 0 then -1 else 1) 0
                             else
                               Vec3 0 0 (if dz > 0 then -1 else 1)
                       in Just $ Intersection p normal
                     otherwise -> Nothing
         )
         Nothing
         (marchTillEscape grid point dir)

{-# INLINE rotateVector #-}
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
