module Shape (
  Shape(Sphere, Plane, Box,
        VoxelBox,
        GroupPair,
        Translate, Rotate),
  cube,
  voxelBox,
  group,
  translate, rotate
) where

import Prelude as P
import Vector as V
import Ray
import Voxel


data Shape = Sphere { sphere_position :: Vec3
                    , sphere_radius :: Float }
           | Plane { plane_normal :: Vec3
                   , plane_distance :: Float }
           | Box { box_min :: Vec3
                 , box_max :: Vec3 }
           | VoxelBox { box_min :: Vec3
                      , box_max :: Vec3
                      , box_voxels :: VoxelGrid }
           | GroupPair { a :: Shape
                       , b :: Shape }
           | Translate { translate_inner :: Shape
                       , translate_translation :: Vec3 }
           | Rotate { rotate_inner :: Shape
                    , rotate_x :: Float
                    , rotate_y :: Float
                    , rotate_z :: Float }
  deriving (Show)


cube :: Float -> Shape
cube side = Box (Vec3 (-half) (-half) (-half))
                (Vec3 half half half)
  where half = side / 2


voxelBox :: VoxelGrid -> Float -> Shape
voxelBox grid@(VoxelGrid w h d _) side = VoxelBox (Vec3 0 0 0)
                                                  (Vec3 (fromIntegral w) (fromIntegral h) (fromIntegral d))
                                                  grid
  where half = side / 2

group :: Shape -> Shape -> Shape
group a b = GroupPair a b

translate :: Vec3 -> Shape -> Shape
translate translation shape = Translate shape translation

rotate :: Float -> Float -> Float -> Shape -> Shape
rotate x y z shape = Rotate shape (d2r x) (d2r y) (d2r z)
  where d2r degrees = degrees * pi / 180
