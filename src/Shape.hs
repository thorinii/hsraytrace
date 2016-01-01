module Shape (
  Shape(Sphere, Plane, Box,
        Translate, Rotate),
  cube,
  translate, rotate
) where

import Prelude as P
import Vector as V
import Ray


data Shape = Sphere { sphere_position :: Vec3
                    , sphere_radius :: Float }
           | Plane { plane_normal :: Vec3
                   , plane_distance :: Float }
           | Box { box_min :: Vec3
                 , box_max :: Vec3 }
           | Translate { translate_inner :: Shape
                       , translate_translation :: Vec3 }
           | Rotate { rotate_inner :: Shape
                    , rotate_x :: Float
                    , rotate_y :: Float
                    , rotate_z :: Float }
  deriving (Show)


cube :: Vec3 -> Float -> Shape
cube (Vec3 x y z) side = Box (Vec3 (x - half) (y - half) (z - half))
                             (Vec3 (x + half) (y + half) (z + half))
  where half = side / 2


translate :: Vec3 -> Shape -> Shape
translate translation shape = Translate shape translation

rotate :: Float -> Float -> Float -> Shape -> Shape
rotate x y z shape = Rotate shape (d2r x) (d2r y) (d2r z)
  where d2r degrees = degrees * pi / 180
