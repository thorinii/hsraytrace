{-# LANGUAGE BangPatterns #-}
module Vector (
  Vec3(Vec3),
  x, y, z,
  add, sub, div,
  min, max,
  squared_mag, mag,
  scalarmult,
  dot, cross,
  normalize,
  neg
) where

import Prelude hiding (div, min, max)
import qualified Prelude as P

-------------------------------------------------
-- 3d vector representation and manipulation

-- NB: We have a right handed coordinate system.  If x increases to your right, and Y increases downwards then
-- you are looking in the direction of increasing Z.

data Vec3 = Vec3 !Float !Float !Float

instance Show Vec3 where
  show (Vec3 x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

x :: Vec3 -> Float
x (Vec3 value _ _) = value

y :: Vec3 -> Float
y (Vec3 _ value _) = value

z :: Vec3 -> Float
z (Vec3 _ _ value) = value

add :: Vec3 -> Vec3 -> Vec3
add (Vec3 x y z) (Vec3 a b c) = Vec3 (a+x) (b+y) (c+z)

sub :: Vec3 -> Vec3 -> Vec3
sub (Vec3 a b c) (Vec3 x y z) = Vec3 (a-x) (b-y) (c-z)

div :: Vec3 -> Vec3 -> Vec3
div (Vec3 a b c) (Vec3 x y z) = Vec3 (a/x) (b/y) (c/z)

min :: Vec3 -> Vec3 -> Vec3
min (Vec3 a b c) (Vec3 x y z) = Vec3 (P.min a x) (P.min b y) (P.min c z)

max :: Vec3 -> Vec3 -> Vec3
max (Vec3 a b c) (Vec3 x y z) = Vec3 (P.max a x) (P.max b y) (P.max c z)

squared_mag :: Vec3 -> Float
squared_mag (Vec3 x y z) = x*x + y*y + z*z

mag :: Vec3 -> Float
mag v = sqrt (squared_mag v)

scalarmult :: Vec3 -> Float -> Vec3
scalarmult (Vec3 x y z) c = Vec3 (x*c) (y*c) (z*c)

dot :: Vec3 -> Vec3 -> Float
dot (Vec3 x y z) (Vec3 a b c) = x*a + b*y + c*z

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a b c) (Vec3 x y z) = Vec3 (b*z + c*y) (-(a*z + c*x)) (a*y + b*x)

normalize :: Vec3 -> Vec3
normalize v
  | (mag v) /= 0 = scalarmult v (1 / mag v)
  | otherwise    = Vec3 0 0 0

neg :: Vec3 -> Vec3
neg (Vec3 x y z) = Vec3 (-x) (-y) (-z)
