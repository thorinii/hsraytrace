module Main where

-- import Lib
import Prelude
import qualified Prelude as P

import Vector
import qualified Vector as V

main :: IO ()
main = do
  -- let ppmResult = render_to_pgm 500 500
  -- putStr ppmResult
  return ()


-- render :: Camera -> Scene -> Image
-- render camera scene image = undefined

data Ray = Ray { ray_base :: !Vec3
               , ray_direction :: !Vec3 }
  deriving (Show)

ray_pointAtTime :: Ray -> Float -> Vec3
ray_pointAtTime (Ray base dir) time = base `V.add` (dir `scalarmult` time)

data Shape = Sphere { sphere_position :: Vec3
                    , sphere_radius :: Float }
           | Plane { plane_normal :: Vec3
                   , plane_distance :: Float }
           | Box { box_min :: Vec3
                 , box_max :: Vec3 }
  deriving (Show)

cube :: Vec3 -> Float -> Shape
cube (Vec3 x y z) side = Box (Vec3 (x - half) (y - half) (z - half)) (Vec3 (x + half) (y + half) (z + half))
  where half = side / 2


-- These intersection equations are taken from www.education.siggraph.org/materials/HyperGraph

data Intersection = Intersection { intersection_normal :: !Vec3
                                 , intersection_point :: !Vec3 }
  deriving (Show)

-- When we calculate reflected rays, they start on the surface of the shape.  Unfortunately,
-- our limited numerical precision can make them be under the surface and so the reflected
-- ray immediately (at t very close to 0) hits the surface of the shape which it's meant to
-- be going away from.  So, we filter out any intersections which occur at t < epsilon.
epsilon :: Float
epsilon = 0.001


intersect :: Ray -> Shape -> Maybe Intersection
--
-- intersect ray@(Ray base dir) (Sphere center rad materialfn) =
--     let a = squared_mag dir
--         b = 2 * ( dir `dot` (base `sub` center))
--         c = (squared_mag (base `sub` center)) - rad^2
--         times = filter (> epsilon) (roots a b c)
--         normal_at_time t = normalize ((position_at_time ray t) `sub` center)
--         intersection_at_time t = (normal_at_time t, position_at_time ray t, ray, materialfn (position_at_time ray t))
--     in map (\t -> (t,intersection_at_time t)) times
--
-- intersect ray@(Ray base dir) (Plane normal d materialfn) =
--     let vd = (normalize normal) `dot` dir
--         v0 = negate (((normalize normal) `dot` base) + d)
--     in if (vd == 0) then []
--        else let t = v0 / vd
--                 hitpoint = position_at_time ray t
--             in if t > epsilon then [ (t, (if (vd > 0) then (neg normal) else normal, hitpoint, ray, materialfn hitpoint)) ]
--                               else []

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
     then Just $ Intersection (Vec3 0 0 0) point
     else Nothing
