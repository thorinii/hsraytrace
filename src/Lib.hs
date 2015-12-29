module Lib (
  render_to_pgm
) where

import Vector

-------------------------------------------------
-- 3d Ray: Used as a semi-infinite 3d line.

type Point3 = Vec3
type Direction3 = Vec3
type Time = Float
type Ray = (Point3, Direction3) -- base and direction

position_at_time :: Ray -> Time -> Point3
position_at_time (base, dir) t = base `add` (scalarmult dir t)

-------------------------------------------------
-- Generic maths functions

-- Calculate the roots of the equation a * x^2 + b * x + c = 0
roots :: Float -> Float -> Float -> [Float]
roots a b c = let discriminant = b*b - 4*a*c
        in if (discriminant < 0.0) then []
           else [ 0.5 * (-b + sqrt discriminant), 0.5 * (-b - sqrt discriminant) ]

-- Arg, I can't find haskells xor function!
xor :: Bool -> Bool -> Bool
xor True b  = not b
xor False b = b

---------------------------------------------------------------------------------------
-- Color model:  We store red, green and blue values as Floats between 0.0 and 1.0
--               However, the bounds are not enforced in any way, except by calling clamp

type Color = Vec3

-- Predefined colors for convenience
red, green, blue, white, black, midgrey, nearlywhite :: Color
red     = Vec3 1.0 0.0 0.0
green   = Vec3 0.0 1.0 0.0
blue    = Vec3 0.0 0.0 1.0
white   = Vec3 1.0 1.0 1.0
black   = Vec3 0.0 0.0 0.0
midgrey = Vec3 0.5 0.5 0.5
nearlywhite = Vec3 0.8 0.8 0.8

scale_col :: Color -> Float -> Color
scale_col c k = scalarmult c k

add_col :: Color -> Color -> Color
add_col a b = add a b

clamp :: Color -> Color
clamp (Vec3 r g b) = Vec3 (clampfloat r) (clampfloat g) (clampfloat b)
                where clampfloat f = (max 0.0 (min 1.0 f))

combine_col :: Color -> Color -> Color
combine_col (Vec3 r1 g1 b1) (Vec3 r2 g2 b2) = Vec3 (r1*r2) (g1*g2) (b1*b2)

---------------------------------------------------------------------------------------
-- Procedural textures:  Various predefined 3d texture functions

flatred, shinyred, semishinygreen, shinywhite :: Point3 -> Material
flatred _ = (red, 0.0, 1.0)
shinyred _ = (red, 0.3, 0.9)
semishinygreen _ = (green, 0.5, 0.7)
shinywhite _ = (white, 0.3, 0.9)

-- alternate 20x20x20 black and white cubes
checked_matt :: Point3 -> Material
checked_matt (Vec3 x y z) =
         let xeven = even (truncate (x / 20.0))
             yeven = even (truncate (y / 20.0))
             zeven = even (truncate (z / 20.0))
          in if (xeven `xor` yeven `xor` zeven) then (white, 0.0, 1.0) else (black, 0.0, 1.0)


---------------------------------------------------------------------------------------
-- Materials:  Each point on the surface of a shape has its own color, reflectivity (kr)
--             and diffuseness (kd)

type Reflectivity = Float
type Diffuseness = Float
type Material = (Color, Reflectivity, Diffuseness)


---------------------------------------------------------------------------------------
-- Shapes:  A shape is something which we check for intersection with a ray, and get
--          information about the intersection point (material, normal and intersection position)
--          Currently we have a plane and a sphere

type Normal = Vec3
type Radius = Float

data Shape = Sphere Point3 Radius (Point3 -> Material)
           | Plane Normal Float (Point3 -> Material)

-- Plane is defined by a normal (its a 2 sided plane though) and a distance.
-- The plane coincident with y=5 and normal (0,0,1) has distance -5.

---------------------------------------------------------------------------------------
-- Intersection calculations

-- An intersection is represented by the normal at the intersection point, the point of intersection,
-- the direction of the viewing ray coming in to the intersection and the material at the intersection
-- point.

-- These intersection equations are taken from www.education.siggraph.org/materials/HyperGraph

type Intersection = (Normal, Point3, Ray, Material)

-- When we calculate reflected rays, they start on the surface of the shape.  Unfortunately,
-- our limited numerical precision can make them be under the surface and so the reflected
-- ray immediately (at t very close to 0) hits the surface of the shape which it's meant to
-- be going away from.  So, we filter out any intersections which occur at t < epsilon.
epsilon :: Float
epsilon = 0.001


intersect :: Ray -> Shape -> [(Time, Intersection)]
intersect ray@(base, dir) (Sphere center rad materialfn) =
    let a = squared_mag dir
        b = 2 * ( dir `dot` (base `sub` center))
        c = (squared_mag (base `sub` center)) - rad^2
        times = filter (> epsilon) (roots a b c)
        normal_at_time t = normalize ((position_at_time ray t) `sub` center)
        intersection_at_time t = (normal_at_time t, position_at_time ray t, ray, materialfn (position_at_time ray t))
    in map (\t -> (t,intersection_at_time t)) times

intersect ray@(base, dir) (Plane normal d materialfn) =
    let vd = (normalize normal) `dot` dir
        v0 = negate (((normalize normal) `dot` base) + d)
    in if (vd == 0) then []
       else let t = v0 / vd
                hitpoint = position_at_time ray t
            in if t > epsilon then [ (t, (if (vd > 0) then (neg normal) else normal, hitpoint, ray, materialfn hitpoint)) ]
                              else []

-- Extract the closest intersection (lowest time) from a list
closest :: [ (Time,Intersection) ] -> Intersection
closest xs = let select_nearest (t1,i1) (t2,i2) = if (t1<t2) then (t1,i1) else (t2,i2)
             in snd (foldl select_nearest (head xs) (tail xs))

---------------------------------------------------------------------------------------
-- Lights:  We have a non-shadowable Directional light and a shadowable spotlight
data Light = Directional Vec3 Color
           | Spotlight Point3 Color

---------------------------------------------------------------------------------------
-- Global bindings

-- If a  ray doesn't hit an object, what color should we use?
background_color :: Color
background_color = black

-- What lights are in our scene?

lights :: [Light]
lights = [ Spotlight (Vec3 100 (-30) 0) nearlywhite,
           Spotlight (Vec3 (-100) (-100) 150) nearlywhite ]

-- What is the ambient lighting in the scene
ambient_light :: Color
ambient_light = Vec3 0.1 0.1 0.1

-- What Shapes are in our scene?
shapes :: [Shape]
shapes = [ Plane (normalize (Vec3 0 (-1) 0)) 50 shinyred,
           Sphere (Vec3 50 10 100) 40 semishinygreen,
           Sphere (Vec3 (-80) 0 80) 50 checked_matt]

---------------------------------------------------------------------------------------
-- Local lighting model

-- Is the light at 'lightpos' visible from point?
point_is_lit :: Point3 -> Point3 -> Bool
point_is_lit point lightpos =
            let path = lightpos `sub` point
                time_at_light = mag path
                ray = (point, normalize path)
                hits = concat (map (intersect ray) shapes)
                times = fst (unzip hits)
            in if (null times) then True else (minimum times) > time_at_light

-- Helper to calculate the diffuse light at the surface normal, given
-- the light direction (from light source to surface)
diffuse_coeff :: Vec3 -> Vec3 -> Float
diffuse_coeff light_dir normal = max 0.0 (negate ((normalize light_dir) `dot` (normalize normal)))

local_light :: Intersection -> Light -> Color
-- Simple case of a non-shadowable directional light
local_light (normal,_,_,(materialcol,_,kd)) (Directional dir lightcol) =
   let mixed_color = combine_col materialcol lightcol
       diffuse = scale_col mixed_color ((diffuse_coeff dir normal) * kd)
   in diffuse

-- Spotlight - shadowable
local_light (normal, hitpoint,_,(materialcol,_,kd)) (Spotlight lightpos lightcol) =
   let mixed_color = combine_col materialcol lightcol
       diffuse = scale_col mixed_color (kd * (diffuse_coeff (hitpoint `sub` lightpos) normal))
   in if (point_is_lit hitpoint lightpos) then diffuse else black


---------------------------------------------------------------------------------------
-- Reflections (part of the global lighting model)

-- Ray trace the outgoing reflected ray from an intersection (depth is the level of recursion
-- which we're at in the ray tracing)
reflected_ray :: Integer -> Intersection -> Color
reflected_ray depth (normal, hitpoint, (_, in_ray_dir), (color, kr, _))
   | kr == 0.0 = black
   | otherwise =
         let k = 2 * ((normalize normal) `dot` (normalize (neg in_ray_dir)))
             out_ray_dir = (scalarmult (normalize normal) k) `sub` (neg in_ray_dir)
             reflected_col = raytrace (depth + 1) (hitpoint, out_ray_dir)
         in scalarmult reflected_col kr

---------------------------------------------------------------------------------------
-- Image output: We can write a ppm (Portable Pixmap) file by converting a list of
-- colors (length is width * height) into a big string
make_pgm :: Integer -> Integer -> [ Color ] -> String
make_pgm width height xs = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify(xs)
      where stringify [] = ""
            stringify ((Vec3 r g b):xs) = show (round (r*255)) ++ " "
                   ++ show (round (g*255)) ++ " "
                   ++ show (round (b*255)) ++ " "
                   ++ stringify xs

---------------------------------------------------------------------------------------
-- Viewing screen and camera functions:  We define the camera position and the point which
-- we're looking at.  We also define an 'up' vector and a distance to the screen.  The
-- following functions generate a list of points (in raster order - a fact which is relied
-- upon when we write the image out as a ppm) which correspond to the 3d positions of the
-- pixels on our virtual screen.

-- Camera position, distance to screen, "Looking at" position, up vector
type View = (Point3, Float, Point3, Vec3)

pixel_grid :: View -> Float -> Float -> [ Point3 ]
pixel_grid (camerapos, viewdist, lookingat, viewup) width height =
   let grid = [ (Vec3 x y 0) | y <- [0..width-1], x <- [0..height-1] ]
       centering_offset = Vec3 (-width / 2.0) (-height / 2.0) 0
       pixel_offsets = map (add centering_offset) grid
       viewdir = normalize (lookingat `sub` camerapos)
       screen_center = camerapos `add` (scalarmult viewdir viewdist)
       viewright = viewdir `cross` viewup
       transform (Vec3 x y _) = screen_center `add` (scalarmult viewright x) `add` (scalarmult (neg viewup) y)
   in map transform pixel_offsets

-- Parallel projection function which creates rays parallel to the viewing screen
parallel_projection :: View -> Point3 -> Ray
parallel_projection (camerapos,_,lookingat,_) point  = (point, normalize (lookingat `sub` camerapos))

-- Perspective projection which creates rays through (0,0,-distance) through the point
perspective_projection :: View -> Point3 -> Ray
perspective_projection (camerapos,_,_,_) point = (point, normalize (point `sub` camerapos))

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
-- Main rendering functions

-- Calculate the overall color of a ray/shape intersection, taking into account
-- local lighting (diffuse only) and global lighting (reflections only, to a depth
-- of 2 bounces)
overall_lighting :: Integer -> Intersection -> Color
overall_lighting depth hit =
               let sum_colors = foldr add_col black
                   local_lighting = ambient_light `add_col` sum_colors (map (local_light hit) lights)
                   global_lighting = if (depth < 2) then (reflected_ray depth hit) else black
               in clamp (local_lighting `add_col` global_lighting)

-- Trace a ray through the scene and work out what color it should be.
-- Takes a 'depth' argument which is 0 for top level viewing rays increases
-- by one for each level of recursive raytracing we do (as a result of reflections
-- or transmissions)
raytrace :: Integer -> Ray -> Color -- uses global 'shapes'
raytrace depth ray = let hits = concat (map (intersect ray) shapes)
                     in if (null hits) then background_color
                        else overall_lighting depth (closest hits)


---------------------------------------------------------------------------------------
-- Testing: We define a function 'test' which renders a fixed view and writes
-- out the result as c:/test.ppm
-- This writes out a pgm of our trivial rendering, given the screen width and height
render_to_pgm :: Float -> Float -> String
render_to_pgm width height =
     let view = ((Vec3 0 0 (-100)), 100, (Vec3 0 0 100), (Vec3 0 (-1) 0))
         projection = perspective_projection view
         ray_collection = map projection (pixel_grid view width height)
         color_collection = map (raytrace 0) ray_collection
     in make_pgm (round width) (round height) color_collection
