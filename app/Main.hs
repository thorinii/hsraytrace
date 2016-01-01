module Main where

-- import Lib
import Prelude
import qualified Prelude as P

import Data.Maybe

import Vector
import qualified Vector as V

import Ray

main :: IO ()
main = do
  -- let ppmResult = render_to_pgm 500 500
  -- putStr ppmResult
  return ()


-- render :: Camera -> Scene -> Image
-- render camera scene image = undefined

data Shape = Sphere { sphere_position :: Vec3
                    , sphere_radius :: Float }
           | Plane { plane_normal :: Vec3
                   , plane_distance :: Float }
           | Box { box_min :: Vec3
                 , box_max :: Vec3 }
  deriving (Show)

cube :: Vec3 -> Float -> Shape
cube (Vec3 x y z) side = Box (Vec3 (x - half) (y - half) (z - half))
                             (Vec3 (x + half) (y + half) (z + half))
  where half = side / 2


data Intersection = Intersection { intersection_point :: !Vec3 }
  deriving (Show)


type Pixel = Bool
data Image = Image Int Int [Pixel]

instance Show Image where
  show (Image width height pixels) =
    let renderPixel True = "#"
        renderPixel False = " "
        render [] _ = ""
        render (p:px) counter =
          if counter == width
          then "\n" ++ (renderPixel p) ++ render px 1
          else (renderPixel p) ++ render px (counter+1)
    in (show width) ++ "x" ++ (show height) ++ " [\n" ++ render pixels 0 ++ "]"

render :: Image
render = renderImage (cube (Vec3 0 0 5) 1) 40 40 6

renderImage :: Shape -> Int -> Int -> Float -> Image
renderImage scene width height pixelsPerUnit =
  let renderPixel scene x y =
        let ray = makeRay width height x (height-y-1) pixelsPerUnit
            shape = scene
            cast = intersect ray shape
            didHit = isJust cast
        in didHit
  in makeImage width height (renderPixel scene)


makeImage :: Int -> Int -> (Int -> Int -> Pixel) -> Image
makeImage width height pixelValue =
  let indexValue index = pixelValue x y
        where x = index `mod` width
              y = index `P.div` width
      pixels = buildList (width*height) indexValue
  in Image width height pixels


buildList :: Int -> (Int -> a) -> [a]
buildList size f = reverse $ buildList' size 0 f []
  where buildList' 0 _ _ acc = acc
        buildList' size index f acc = buildList' (size-1) (index+1) f (f index : acc)


-- intersectionGrid origin width height castsPerUnit -> [Intersection]
intersectionGrid :: Vec3 -> Float -> Float -> Float -> [Intersection]
intersectionGrid origin width height castsPerUnit =
  let rays = rayGrid origin width height castsPerUnit
  in intersection rays

intersection :: [Ray] -> [Intersection]
intersection rays =
  let shape = cube (Vec3 0 0 5) 1
  in catMaybes $ map (\ray -> intersect ray shape) rays

rayGrid :: Vec3 -> Float -> Float -> Float -> [Ray]
rayGrid (Vec3 ox oy oz) width height castsPerUnit = do
  let xCasts = truncate $ width * castsPerUnit :: Int
      yCasts = truncate $ height * castsPerUnit :: Int
  xIndex <- [0..xCasts]
  yIndex <- [0..yCasts]
  let x = ox + (fromIntegral xIndex) / castsPerUnit - width / 2
      y = oy + (fromIntegral yIndex) / castsPerUnit - height / 2
      ray = Ray (Vec3 x y oz) (Vec3 0 0 1)
  return ray


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
