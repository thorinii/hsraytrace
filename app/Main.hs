module Main where

-- import Lib
import Prelude
import qualified Prelude as P

import Data.Maybe

import Vector
import qualified Vector as V

import Ray
import Shape
import Intersection
import Image

main :: IO ()
main = do
  -- let ppmResult = render_to_pgm 500 500
  -- putStr ppmResult
  return ()


-- render :: Camera -> Scene -> Image
-- render camera scene image = undefined


-- render :: Image
render angle =
  let scene = rotate 0 0 angle $ translate (Vec3 2 0 0) $ cube (Vec3 0 0 5) 1
  in renderImage scene 40 40 6

renderImage :: Shape -> Int -> Int -> Float -> Image
renderImage scene width height pixelsPerUnit =
  let renderPixel scene x y =
        let ray = makeRay width height x (height-y-1) pixelsPerUnit
            shape = scene
            cast = intersect ray shape
            didHit = isJust cast
        in didHit
  in makeImage width height (renderPixel scene)
