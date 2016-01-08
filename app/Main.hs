{-# LANGUAGE BangPatterns #-}
module Main where

-- import Lib
import Prelude
import qualified Prelude as P

import Data.Maybe
import Control.Concurrent (threadDelay)
import Control.Parallel (par)

import Vector
import Ray
import Shape
import Intersection
import Image

main :: IO ()
main = do
  animateTimes 300


-- render :: Camera -> Scene -> Image
-- render camera scene image = undefined

animateTimes times =
  let fps = 30
      delay = (1 * 1000 * 1000) `P.div` fps
      animateTimes' 0 = return ()
      animateTimes' left = do
        let angle = ((fromIntegral left) / (fromIntegral times) * 360)
        let image = show $ render (angle * 18) (angle*angle / 90) 0
        let !_ = seqList image
        putStrLn $ image
        threadDelay delay
        animateTimes' (left-1)
  in animateTimes' times

-- render :: Image
render x y z =
  let box1 = translate (Vec3 2 0 0) $ cube (sin (z/ 10) * 0.5 + 1)
      box2 = translate (Vec3 (-2) 0 0) $ cube 1
      boxes = rotate x 0 0 $ group box1 box2
      !scene = translate (Vec3 0 0 5) $ rotate 0 y 0 $ rotate 0 0 z $ boxes
      !fovX = 60 / 180 * pi
  in renderImage scene fovX 80 40

renderImage :: Shape -> Float -> Int -> Int -> Image
renderImage scene fovX width height =
  let castValue x y =
        let !ray = makeRay fovX width height x (fromIntegral height - y - 1)
            !cast = intersect ray scene
            !didHit = isJust cast
        in if didHit then 1.0 else 0.0
      castValueMany x y =
        let fx = fromIntegral x
            fy = fromIntegral y
            j = 0.33 -- jitter
            !value =
              castValue (fx-j) (fy+j) +
              castValue (fx+j) (fy+j) +
              castValue (fx) (fy) +
              castValue (fx-j) (fy-j) +
              castValue (fx+j) (fy-j)
        in value / 4
      renderPixel x y =
        let value' = castValueMany x (y*2)
            value = par value' value'
        in value
  in makeImage width height renderPixel

seqList :: [a] -> ()
seqList [] = ()
seqList (x:xs) = seqList xs
