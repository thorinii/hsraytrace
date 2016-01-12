{-# LANGUAGE BangPatterns #-}
module Main where

-- import Lib
import Prelude
import qualified Prelude as P

import Data.Maybe
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Parallel (par)
import qualified System.Clock as Clock

import Data.Vector.Unboxed ((!))

import Vector
import Ray
import Shape
import Intersection
import Image

import UI.NCurses

main :: IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  runGameLoop w

runGameLoop :: Window -> Curses ()
runGameLoop w = do
  let getNow = liftIO $ Clock.getTime Clock.Monotonic
      loop start previous = do
        now <- getNow
        let uptime = now - start
            uptimeMillis = (Clock.sec uptime) * 1000 + (Clock.nsec uptime) `P.div` 1000000
            dt = now - previous
            dtMicro = (Clock.sec dt) * 1000000 + (Clock.nsec dt) `P.div` 1000
            fps = if dtMicro == 0
                  then 0
                  else 1000000 `P.div` dtMicro
        updateWindow w $ do
          moveCursor 0 0
          clearLine
          drawString (show fps)
          drawString " fps"
          moveCursor 1 1
          renderScene uptimeMillis
        render
        loop start now
  startTime <- getNow
  loop startTime startTime

-- renderScene :: a -> Update ()
renderScene uptimeMillis =
  let angle = ((fromIntegral uptimeMillis) / 10000 * 360)
  in drawImage $ renderAnImage (angle * 18) (angle) 0

drawImage :: Image -> Update ()
drawImage (Image width height pixels) =
  let numberOfPixels = width*height
      render :: Int -> Int -> Update ()
      render index _ | index == numberOfPixels = return ()
      render index counter = do
        -- moveCursor counter (index / width)
        if counter == width
        then do
          renderPixel (pixels ! index)
          drawString "\n"
          render (index+1) 1
        else do
          renderPixel (pixels ! index)
          render (index+1) (counter+1)
  in render 0 1

renderPixel :: Pixel -> Update ()
renderPixel value =
  drawString $ case value of
    _ | value < 0.1 -> " "
    _ | value < 0.2 -> "."
    _ | value < 0.3 -> ":"
    _ | value < 0.4 -> "-"
    _ | value < 0.5 -> "="
    _ | value < 0.6 -> "+"
    _ | value < 0.7 -> "*"
    _ | value < 0.8 -> "#"
    _ | value < 0.9 -> "%"
    _               -> "@"


-- render :: Camera -> Scene -> Image
-- render camera scene image = undefined

-- animateTimes times =
--   let fps = 30
--       delay = (1 * 1000 * 1000) `P.div` fps
--       animateTimes' 0 = return ()
--       animateTimes' left = do
--         let angle = ((fromIntegral left) / (fromIntegral times) * 360)
--         let image = show $ render (angle * 18) (angle*angle / 90) 0
--         let !_ = seqList image
--         putStrLn $ image
--         threadDelay delay
--         animateTimes' (left-1)
--   in animateTimes' times
--
-- renderAnImage :: Image
renderAnImage x y z =
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
            value = value' -- par value' value'
        in value
  in makeImage width height renderPixel

seqList :: [a] -> ()
seqList [] = ()
seqList (x:xs) = seqList xs
