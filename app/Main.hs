{-# LANGUAGE BangPatterns #-}
module Main where

-- import Lib
import Prelude
import qualified Prelude as P

import Data.List (foldl')
import Data.Maybe
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Parallel (par)
import qualified System.Clock as Clock

import Data.Vector.Unboxed ((!))
import GHC.Int (Int64)

import Vector
import Ray
import Shape
import Intersection
import Image
import Voxel

import UI.NCurses

main :: IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  runGameLoop w

runGameLoop :: Window -> Curses ()
runGameLoop w = do
  let loop start previous = do
        now <- getNow
        let uptime = now - start
            uptimeMillis = microsFromTimeStamp uptime `P.div` 1000
            dt = now - previous
            dtMicro = microsFromTimeStamp dt
            fps = if dtMicro == 0
                  then 0
                  else 1000000 `P.div` dtMicro
        updateWindow w $ drawGame uptimeMillis fps
        render
        loop start now
  startTime <- getNow
  loop startTime startTime

getNow :: Curses Clock.TimeSpec
getNow = liftIO $ Clock.getTime Clock.Monotonic

microsFromTimeStamp :: Clock.TimeSpec -> Int64
microsFromTimeStamp timestamp =
  (Clock.sec timestamp) * 1000000 + (Clock.nsec timestamp) `P.div` 1000

drawGame :: Int64 -> Int64 -> Update ()
drawGame uptimeMillis fps = do
  moveCursor 0 0
  clearLine
  drawString (show fps)
  drawString " fps"
  moveCursor 1 1
  drawScene uptimeMillis

drawScene :: Int64 -> Update ()
drawScene uptimeMillis =
  let angle = ((fromIntegral uptimeMillis) / 10000 * 360)
  in drawImage $ generateAndRenderScene (angle * 18) (angle) 0

drawImage :: Image -> Update ()
drawImage (Image width height pixels) =
  let numberOfPixels = width*height
      render :: Int -> Int -> Update ()
      render index _ | index == numberOfPixels = return ()
      render index counter = do
        -- moveCursor counter (index / width)
        drawPixel (pixels ! index)
        if counter == width
        then do
          drawString "\n"
          render (index+1) 1
        else render (index+1) (counter+1)
  in render 0 1

drawPixel :: Pixel -> Update ()
drawPixel value =
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


generateAndRenderScene :: Float -> Float -> Float -> Image
generateAndRenderScene x y z =
  let box1 = translate (Vec3 2 0 0) $ cube (sin (z/ 10) * 0.5 + 1)
      box2 = translate (Vec3 (-2) 0 0) $ cube 1
      boxes = rotate x 0 0 $ group box1 box2
      !scene = translate (Vec3 0 0 5) $ rotate 0 y 0 $ rotate 0 0 z $ voxelBox voxels 1
      !fovX = 60 / 180 * pi
  in renderSceneToImage scene fovX 80 40

renderSceneToImage :: Shape -> Float -> Int -> Int -> Image
renderSceneToImage scene fovX width height =
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
            !value = value' -- par value' value'
        in value
  in makeImage width height renderPixel


voxels = makeVoxelGrid 10 10 10 (\(Vec3i x y z) -> if (x + y < 7 && x + y > 4) then True else False)
