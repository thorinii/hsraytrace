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
      !scene = translate (Vec3 0 0 5) $ rotate 0 y 0 $ rotate 0 0 z $ boxes
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


tmp =
  let voxels = makeImage 10 10 (\x y -> if (x + y < 7 && x + y > 4) then 1 else 0)
      px = 2.2 :: Float
      py = 0 :: Float
      dx = 0.4472135955 :: Float
      dy = 0.894427191 :: Float

      onGridX = px == (fromIntegral $ round px)
  in if (onGridX) then
       (floor (if (dx < 0) then px - 1 else px), floor py)
     else
       (floor px, floor (if (dy < 0) then py - 1 else py))

indexInto :: Image -> (Int, Int) -> Pixel
indexInto (Image w h d) (x, y) =
  d ! (x + y * w)

escaped :: Image -> (Int, Int) -> Bool
escaped (Image w h _) (x, y) =
  x < 0 || y < 0 || x >= w || y >= h

marchToIntersection :: Image -> (Float, Float) -> (Float, Float) -> Maybe (Float, Float)
marchToIntersection image point dir =
  foldl' (\hit p ->
            if isJust hit then hit
            else let value = indexInto image (cellAt p dir)
                 in if value > 0.0 then Just p else Nothing)
          Nothing
          $ marchTillEscape image point dir
  -- P.takeWhile (\v -> v == 0.0) $ P.map (\p -> indexInto voxels (cellAt p (0.5,0.8))) $ marchTillEscape voxels (2.2, 0.0) (0.5, 0.8)

marchTillEscape :: Image -> (Float, Float) -> (Float, Float) -> [(Float, Float)]
marchTillEscape image point dir =
  let testEscaped = \p -> let cell = cellAt p dir
                          in not $ escaped image cell
  in P.takeWhile testEscaped (marchIndefinitely point dir)

marchIndefinitely :: (Float, Float) -> (Float, Float) -> [(Float, Float)]
marchIndefinitely point dir =
  let next = march point dir
  in next : marchIndefinitely next dir

march :: (Float, Float) -> (Float, Float) -> (Float, Float)
march !point !dir =
  let !cell = cellAt point dir
      !next = nextLocation point cell dir
  in next

cellAt :: (Float, Float) -> (Float, Float) -> (Int, Int)
cellAt (!x, !y) (!dx, !dy) =
  let !onGridX = x == (fromIntegral $ round x)
  in if onGridX then
       (floor (if dx < 0 then x - 1 else x), floor y)
     else
       (floor x, floor (if dy < 0 then y - 1 else y))

nextLocation :: (Float, Float) -> (Int, Int) -> (Float, Float) -> (Float, Float)
nextLocation (x, y) (cellx, celly) (dx, dy) =
  let b = y - (x * dy / dx)
      xTry1 = (yTry1 - b) / (dy / dx)
      yTry1 = fromIntegral celly + 1
      xTry2 = fromIntegral cellx + 1
      yTry2 = xTry2 * (dy / dx) + b
  in if dy >= 0 then
       if yTry1 < yTry2 then (xTry1, yTry1) else (xTry2, yTry2)
     else
       if yTry1 < yTry2 then (xTry2, yTry2) else (xTry1, yTry1)
