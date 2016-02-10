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
import Data.Array.Repa as R hiding (Shape)
import Data.Array.Repa.Index (ix2)
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
  let loop start previous minFps maxFps = do
        now <- getNow
        let uptime = now - start
            uptimeMillis = microsFromTimeStamp uptime `P.div` 1000
            dt = now - previous
            dtMicro = microsFromTimeStamp dt
            fps = if dtMicro == 0
                  then 0
                  else 1000000 `P.div` dtMicro
        updateWindow w $ drawGame uptimeMillis fps minFps maxFps
        render
        loop start now
             (P.min fps (if uptimeMillis < 2000 then 1000 else minFps))
             (P.max fps (if uptimeMillis < 2000 then 0 else maxFps))
  startTime <- getNow
  loop startTime startTime 0 1000

getNow :: Curses Clock.TimeSpec
getNow = liftIO $ Clock.getTime Clock.Monotonic

microsFromTimeStamp :: Clock.TimeSpec -> Int64
microsFromTimeStamp timestamp =
  (Clock.sec timestamp) * 1000000 + (Clock.nsec timestamp) `P.div` 1000

drawGame :: Int64 -> Int64 -> Int64 -> Int64 -> Update ()
drawGame uptimeMillis fps minFps maxFps = do
  moveCursor 0 0
  clearLine
  drawString (show fps)
  drawString " fps  "
  drawString (show minFps)
  drawString " min  "
  drawString (show maxFps)
  drawString " max"
  moveCursor 1 1
  drawScene uptimeMillis

drawScene :: Int64 -> Update ()
drawScene uptimeMillis =
  let angle = ((fromIntegral uptimeMillis) / 10000 * 360)
  in drawImage $ generateAndRenderScene (angle * 18) (angle) 0

drawImage :: Image R.U -> Update ()
drawImage (Image pixels) =
  let (Z :. width :. height) = R.extent pixels
      render :: Int -> Int -> Update ()
      render x y | x >= width && y >= height = return ()
      render x y | x >= width = do
        drawString "\n"
        render 0 (y+1)
      render x y =
        if y >= height then return ()
        else if x >= width then do
          drawString "\n"
          render 0 (y+1)
        else do
          drawPixel (pixels R.! (ix2 x y))
          render (x+1) y
  in render 0 0

drawPixel :: Pixel -> Update ()
drawPixel value =
  drawString $ case value of
    _ | value <   0 -> "D"
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


generateAndRenderScene :: Float -> Float -> Float -> Image R.U
generateAndRenderScene x y z =
  -- $ rotate 0 y 0 $ rotate 0 0 z
  let !scene = translate (Vec3 0 ((sin (x/500))*6) 15) $ voxelBox voxels 1
      !fovX = 60 / 180 * pi
  in evaluateImage $ renderSceneToImage scene fovX 80 40

renderSceneToImage :: Shape -> Float -> Int -> Int -> Image R.D
renderSceneToImage scene fovX width height =
  let castValue x y =
        let !ray = makeRay fovX width height x (fromIntegral height - y - 1)
            !cast = intersect ray scene
            !value = case cast of
                       Just (Intersection _ (Vec3 nx ny nz)) -> 1
                       otherwise -> 0.0
        in value
      castValueMany x y =
        let fx = fromIntegral x
            fy = fromIntegral y
            j = 0.33 -- jitter
            !value =
              castValue (fx-j) (fy+j) +
              castValue (fx+j) (fy+j) +
              castValue (fx-j) (fy-j) +
              castValue (fx+j) (fy-j) +
              castValue (fx) (fy)
        in value / 5
      renderPixel x y =
        let value' = castValueMany x ((y - height `P.div` 2)*2)
            !value = value' -- par value' value'
        in value
  in makeImage width height renderPixel


voxels = makeVoxelGrid 10 10 10 (\(Vec3i x y z) -> not ((x > 1 && x < 5 && y > 1 && y < 5) || (z > 1 && z < 5 && y > 1 && y < 5) || (z > 1 && z < 5 && x > 1 && x < 5)))
