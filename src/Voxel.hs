{-# LANGUAGE BangPatterns #-}
module Voxel (
  Vec3i(Vec3i),
  Voxel,
  VoxelGrid(VoxelGrid),
  makeVoxelGrid,
  indexIntoVoxelGrid,
  marchToIntersection,
  marchTillEscape,
  march, cellAt, nextLocation
) where

import Data.Maybe (isJust)
import Data.List (foldl')
import Data.Vector.Unboxed (Vector, generate, (!))
import Vector (Vec3(Vec3))

data Vec3i = Vec3i !Int !Int !Int

instance Show Vec3i where
  show (Vec3i x y z) = "i(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"


type Voxel = Bool
data VoxelGrid = VoxelGrid !Int !Int !Int (Vector Voxel)

instance Show VoxelGrid where
  show (VoxelGrid w h d _) = "VG" ++ show w ++ "," ++ show h ++ "," ++ show d

makeVoxelGrid :: Int -> Int -> Int -> (Vec3i -> Voxel) -> VoxelGrid
makeVoxelGrid width height depth pixelValue =
  let indexValue index = pixelValue (Vec3i x y z)
        where z = index `div` (width * height)
              index2 = index - (z * width * height)
              y = index2 `div` width
              x = index2 `mod` width
      pixels = generate (width*height*depth) indexValue
  in VoxelGrid width height depth pixels

indexIntoVoxelGrid :: VoxelGrid -> Vec3i -> Voxel
indexIntoVoxelGrid (VoxelGrid width height depth values) (Vec3i x y z) =
  values ! (x + y * width + z * width * height)


marchToIntersection :: VoxelGrid -> Vec3 -> Vec3 -> Maybe Vec3
marchToIntersection grid point dir =
  foldl' (\hit p ->
            if isJust hit then hit
            else let value = indexIntoVoxelGrid grid (cellAt p dir)
                 in if value then Just p else Nothing)
          Nothing
          $ marchTillEscape grid point dir

marchTillEscape :: VoxelGrid -> Vec3 -> Vec3 -> [Vec3]
marchTillEscape image point dir =
  let testEscaped = \p -> let cell = cellAt p dir
                          in not $ escaped image cell
  in takeWhile testEscaped (marchIndefinitely point dir)

marchIndefinitely :: Vec3 -> Vec3 -> [Vec3]
marchIndefinitely point dir =
  let next = march point dir
  in take 5000 $ next : marchIndefinitely next dir

march :: Vec3 -> Vec3 -> Vec3
march !point !dir =
  let !cell = cellAt point dir
      !next = nextLocation point cell dir
  in next

cellAt :: Vec3 -> Vec3 -> Vec3i
cellAt (Vec3 !x !y !z) (Vec3 !dx !dy !dz) =
  let !onGridX = x == (fromIntegral $ round x)
      !onGridY = y == (fromIntegral $ round y)
  in if onGridX then
       Vec3i (floor (if dx < 0 then x - 1 else x)) (floor y) (floor z)
     else
       if onGridY then
         Vec3i (floor x) (floor (if dy < 0 then y - 1 else y)) (floor z)
       else
         Vec3i (floor x) (floor y) (floor (if dz < 0 then z - 1 else z))

nextLocation :: Vec3 -> Vec3i -> Vec3 -> Vec3
nextLocation (Vec3 x y z) (Vec3i cellx celly cellz) (Vec3 dx dy dz) =
  let x2y = y - (x * dy / dx) -- TODO: use t to determine best
      x2z = z - (x * dz / dx)
      xTryX = if dx >= 0 then fromIntegral cellx + 1 else fromIntegral cellx
      yTryX = xTryX * (dy / dx) + x2y
      zTryX = xTryX * (dz / dx) + x2z
      yTryY = if dy >= 0 then fromIntegral celly + 1 else fromIntegral celly
      xTryY = (yTryY - x2y) / (dy / dx)
      zTryY = xTryY * (dz / dx) + x2z
      zTryZ = if dz >= 0 then fromIntegral cellz + 1 else fromIntegral cellz
      xTryZ = (zTryZ - x2z) / (dz / dx)
      yTryZ = xTryZ * (dy / dx) + x2y
      tryX = Vec3 xTryX yTryX zTryX
      tryY = Vec3 xTryY yTryY zTryY
      tryZ = Vec3 xTryZ yTryZ zTryZ
      xOp = if dx >= 0 then (<) else (>)
      yOp = if dy >= 0 then (<) else (>)
      zOp = if dz >= 0 then (<) else (>)
      xIsSmallest = abs (xTryX - x) `xOp` abs (xTryY - x) && abs (xTryX - x) `xOp` abs (xTryZ - x)
      yIsSmallest = abs (yTryY - y) `yOp` abs (yTryX - y) && abs (yTryY - y) `yOp` abs (yTryZ - y)
      zIsSmallest = abs (zTryZ - z) `zOp` abs (zTryX - z) && abs (zTryZ - z) `zOp` abs (zTryY - z)
  in if xIsSmallest then tryX
     else if yIsSmallest then tryY
     else tryZ

escaped :: VoxelGrid -> Vec3i -> Bool
escaped (VoxelGrid w h d _) (Vec3i x y z) =
  x < 0 || y < 0 || z < 0 || x >= w || y >= h || z >= d
