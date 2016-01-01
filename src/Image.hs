module Image (
  Pixel,
  Image(Image),
  makeImage
) where

import Data.Vector.Unboxed (Vector, generate, (!))

type Pixel = Bool
data Image = Image Int Int (Vector Pixel)

instance Show Image where
  show (Image width height pixels) =
    let renderPixel True = "#"
        renderPixel False = " "
        numberOfPixels = width*height
        render index _ | index == numberOfPixels = ""
        render index counter =
          if counter == width
          then "\n" ++ (renderPixel (pixels ! index)) ++ render (index+1) 1
          else (renderPixel (pixels ! index)) ++ render (index+1) (counter+1)
    in (show width) ++ "x" ++ (show height) ++ " [\n" ++ render 0 0 ++ "]"


makeImage :: Int -> Int -> (Int -> Int -> Pixel) -> Image
makeImage width height pixelValue =
  let indexValue index = pixelValue x y
        where x = index `mod` width
              y = index `div` width
      pixels = generate (width*height) indexValue
  in Image width height pixels
