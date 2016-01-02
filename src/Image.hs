module Image (
  Pixel,
  Image(Image),
  makeImage
) where

import Data.Vector.Unboxed (Vector, generate, (!))

type Pixel = Float
data Image = Image Int Int (Vector Pixel)

instance Show Image where
  show (Image width height pixels) =
    let numberOfPixels = width*height
        render index _ | index == numberOfPixels = ""
        render index counter =
          if counter == width
          then "\n" ++ (renderPixel (pixels ! index)) ++ render (index+1) 1
          else (renderPixel (pixels ! index)) ++ render (index+1) (counter+1)
    in (show width) ++ "x" ++ (show height) ++ " [\n" ++ render 0 0 ++ "]"

renderPixel :: Pixel -> String
renderPixel value =
  case value of
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

makeImage :: Int -> Int -> (Int -> Int -> Pixel) -> Image
makeImage width height pixelValue =
  let indexValue index = pixelValue x y
        where x = index `mod` width
              y = index `div` width
      pixels = generate (width*height) indexValue
  in Image width height pixels
