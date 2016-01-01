module Image (
  Pixel,
  Image(Image),
  makeImage
) where

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



makeImage :: Int -> Int -> (Int -> Int -> Pixel) -> Image
makeImage width height pixelValue =
  let indexValue index = pixelValue x y
        where x = index `mod` width
              y = index `div` width
      pixels = buildList (width*height) indexValue
  in Image width height pixels


buildList :: Int -> (Int -> a) -> [a]
buildList size f = reverse $ buildList' size 0 f []
  where buildList' 0 _ _ acc = acc
        buildList' size index f acc = buildList' (size-1) (index+1) f (f index : acc)
