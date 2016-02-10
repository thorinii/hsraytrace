module Image (
  Pixel,
  Image(Image),
  makeImage,
  evaluateImage
) where

-- import Data.Vector.Unboxed (Vector, generate, (!))
import Control.Monad.Identity (runIdentity)
import Data.Array.Repa as R
import Data.Array.Repa.Index (DIM2)

type Pixel = Float
newtype Image s = Image (R.Array s DIM2 Pixel)

makeImage :: Int -> Int -> (Int -> Int -> Pixel) -> Image R.D
makeImage width height pixelValue =
  let indexValue (R.Z :. x :. y) = pixelValue x y
      pixels = fromFunction (R.Z :. width :. height :: DIM2) indexValue
  in Image pixels


evaluateImage :: Image R.D -> Image R.U
evaluateImage (Image raw) = Image $ runIdentity $ computeUnboxedP raw
