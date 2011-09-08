-- img2html.hs
--
-- Converts a PNG/JPG-image to a HTML ASCII representation.
--
-- Instructions
--  1. Install "stb-image" cabalilla "cabal install stb-image"
--  2. Run ghc --make -o img2html img2html.hs
--  3. Usage: img2html picture.(png|jpeg|bmp|gif) > picture.html
--
-- Tested
--      GHC 6.10.4, 6.12.1, 7.0.2
--
-- (c) 2009 ane, license BSD v3.

module Main where

import System (getArgs)
import Text.Printf
import Data.List
import Data.Char
import Data.Bitmap.IO
import Codec.Image.STB
import Data.ByteString (unpack)
import Data.ByteString.Internal

-- Splits a list into 3-tuples and then into w-sized chunks.
bitmapToRGBVals :: ByteString -> Int -> [[(Int, Int, Int)]]
bitmapToRGBVals res width = chunk width (chunksToTuple (chunk 3 result))
  where
    -- Word8 -> Char -> Int
    result = map (ord . w2c) . unpack $ res

-- Splits a list into n-sized chunks.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = y1 : chunk n y2
  where
    (y1, y2) = splitAt n xs

chunksToTuple :: [[Int]] -> [(Int, Int, Int)]
chunksToTuple list@(x:xs) = map toTuple (filter (not . null) list)
  where
    toTuple [r, g, b] = (r, g, b)

-- Converts a RGB-tuple into its corresponding hexadecimal color format.
rgbToHexString :: (Int, Int, Int) -> String
rgbToHexString (r, g, b) = printf "<b style=\"color: #%02x%02x%02x\">X</b>" r g b

main :: IO ()
main = do
  args <- getArgs
  if null args then putStrLn "Usage: img2hmtl pic.(png|jpg|gif|bmp) > output.html" else do
    img <- loadImage (head args)
    case img of
      Left err -> putStrLn err
      Right image ->
        do
          let (w, h) = bitmapSize image
          -- Conver the image into a bytestring
          img_data <- copyBitmapToByteString image
          let rows = bitmapToRGBVals img_data w
          putStrLn "<html><head><style type=\"text/css\">body { text-align: center; font: 5px Monospace; background: black; }</style></head><body>"
          -- Converts pixels into RGB-tuples and intercalates each row with a line break (br)
          putStrLn (intercalate "<br/>" (map (concatMap rgbToHexString) rows))
          putStrLn "</body></html>"

