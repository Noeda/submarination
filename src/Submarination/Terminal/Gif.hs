{-# LANGUAGE CPP #-}

module Submarination.Terminal.Gif
  ( toImage
  , writeGif )
  where

import Codec.Picture
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.List ( (!!) )
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Word
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Cairo
import Graphics.Rendering.Pango.Enums hiding ( Color )
import Graphics.Rendering.Pango.Layout
import Foreign.Ptr
import Foreign.Storable
import qualified Prelude as P
import Protolude
import System.IO.Unsafe

import Submarination.Terminal

#ifndef GIF
#error "Must not be compiled if GIF is not turned on."
#endif

import System.Console.ANSI ( Color(..), ColorIntensity(..) )

printLock :: MVar ()
printLock = unsafePerformIO $ newMVar ()
{-# NOINLINE printLock #-}

writeGif :: MonadIO m => [TerminalState] -> FilePath -> m ()
writeGif tss fpath = liftIO $ do
  imgs <- forConcurrently (zip [1..] tss) $ \(counter, ts) -> do
            withMVar printLock $ \_ -> putStrLn $ (show (counter :: Int) :: Text)
            img <- toImage ts
            evaluated <- evaluate img
            withMVar printLock $ \_ -> putStrLn $ (show counter <> " done" :: Text)
            return evaluated

  case writeGifImages fpath LoopingForever (fmap (\img -> (gifPalette, 10, img)) imgs) of
    Left err -> P.error err
    Right io -> io

toImage :: MonadIO m => TerminalState -> m (Image Pixel8)
toImage ts = liftIO $ do
  (pxl, width, height) <- pixels
  return $ generateImage (\x y -> toPixel $ pxl V.! (x+y*width)) width height
 where
  toPixel :: Word32 -> Pixel8
  toPixel word32 =
    let r = fromIntegral $ (word32 .&. 0xff0000) `shiftR` 16
        g = fromIntegral $ (word32 .&. 0x00ff00) `shiftR` 8
        b = fromIntegral $ (word32 .&. 0x0000ff)

     in closestPixel r g b
  {-# INLINE toPixel #-}

  pixels :: IO (V.Vector Word32, Int, Int)
  pixels = do
    (tw, th) <- withImageSurface FormatRGB24 10 10 $ \test_surf ->
      renderWith test_surf $ do
        setAntialias AntialiasNone
        layout <- createLayout ("A" :: Text)
        liftIO $ layoutSetAttributes layout [AttrSize 0 10000 50, AttrFamily 0 10000 "Monospace"]
        (_, PangoRectangle _ _ tw th) <- liftIO $ layoutGetExtents layout
        return (tw, th)

    let (width, height) = (floor (tw*fromIntegral w), floor (th*fromIntegral h))
    marr <- VM.replicate (width*height+1) (0 :: Word32)

    withImageSurface FormatRGB24 width height $ \surf -> do
      renderWith surf $ do
        for_ [0..h-1] $ \ty -> for_ [0..w-1] $ \tx -> do
          let layout_x = tw*fromIntegral tx
              layout_y = th*fromIntegral ty

              Cell fintensity fcolor bintensity bcolor ch = terminalCellAt ts tx ty

          layout <- createLayout [ch]
          liftIO $ layoutSetAttributes layout [AttrSize 0 10000 50, AttrFamily 0 10000 "Monospace"]
          save
          translate layout_x layout_y
          setSourceRGB' $ cellColorsToTriple bintensity bcolor
          rectangle 0 0 tw th
          fill
          setSourceRGB' $ cellColorsToTriple fintensity fcolor
          showLayout layout
          restore

      cairopxls <- imageSurfaceGetData surf

      B.unsafeUseAsCString cairopxls $ \cstr' -> do
        let pxlptr = castPtr cstr' :: Ptr Word32
        stride <- imageSurfaceGetStride surf
        for_ [0..width-1] $ \x ->
          for_ [0..height-1] $ \y -> do
            let pxlptr' = plusPtr pxlptr (stride * y)
            let poffset = x
            pxl <- peekElemOff pxlptr' poffset
            VM.write marr (x+y*width) (pxl .&. 0x00ffffff)

    frozen <- V.freeze marr
    return (frozen, width, height)

  (w, h) = terminalSize ts

colorList :: VB.Vector (ColorIntensity, Color)
colorList = VB.fromList [ (fintensity, fcolor) | fintensity <- [Vivid, Dull], fcolor <- [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White] ]
{-# NOINLINE colorList #-}

colorTripleList :: V.Vector (Double, Double, Double)
colorTripleList = V.fromList $ fmap (\(i, c) -> cellColorsToTriple i c) $ VB.toList colorList
{-# NOINLINE colorTripleList #-}

closestPixel :: Word8 -> Word8 -> Word8 -> Word8
closestPixel r' g' b' =
  let idx = V.minIndexBy (comparing $ \(tr, tg, tb) -> abs (r-tr) + abs (g-tg) + abs (b-tb)) colorTripleList
   in fromIntegral idx
 where
  r = fromIntegral r' / 255.0
  g = fromIntegral g' / 255.0
  b = fromIntegral b' / 255.0
{-# INLINE closestPixel #-}

gifPalette :: Palette
gifPalette =
 generateImage (\x _ -> let (fintensity, fcolor) = colorList VB.! x
                            (r, g, b) = cellColorsToTriple fintensity fcolor
                         in PixelRGB8 (floor $ r*255.0)
                                      (floor $ g*255.0)
                                      (floor $ b*255.0))
               (length colorList)
               1

cellColorsToTriple :: ColorIntensity -> Color -> (Double, Double, Double)
cellColorsToTriple fintensity fcolor = case (fcolor, fintensity) of
  (White, Vivid)     -> (1, 1, 1)
  (White, Dull)      -> (0.53, 0.53, 0.53)
  (Black, Vivid)     -> (0.26, 0.26, 0.26)
  (Black, Dull)      -> (0, 0, 0)
  (Blue, Vivid)      -> (0, 0, 1)
  (Blue, Dull)       -> (0, 0, 0.53)
  (Green, Vivid)     -> (0, 1, 0)
  (Green, Dull)      -> (0, 0.53, 0)
  (Red, Vivid)       -> (1, 0, 0)
  (Red, Dull)        -> (0.53, 0, 0)
  (Cyan, Vivid)      -> (0, 1, 1)
  (Cyan, Dull)       -> (0, 0.53, 0.53)
  (Magenta, Vivid)   -> (1, 0, 1)
  (Magenta, Dull)    -> (0.53, 0, 0.53)
  (Yellow, Vivid)    -> (1, 1, 0)
  (Yellow, Dull)     -> (0.53, 0.53, 0)

setSourceRGB' :: (Double, Double, Double) -> Render ()
setSourceRGB' (r, g, b) = setSourceRGB r g b

