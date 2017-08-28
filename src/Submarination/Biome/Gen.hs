module Submarination.Biome.Gen
  ( toHaskellSourceFile )
  where

import Data.Binary
import Data.Binary.Put
import qualified Data.Text as T
import qualified Prelude as P
import Protolude hiding ( put )
import System.IO

import Submarination.Level

toHaskellSourceFile :: MonadIO m => FilePath -> Text -> Level -> m ()
toHaskellSourceFile fpath name lvl = liftIO $
  withFile fpath WriteMode $ \handle -> do
    hPutStrLn handle $ "module " <> T.unpack name <> " ( biome ) where"
    hPutStrLn handle ""
    hPutStrLn handle "import Data.Binary"
    hPutStrLn handle "import Data.Binary.Get"
    hPutStrLn handle "import Submarination.Level"
    hPutStrLn handle ""
    hPutStrLn handle "biome :: Level"
    hPutStr handle "biome = runGet get "
    hFlush handle

    let b = runPut $ put lvl
    hPrint handle b
    hPutStrLn handle "{-# NOINLINE biome #-}"


