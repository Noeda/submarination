#!/usr/bin/env runhaskell

import Control.Monad
import Data.List
import Data.Maybe ( fromMaybe )
import Data.Monoid
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Console.GetOpt
import System.Process

data Flags = Profile deriving Eq
flags = [Option "" ["profile-game"] (NoArg $ Right Profile) "Compile with profiling."]

findLocalRoot yaml = liftIO $ do
  rlines <- readProcess "stack" ["--stack-yaml", yaml, "path"] ""
  case find ("local-install-root: " `isInfixOf`) (lines rlines) of
    Nothing -> fail "stack path failed"
    Just line -> return $ drop 20 line

buildableLevels =
  ["src/Submarination/Biome/IntertidalZoneGen.hs"]

hsDependencies = do
  hs <- getDirectoryFiles "" ["src//*.hs"]
  hs_exe <- getDirectoryFiles "" ["exe//*.hs"]
  need $ hs <> hs_exe <> buildableLevels

main :: IO ()
main = shakeArgsWith shakeOptions{shakeFiles="_build", shakeThreads=2} flags $ \flags targets -> return $ Just $ do
  if null targets
    then want ["_build/submarination" <.> exe
              ,"_build/submarination.min.js"]
    else want targets

  phony "clean" $ do
    () <- cmd "stack clean --stack-yaml stack.yaml"
    () <- cmd "stack clean --stack-yaml stack-browser.yaml"
    removeFilesAfter "." buildableLevels
    removeFilesAfter "_build" ["//*"]


  phony "deploy" $ do
    bucket <- fromMaybe (error "SUBMARINATION_BUCKET must be defined (env)") <$> getEnv "SUBMARINATION_BUCKET"
    prefix <- fromMaybe (error "SUBMARINATION_PREFIX must be defined (env)") <$> getEnv "SUBMARINATION_PREFIX"
    need ["web/index.html", "web/DejaVuSansMono.ttf", "web/style.css", "_build/submarination.min.js.gz"]

    let copy file tgt = cmd "aws" "s3" "cp" file ("s3://" <> bucket </> prefix </> tgt) :: Action ()

    copy "web/index.html" "index.html"
    copy "web/DejaVuSansMono.ttf" "DejaVuSansMono.ttf"
    copy "web/style.css" "style.css"

    cmd "s3cmd" "put" "--add-header" "Content-Type:application/json" "--add-header" "Content-Encoding:gzip" "_build/submarination.min.js.gz" ("s3://" <> bucket </> prefix </> "bundle.js")

  "_build/*.gz" %> \out -> do
    need [dropExtension out]
    cmd "gzip" "-k" (dropExtension out)

  "src/Submarination/Biome/IntertidalZoneGen.hs" %> \out -> do
    need ["src/Submarination/Biome/IntertidalZone.hs"]
    let mod_name = fmap (\c -> if c == '/' then '.' else c) $ dropExtension $ dropDirectory1 out
        c = "echo 'import Submarination.Biome.Gen\\ntoHaskellSourceFile \"" <> out <> "\" \"" <> mod_name <> "\" intertidalZoneGen\n' | stack ghci --flag submarination:-use-baked-levels --stack-yaml stack.yaml"
    cmd Shell c

  "_build/submarination" <.> exe %> \out -> do
    hsDependencies

    let prof = if Profile `elem` flags then "--library-profiling --executable-profiling" else ""
    cmd "stack build --flag submarination:use-baked-levels --stack-yaml stack.yaml --local-bin-path" (takeDirectory1 out) "--copy-bins" prof

  "_build/submarination.min.js" %> \out -> do
    let inp = (dropExtension $ dropExtension out) <.> "js"
    need [inp]
    cmd "closure-compiler" inp "--js_output_file" out

  "_build/submarination.js" %> \out -> do
    root <- findLocalRoot "stack-browser.yaml"
    hsDependencies
    () <- cmd "stack build --flag submarination:use-baked-levels --stack-yaml stack-browser.yaml --local-bin-path" (takeDirectory1 out)
    () <- cmd "cp" (root </> "bin/submarination.jsexe/all.js") out
    return ()


