{-# LANGUAGE QuasiQuotes #-}

module Sequential.Utils
  ( -- * Printing with timestamps
    putTimeInfoStr,
    putTimeSuccessStr,
    putTimeErrStr,

    -- ** ANSI Colors
    colorBlue,
    colorGreen,
    colorRed,
    colorMagenta,

    -- * Time
    getLocalTime,
    formatLocalTime,

    -- * Paths
    outputDir,
    cachePath,
    reportPath,
    logsDir,

    -- ** Conversions
    textToOsPathThrowM,
    osPathToStringThrowM,
    osPathToStringShow,

    -- * Files
    readBinaryFile,
    writeBinaryFile,
    readFileUtf8,
    withBinaryFileWriteMode,
    writeJson,
    encodePretty,

    -- * Misc
    putStrErrLn,
    removeDirectoryRecursiveIfExists,
  )
where

import Control.Exception (throwIO)
import Control.Monad (when, (>=>))
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty qualified as AsnPretty
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (LocalTime)
import Data.Time.LocalTime qualified as Local
import System.Console.Pretty (Color (Blue, Green, Magenta, Red))
import System.Console.Pretty qualified as Pretty
import System.Directory.OsPath qualified as Dir
import System.File.OsPath qualified as FileIO
import System.IO (Handle, IOMode (WriteMode), hPutStrLn, stderr)
import System.OsPath (OsPath, osp, (</>))
import System.OsPath qualified as OsPath

-- | 'putStrLn' with a timestamp and info prefix.
putTimeInfoStr :: Bool -> String -> IO ()
putTimeInfoStr b s = do
  timeStr <- getLocalTimeString
  putStrLn $ colorBlue b $ "[" ++ timeStr ++ "][Info]    " ++ s

-- | 'putStrLn' with a timestamp and info prefix.
putTimeSuccessStr :: Bool -> String -> IO ()
putTimeSuccessStr b s = do
  timeStr <- getLocalTimeString
  putStrLn $ colorGreen b $ "[" ++ timeStr ++ "][Success] " ++ s

-- | 'putStrErrLn' with a timestamp and error prefix.
putTimeErrStr :: Bool -> String -> IO ()
putTimeErrStr b s = do
  timeStr <- getLocalTimeString
  putStrErrLn $ colorRed b $ "[" ++ timeStr ++ "][Error]   " ++ s

getLocalTime :: IO LocalTime
getLocalTime = Local.zonedTimeToLocalTime <$> Local.getZonedTime

getLocalTimeString :: IO String
getLocalTimeString = formatLocalTime <$> getLocalTime

formatLocalTime :: LocalTime -> String
formatLocalTime = Format.formatTime Format.defaultTimeLocale fmt
  where
    fmt = "%0Y-%m-%d %H:%M:%S"

-- | putStrLn for stderr.
putStrErrLn :: String -> IO ()
putStrErrLn = hPutStrLn stderr

-- | Output directory.
outputDir :: OsPath
outputDir = [osp|output|]

-- | Cache path.
cachePath :: OsPath
cachePath = outputDir </> [osp|cache.json|]

-- | Report path.
reportPath :: OsPath
reportPath = outputDir </> [osp|report.json|]

logsDir :: OsPath
logsDir = outputDir </> [osp|logs|]

-- | Encode Text to OsPath
textToOsPathThrowM :: Text -> IO OsPath
textToOsPathThrowM = OsPath.encodeUtf . T.unpack

-- | Decode OsPath to String.
osPathToStringThrowM :: OsPath -> IO String
osPathToStringThrowM = OsPath.decodeUtf

-- | Decode OsPath to String.
osPathToStringShow :: OsPath -> String
osPathToStringShow p = case OsPath.decodeUtf p of
  Nothing -> show p
  Just s -> s

-- | Reads a UTF-8 file into 'Text', throw an exception if decoding fails.
readFileUtf8 :: OsPath -> IO Text
readFileUtf8 = readBinaryFile >=> either throwIO pure . TEnc.decodeUtf8'

-- | Reads a file.
readBinaryFile :: OsPath -> IO ByteString
readBinaryFile = FileIO.readFile'

-- | Reads a file.
writeBinaryFile :: OsPath -> ByteString -> IO ()
writeBinaryFile = FileIO.writeFile'

-- | With a file in write mode.
withBinaryFileWriteMode :: OsPath -> (Handle -> IO r) -> IO r
withBinaryFileWriteMode p = withBinaryFile p WriteMode

-- | With a file.
withBinaryFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = FileIO.withBinaryFile

-- | Removes the directory if it exists.
removeDirectoryRecursiveIfExists :: OsPath -> IO ()
removeDirectoryRecursiveIfExists p =
  Dir.doesDirectoryExist p >>= (`when` Dir.removeDirectoryRecursive p)

-- | Write to the file.
writeJson :: (ToJSON a) => OsPath -> a -> IO ()
writeJson p = FileIO.writeFile' p . encodePretty

-- | Encodes JSON
encodePretty :: (ToJSON a) => a -> ByteString
encodePretty =
  BSL.toStrict
    . AsnPretty.encodePretty'
      ( AsnPretty.defConfig
          { AsnPretty.confCompare = orderJsonKeys
          }
      )
  where
    orderJsonKeys :: Text -> Text -> Ordering
    orderJsonKeys l r = case liftA2 (,) (topKeyInt l) (topKeyInt r) of
      Just (lInt, rInt) -> compare lInt rInt
      Nothing -> case liftA2 (,) (resultsKeyInt l) (resultsKeyInt r) of
        Just (lInt, rInt) -> compare lInt rInt
        Nothing -> EQ

    topKeyInt :: Text -> Maybe Int
    topKeyInt "startTime" = Just 0
    topKeyInt "endTime" = Just 1
    topKeyInt "stats" = Just 2
    topKeyInt "results" = Just 3
    topKeyInt _ = Nothing

    resultsKeyInt :: Text -> Maybe Int
    resultsKeyInt "failures" = Just 0
    resultsKeyInt "untested" = Just 1
    resultsKeyInt "successes" = Just 2
    resultsKeyInt _ = Nothing

colorBlue :: Bool -> String -> String
colorBlue b = colorIf b Blue

colorMagenta :: Bool -> String -> String
colorMagenta b = colorIf b Magenta

colorGreen :: Bool -> String -> String
colorGreen b = colorIf b Green

colorRed :: Bool -> String -> String
colorRed b = colorIf b Red

colorIf :: Bool -> Color -> String -> String
colorIf True = Pretty.color
colorIf False = const id
