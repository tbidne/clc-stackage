module Sequential.Args
  ( Args (..),
    getArgs,
    CabalVerbosity (..),
    cabalVerbosityToArg,
    Jobs (..),
    jobsToArg,
    WriteLogs (..),
  )
where

import Data.Word (Word8)
import Options.Applicative
  ( Mod,
    Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
    (<**>),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Text.Read qualified as TR

-- | Cabal's --verbose flag
data CabalVerbosity
  = -- | V0
    CabalVerbosity0
  | -- | V1
    CabalVerbosity1
  | -- | V2
    CabalVerbosity2
  | -- | V3
    CabalVerbosity3
  deriving stock (Eq, Show)

cabalVerbosityToArg :: CabalVerbosity -> String
cabalVerbosityToArg CabalVerbosity0 = "--verbose=0"
cabalVerbosityToArg CabalVerbosity1 = "--verbose=1"
cabalVerbosityToArg CabalVerbosity2 = "--verbose=2"
cabalVerbosityToArg CabalVerbosity3 = "--verbose=3"

-- | Number of build jobs.
data Jobs
  = -- | Literal number of jobs.
    JobsN Word8
  | -- | String "$ncpus"
    JobsNCpus
  | -- | Job semaphore. Requires GHC 9.8 and Cabal 3.12
    JobsSemaphore
  deriving stock (Eq, Show)

jobsToArg :: Jobs -> String
jobsToArg (JobsN n) = "--jobs=" ++ show n
jobsToArg JobsNCpus = "--jobs=$ncpus"
jobsToArg JobsSemaphore = "--semaphore"

-- | Determines what cabal output to write
data WriteLogs
  = -- | No logs written.
    WriteLogsNone
  | -- | Current logs written (overwritten).
    WriteLogsCurrent
  | -- | Current logs written with failures saved.
    WriteLogsSaveFailures
  deriving stock (Eq, Show)

-- | CLI args.
data Args = MkArgs
  { -- | If given, batches packages together so we build more than one.
    batch :: Maybe Int,
    -- | Cabal's --verbosity flag.
    cabalVerbosity :: Maybe CabalVerbosity,
    -- | Determines if we color the logs. If 'Nothing', attempts to detect
    -- if colors are supported.
    colorLogs :: Maybe Bool,
    -- | If the first error should kill all builds.
    failFast :: Bool,
    -- | Number of build jobs.
    jobs :: Maybe Jobs,
    -- | Disables the cache, which otherwise saves the outcome of a run in a
    -- json file. The cache is used for resuming a run that was interrupted.
    noCache :: Bool,
    -- | If we do not revert the cabal file at the end (i.e. we leave the
    -- last attempted build).
    noCleanup :: Bool,
    -- | Whether to retry packages that failed.
    retryFailures :: Bool,
    -- | Determines what logs to write.
    writeLogs :: Maybe WriteLogs
  }

-- | Returns CLI args.
getArgs :: IO Args
getArgs = OA.execParser parserInfoArgs
  where
    parserInfoArgs =
      ParserInfo
        { infoParser = parseCliArgs,
          infoFullDesc = True,
          infoProgDesc = desc,
          infoHeader = Chunk headerTxt,
          infoFooter = Chunk Nothing,
          infoFailureCode = 1,
          infoPolicy = Intersperse
        }
    headerTxt = Just "Sequential: Builds clc-stackage one package at a time."
    desc =
      Chunk.vsepChunks
        [ Chunk.paragraph $
            mconcat
              [ "Sequential provides an alternative to building clc-stackage ",
                "all at once. That is, instead of 'cabal build clc-stackage', ",
                "we install and run 'sequential'. This will build each package ",
                "individually (essentially we build clc-stackage with a cabal ",
                "file modified with a single package, for every package in ",
                "the original). Though this ultimately does not save on work ",
                "since we build everything either way, it can be useful for ",
                "situations where building everything at the same time is a ",
                "problem (e.g. memory constraints, nix issues)."
              ],
          Chunk.paragraph $
            mconcat
              [ "Note: The cabal.project file should be modified to have the ",
                "original clc-stackage.cabal's build-depends pasted into the ",
                "constraints section, to ensure the right transitive ",
                "dependencies are used."
              ]
        ]

parseCliArgs :: Parser Args
parseCliArgs =
  MkArgs
    <$> parseBatch
    <*> parseCabalVerbosity
    <*> parseColorLogs
    <*> parseFailFast
    <*> parseJobs
    <*> parseNoCache
    <*> parseNoCleanup
    <*> parseRetryFailures
    <*> parseWriteLogs
      <**> OA.helper

parseBatch :: Parser (Maybe Int)
parseBatch =
  OA.optional $
    OA.option
      OA.auto
      ( mconcat
          [ OA.long "batch",
            OA.metavar "NAT",
            mkHelp $
              mconcat
                [ "By default, we only attempt to build one package at a time. ",
                  "This option batches packages together, for a potentially ",
                  "faster build. Of course, batching everything together is ",
                  "mostly useless as that is equivalent to building ",
                  "clc-stackage normally."
                ]
          ]
      )

parseCabalVerbosity :: Parser (Maybe CabalVerbosity)
parseCabalVerbosity =
  OA.optional $
    OA.option
      readCabalVerbosity
      ( mconcat
          [ OA.long "cabal-verbosity",
            OA.metavar "(0 | 1 | 2 | 3)",
            mkHelp
              "Cabal's --verbose flag. Uses cabal's default if not given (1)."
          ]
      )
  where
    readCabalVerbosity =
      OA.str >>= \case
        "0" -> pure CabalVerbosity0
        "1" -> pure CabalVerbosity1
        "2" -> pure CabalVerbosity2
        "3" -> pure CabalVerbosity3
        other ->
          fail $ "Expected one of (0 | 1 | 2 | 3), received: " ++ other

parseColorLogs :: Parser (Maybe Bool)
parseColorLogs =
  OA.optional $
    OA.option
      readStr
      ( mconcat
          [ OA.long "color-logs",
            OA.metavar "(false | true)",
            mkHelp $
              mconcat
                [ "Determines whether we color logs. No option means we ",
                  "attempt to detect if colors are supported automatically."
                ]
          ]
      )
  where
    readStr =
      OA.str >>= \case
        "true" -> pure True
        "false" -> pure False
        bad -> fail $ "Expected one of (false | true), received: " <> bad

parseFailFast :: Parser Bool
parseFailFast =
  OA.switch
    ( mconcat
        [ OA.long "fail-fast",
          mkHelp "Fails on the first build error."
        ]
    )

parseJobs :: Parser (Maybe Jobs)
parseJobs =
  OA.optional $
    OA.option
      readJobs
      ( mconcat
          [ OA.short 'j',
            OA.long "jobs",
            OA.metavar "(NAT | $ncpus | semaphore)",
            mkHelp $
              mconcat
                [ "Controls the number of build jobs i.e. the flag passed to ",
                  "cabal's --jobs option. Can be a natural number in [1, 255] ",
                  "or the literal string '$ncpus', meaning all cpus. The ",
                  "literal 'semaphore' will instead use cabal's --semaphore ",
                  "option. This requires GHC 9.8+ and Cabal 3.12+. No option ",
                  "uses cabal's default i.e. $ncpus."
                ]
          ]
      )
  where
    readJobs =
      OA.str >>= \case
        "$ncpus" -> pure JobsNCpus
        "semaphore" -> pure JobsSemaphore
        other -> case TR.readMaybe @Int other of
          Just n ->
            if n > 0 && n < 256
              then pure $ JobsN $ fromIntegral n
              else fail $ "Expected NAT in [1, 255], received: " ++ other
          Nothing ->
            fail $
              mconcat
                [ "Expected one of (NAT in [1, 255] | $ncpus | semaphore), ",
                  "received: ",
                  other
                ]

parseNoCache :: Parser Bool
parseNoCache =
  OA.switch
    ( mconcat
        [ OA.long "no-cache",
          mkHelp $
            mconcat
              [ "Disables the cache. Normally, the outcome of a run is saved ",
                "to a json cache. This is useful for resuming a run that was ",
                "interrupted (e.g. CTRL-C). The next run will fetch the ",
                "packages to build from the cache."
              ]
        ]
    )

parseNoCleanup :: Parser Bool
parseNoCleanup =
  OA.switch
    ( mconcat
        [ OA.long "no-cleanup",
          mkHelp "Will not revert the cabal file after exiting."
        ]
    )

parseRetryFailures :: Parser Bool
parseRetryFailures =
  OA.switch
    ( mconcat
        [ OA.long "retry-failures",
          mkHelp "Retries failures from the cache. Incompatible with --no-cache. "
        ]
    )

parseWriteLogs :: Parser (Maybe WriteLogs)
parseWriteLogs =
  OA.optional $
    OA.option
      readWriteLogs
      ( mconcat
          [ OA.long "write-logs",
            OA.metavar "(none | current | save-failures)",
            mkHelp $
              mconcat
                [ "Determines what cabal logs to write to the output/ ",
                  "directory. 'None' writes nothing. 'Current' writes stdout ",
                  "and stderr for the currently building project. ",
                  "'Save-failures' is the same as 'current' except the files ",
                  "are not deleted if the build failed. Defaults to ",
                  "save-failures."
                ]
          ]
      )
  where
    readWriteLogs =
      OA.str >>= \case
        "none" -> pure WriteLogsNone
        "current" -> pure WriteLogsCurrent
        "save-failures" -> pure WriteLogsSaveFailures
        other ->
          fail $
            mconcat
              [ "Expected one of (none | current | save-failures), received: ",
                other
              ]

mkHelp :: String -> Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph
