module Wrecker.Main
    ( defaultMain
    ) where

import Control.Monad (void)
import Wrecker.Options (runParser)
import Wrecker.Runner (Environment, run)

{- | 'defaultMain' is typically the main entry point for 'wrecker' benchmarks.
     'defaultMain' will parse all command line arguments and then call 'run'
     with the correct 'Options'.

> import Wrecker
> import Your.Performance.Scripts (landingPage, purchase)
>
> main :: IO ()
> main = defaultMain
>  [ ("loginReshare", loginReshare)
>  , ("purchase"    , purchase    )
>  ]

To see the options defaultMain can parse call `--help`

> $ wrecker-based-app --help
>
> wrecker - HTTP stress tester and benchmarker
>
> Usage: wreck [--concurrency ARG] [--bin-count ARG] ([--run-count ARG] |
>              [--run-timed ARG]) [--timeout-time ARG] ([--non-interactive] |
>              [--interactive]) [--log-level ARG] [--log-format ARG] [--match ARG]
>              [--request-name-size ARG] [--output-path ARG] [--silent]
>              ([--relative-url-display] | [--absolute-url-display])
>              [--record-query] [--list-test-groups]
>   Welcome to wrecker
>
> Available options:
>   -h,--help                Show this help text
>   --concurrency ARG        Number of threads for concurrent requests
>   --bin-count ARG          Number of bins for latency histogram
>   --run-count ARG          number of times to repeat
>   --run-timed ARG          number of seconds to repeat
>   --timeout-time ARG       How long to wait for all requests to finish
>   --log-level ARG          Log to stderr events of criticality greater than the
>                            LOG_LEVEL
>   --log-format ARG         Log format to use
>   --match ARG              Only run tests that match the glob
>   --request-name-size ARG  Request name size for the terminal display
>   --output-path ARG        Save a JSON file of the the statistics to given path
>   --silent                 Disable all output
>   --record-query           Take in consideration the query string for the report
>   --list-test-groups       Shows the list of tests to run and exit
-}
defaultMain :: [(String, Environment -> IO ())] -> IO ()
defaultMain actions = void . flip run actions =<< runParser
