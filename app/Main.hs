import Control.Exception
import Control.Monad (void)
import Data.Monoid
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import Options.Applicative.Builder
import Wrecker
import Wrecker.Options

parser :: Parser (PartialOptions, String)
parser = (,) <$> pPartialOptions <*> strArgument mempty

runParser' :: IO (Options, String)
runParser' = do
  let opts =
        info
          (helper <*> parser)
          (fullDesc <> progDesc "Welcome to wrecker" <>
           header "wrecker - HTTP stress tester and benchmarker")
  (partialOptions, url) <- execParser opts
  options <-
    case completeOptions partialOptions of
      Nothing -> throwIO $ userError ""
      Just x -> return x
  return (options, url)

main :: IO ()
main = do
  (options, url) <- runParser'
  man <-
    newManager
      tlsManagerSettings
      {managerConnCount = concurrency options, managerIdleConnectionCount = concurrency options}
  req <- parseRequest url
  void $ runOne options $ \env -> void $ record (recorder env) url $ httpLbs req man
