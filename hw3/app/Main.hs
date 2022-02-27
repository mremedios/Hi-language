module Main where

import Control.Monad.Except (liftIO)
import qualified Data.Set as Set
import HW3.Action
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just "" -> loop
        Just input -> do
          let parsed = parse input
          case parsed of
            Left err -> outputStrLn $ show err
            Right expr -> do
                evaluated <- liftIO $ runHIO (eval expr)
                  (Set.fromList [AllowWrite, AllowTime, AllowRead])
                case evaluated of
                   Left err  -> outputStrLn $ show err
                   Right val -> outputStrLn $ show $ prettyValue val
          loop
