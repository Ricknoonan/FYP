import Control.Monad.Except

import System.IO.Unsafe (unsafePerformIO)
import System.Console.Haskeline


type ErrorWithIO = ExceptT String IO


foo :: String -> ExceptT String IO
foo "Yes" = do liftIO $ putStrLn "Paul!"
foo _ = throwError "ERROR!"


runRepl :: IO ()
runRepl = runInputT defaultSettings $ loop

loop :: InputT IO ()
loop = do
    line <- getInputLine "> "
    case line of
        Nothing -> return ()
        Just input -> do 
          liftIO (putStrLn "asd")
          x <- liftIO (runExceptT $ foo input)
          case x of
            Left err  -> outputStrLn err
            Right res -> outputStrLn (show res)
          loop

main :: IO ()
main = runRepl >> putStrLn "Goodbye!"

