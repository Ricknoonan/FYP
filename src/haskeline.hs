import Control.Monad.Except

import System.IO.Unsafe (unsafePerformIO)
import System.Console.Haskeline


type ErrorWithIO = ExceptT String IO


foo :: String -> ErrorWithIO String
foo "paul" = do liftIO $ putStrLn "Paul!"
                return "OK!"
foo _ = throwError "ERROR!"


runRepl :: IO ()
runRepl = runInputT defaultSettings $ loop


loop :: InputT IO ()
loop = do
    line <- getInputLine "> "
    case line of
        Nothing -> return ()
        Just input -> do return $ putStrLn "asd"
                         case unsafePerformIO $ runExceptT $ foo input of
                             Left err -> outputStrLn err >> loop
                             Right res -> do
                                 x <- outputStrLn . show $ res
                                 loop

main :: IO ()
main = runRepl >> putStrLn "Goodbye!"