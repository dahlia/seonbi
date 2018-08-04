import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments = ["app", "src", "test"]

main :: IO ()
main = do
    hlints <- hlint arguments
    case hlints of
        [] -> exitSuccess
        _ -> exitFailure
