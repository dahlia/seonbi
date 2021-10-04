{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
main :: IO ()
main = do
    putStrLn "Temporarily, doctests are ignored for GHC >= 8.10 due to bugs:\n"
    putStrLn "  https://github.com/sol/doctest/issues/301"
#elif __GLASGOW_HASKELL__ >= 808 && defined(mingw32_HOST_OS)
main :: IO ()
main = do
    putStr "Temporarily, doctests are ignored for GHC >= 8.8 on Windows "
    putStrLn "due to bugs:\n"
    putStrLn "  https://github.com/sol/doctest/issues/300"
#else
{-# OPTIONS_GHC -F -pgmF doctest-discover -optF test/doctest.json #-}
#endif
