{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 810
{-# OPTIONS_GHC -F -pgmF doctest-discover -optF test/doctest.json #-}
#else
main :: IO ()
main = do
    -- FIXME
    putStrLn "Temporarily, doctests are ignored for GHC >= 8.10 due to a bug:\n"
    putStrLn "  https://github.com/sol/doctest/issues/301"
#endif
