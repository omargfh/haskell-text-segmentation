-- | Define module Main for Word Segmenter
module Main where

    import System.IO
    import Data.Functor

    import HashedList
    import WordSeg

    -- Execute IO code
    main :: IO ()
    main = do
        -- Opens a dictionary file
        handle   <- openFile "large" ReadMode
        -- Loads dictionary into memory
        dict     <- hGetContents handle <&> words <&> toVector <&> loadVector
        -- Gets input from user and processes it then outputs the result
        getContents <&> smartSegment dict >>= putStrLn
        -- Closes file
        hClose handle