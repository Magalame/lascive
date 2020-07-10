{-# LANGUAGE MagicHash #-}

module Main where

import Lib



main :: IO ()
main = do
    let m = 3
        n = 4*4 -- 4 avx registers, each can pack 4 doubles
        
        a = matrixFromList m 1 $ take m $ repeat (1 :: Double)
        b = matrixFromList 1 n $ [0..4*4-1 :: Double]
        c = matrixFromList m n $ take (m*n) $ repeat (0 :: Double)

    let 
        d = kernel a b c

    print d

