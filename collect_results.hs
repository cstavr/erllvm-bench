{-# LANGUAGE TypeFamilies, GADTs, ScopedTypeVariables #-}


import System.IO
import System.Directory
import Data.List
import Text.Printf

main = do
  cBeam <- readFile "results/runtime_beam.res"
  let names = map (head . words) (lines cBeam)
  let names2 = map (printf "%15s") names
  let beam = map read_num (lines cBeam)
  cHipe <- readFile "results/runtime_hipe.res"
  let hipe = map read_num (lines cHipe)
  cErllvm <- readFile "results/runtime_erllvm.res"
  let erllvm = map read_num (lines cErllvm)
  cErjang <- readFile "results/runtime_erjang.res"
  let erjang = map read_num  (lines cErjang)
  cErjang1 <- readFile "results/runtime_erjang_1.res"
  let erjang1 = map read_num  (lines cErjang1)
  putStrLn "# Name\tBeam\tHiPE\tErLLVM\tErjang1\tErjang"
  let tab = take (length names) (repeat "\t")
  let foo = zipAll [names2, tab, beam, tab, hipe, tab, erllvm, tab, erjang1, tab, erjang]
  let foo1 = map unwords foo
  putStrLn (unlines foo1)

read_num = printf "%6s" . last . words

zipAllWith :: ([a] -> b) -> [[a]] -> [b]
zipAllWith _ []  = []
zipAllWith f xss = map f . transpose $ xss

zipAll = zipAllWith id

divide (x,y) = printf "%.3f" (x1/y1)
  where x1 = read x :: Double
        y1 = read y :: Double
