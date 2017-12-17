module Main where

import Data.List (sort)

sortWrapper xs = sort xs

main = print $ sortWrapper [3,2,1]
