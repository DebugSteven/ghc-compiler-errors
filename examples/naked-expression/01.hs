module Naked where

fact :: Int -> Int
fact 0 = 1
fact n = n * fact ( n - 1 )

printIt = print (fact 5)
