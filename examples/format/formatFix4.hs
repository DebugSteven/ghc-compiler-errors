module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

someFunc = print "hey"

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
ruleBreaker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = 
             "sorry, that isn't true"

lie = 
 "this code will compile fine"

-- ```
-- [1 of 1] Compiling Main             ( format.hs, interpreted )
-- 
-- format.hs:1:1: error:
--     The IO action ‘main’ is not defined in module ‘Main’
--   |
-- 1 | module Main where
--   | ^
-- Failed, no modules loaded.
-- ``` 
