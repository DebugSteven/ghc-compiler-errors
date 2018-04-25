{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

module Main where

someFunc = print "hey"

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebreaker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
      False -> "no broken rules here... " ++ truth
        where truth = "sorry, that isn't true"

let lie = 
"this code will compile fine"

-- ``` 
-- [1 of 1] Compiling Main             ( format.hs, interpreted )
-- 
-- format.hs:5:1: error: parse error on input ‘module’
--   |
-- 5 | module Main where
--   | ^^^^^^
-- Failed, no modules loaded.
-- ```
