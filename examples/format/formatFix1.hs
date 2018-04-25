module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

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
-- format.hs:16:13: error: parse error on input ‘->’
--    |
-- 16 |       False -> "no broken rules here... " ++ truth
--    |             ^^
-- Failed, no modules loaded.
-- ```
