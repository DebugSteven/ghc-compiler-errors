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
      where truth = 
             "sorry, that isn't true"

lie = 
 "this code will compile fine"
 
-- ```
-- [1 of 1] Compiling Main             ( format.hs, interpreted )
-- 
-- format.hs:12:1: error:
--     The type signature for ‘ruleBreaker’ lacks an accompanying binding
--    |
-- 12 | ruleBreaker :: Bool -> String
--    | ^^^^^^^^^^^
-- Failed, no modules loaded.
-- ```
