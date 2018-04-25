module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

someFunc = print "hey"

main :: Int
main = print "hello world"

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
-- format.hs:10:1: error:
--     • Couldn't match expected type ‘IO t0’ with actual type ‘Int’
--     • In the expression: main
--       When checking the type of the IO action ‘main’
--    |
-- 10 | main = print "hello world"
--    | ^
-- 
-- format.hs:10:8: error:
--     • Couldn't match expected type ‘Int’ with actual type ‘IO ()’
--     • In the expression: print "hello world"
--       In an equation for ‘main’: main = print "hello world"
--    |
-- 10 | main = print "hello world"
--    |        ^^^^^^^^^^^^^^^^^^^
-- Failed, no modules loaded.
-- ``` 
