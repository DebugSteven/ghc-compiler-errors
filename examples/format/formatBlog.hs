-- import Modules & language extensions must come before any code
-- My personal preference is to have language extensions first (with
-- `LANGUAGE` in all caps, but it can also be lowercase) followed by
-- the modules you would like to import. 
--
-- Your file will load when it's empty, & with just language extensions
-- & the modules you would like to import.
{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

-- you can have functions without type signatures
someFunc = print "blah"

-- DIFFERENT FILE START
-- THIS WILL BE OUR FIRST TYPE ERROR.
-- we can specify types for our values & variables.
anotherFunc = print ("hey" :: String)

-- But be careful with this.
-- `anotherFunc = print "hey" :: String`
-- We get type errors if we don't say an entire expression is a type it
-- isn't. In this example, we are staying the whole expression `print "hey"` is a `String`
-- or `[Char]`. 
-- ```
-- format.hs:15:15: error:
--     • Couldn't match type ‘IO ()’ with ‘[Char]’
--       Expected type: String
--         Actual type: IO ()
--     • In the expression: print "hey" :: String
--       In an equation for ‘anotherFunc’:
--           anotherFunc = print "hey" :: String
-- Failed, modules loaded: none.
-- ```
-- DIFFERENT FILE END

-- ```
-- main :: Int
-- main = 5
-- 
-- format.hs:6:1: error:
--     • Couldn't match expected type ‘IO t0’ with actual type ‘Int’
--     • In the expression: main
--       When checking the type of the IO action ‘main’
-- Failed, modules loaded: none.
-- ```
-- main function must have the type IO ().
main :: IO ()
main = print "hello world"

-- none of this biz
-- main :: Int -> IO ()
-- main num = print num
--
-- other rules for parse errors include
-- the correct use of $, <-, case of, let (in) & where.
-- The focus of this section is to go over the formatting.
-- Don't worry about what the code is trying to do.

