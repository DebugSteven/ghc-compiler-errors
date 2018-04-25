## Seriously, 
## the Haskell Type Checker is Your Friend: 
### a Pragmatic Approach to Working with GHC Errors

#### by J Haigh
#### @DebugSteven

---

## Now Haskell is my Best Friend

_friendship ending with other languages not required_

---

#### Lovely Haskell Types & What They Look Like

```haskell
data Bool     = False | True
data Char     = GHC.Types.C# GHC.Prim.Char#  
data Int      = GHC.Types.I# GHC.Prim.Int# 
data Ordering = LT | EQ | GT
type String   = [Char]
data (,) a b  = (,) a b
data [] a     = [] | a : [a]
```
---

#### GHC Referee
put a picture here

---

#### Oh, The Kinds of Errors You'll See
<ol>
<li> Parse Errors </li>
<li> Type Errors </li>
</ol>

---

#### Formatting Rules

```haskell
{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

module Main where

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
```

+++

#### error #1
``` 
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:5:1: error: parse error on input ‘module’
  |
5 | module Main where
  | ^^^^^^
Failed, no modules loaded.
```

+++

```haskell
{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

module Main where

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
```
@[5]()

+++

#### Top Of File

<ol>
<li> Module name is declared on top line </li>
<li> Language extensions & imported modules listed before functions </li>
</ol>

Note:
If you decide to give your file a module name,
it must be the top line in your file, with a capitalized name.
I personally like to then have my language extensions or pragmas next &
I like to have LANGUAGE in all uppercase. Then I list modules I would like
to import into my file. Language extensions & importing modules must come
before your functions.

+++

#### Move module to top
```haskell
module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

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
```

+++

#### error #2
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:16:13: error: parse error on input ‘->’
   |
16 |       False -> "no broken rules here... " ++ truth
   |             ^^
Failed, no modules loaded.
```

+++

```haskell
module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

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
```

@[13-18]()

+++

#### Indentation Rules

<ol>
 <li> Code implementations start at least 1 space after the function name on the following line </li>
 <li> Code blocks must spatially align </li>
 <li> New code blocks inside of other functions must be 1 space over to denote a new block </li>
 <li> For readability, may I recommend 2 spaces </li>
</ol>
<!--
My most common one is not having enough spaces between my function name 
and on the next line my implementation. You need to have the 
implementation 1 space over on the next line compared to the function name. 
This rule applies for let expressions, case of expressions, guards, & in where blocks!
-->

+++

#### Indent our case, where block, & let expression
```haskell
module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

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
```

+++

#### error #3
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:21:1: error:
    parse error (possibly incorrect indentation or mismatched brackets)
Failed, no modules loaded.
```

+++

```haskell
module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

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
```
@[19-20]()

+++

#### Functions are Top Level Declarations
`let` and `where` are meant to define functions
inside other functions with a local scope.
Just a function name at the top level will be fine!

+++

#### Functions at The Top Level
```haskell
module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

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
```

+++

#### error #4
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:12:1: error:
    The type signature for ‘ruleBreaker’ lacks an accompanying binding
   |
12 | ruleBreaker :: Bool -> String
   | ^^^^^^^^^^^
Failed, no modules loaded.
```

+++

```haskell
module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

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
```
@[12-13]()

+++

#### If you have a type signature,
#### you must have a function implementation with it

+++

#### Type signatures for functions that exist
```haskell
module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

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
```

+++

#### error #5
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:1:1: error:
    The IO action ‘main’ is not defined in module ‘Main’
  |
1 | module Main where
  | ^
Failed, no modules loaded.
``` 

+++

```haskell
module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

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
```
@[1, 9-10]()

+++

#### module Main where
`module Main` must have a `main` function

If you don't want a `main` function pick a different module name.

+++

#### main for Main
```haskell
module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

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
```

+++
#### error #6
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:10:1: error:
    • Couldn't match expected type ‘IO t0’ with actual type ‘Int’
    • In the expression: main
      When checking the type of the IO action ‘main’
   |
10 | main = print "hello world"
   | ^

format.hs:10:8: error:
    • Couldn't match expected type ‘Int’ with actual type ‘IO ()’
    • In the expression: print "hello world"
      In an equation for ‘main’: main = print "hello world"
   |
10 | main = print "hello world"
   |        ^^^^^^^^^^^^^^^^^^^
Failed, no modules loaded.
``` 
+++

```haskell
module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

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
```
@[5-6]()

+++

### main :: IO ()
`main` always, always, always returns `IO ()`

+++

#### IO () & main, together forever
```haskell
module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

main :: IO ()
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
```

+++

```
[1 of 1] Compiling Main             ( format.hs, interpreted )
Ok, one module loaded.
```

---

#### Properly formatted file!
```haskell
module Main where

{-#LANGUAGE InstanceSigs #-}

import Control.Applicative

main :: IO ()
main = print "hello world"

ruleBreaker :: Bool -> String
ruleBreaker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = 
             "it's true!"

lie = 
 "this code won't compile fine"
```
