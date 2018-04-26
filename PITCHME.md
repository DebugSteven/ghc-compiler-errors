## Seriously, 
## the Haskell Type Checker is Your Friend: 
### a Pragmatic Approach to Working with GHC Errors

#### by J Haigh
#### @DebugSteven

Note:
Hi I’m J (& I’m DebugSteven on Twitter if you’d like to follow me there) and over the last year I’ve become friends with the Glorious Glasgow Haskell Compilation System. For this presentation I’ll be using GHC version 8.2.2. I started self studying Haskell after I completed my bachelor’s in Computer Science last year and I started a Haskell Book Club here in Denver in August. We have worked through almost the entire book at this point. I’ve struggled along with my fellow attendees to get my Haskell code working, which means reading the error messages and trying to figure out what they mean. Working with the Haskell compiler errors can be really difficult, because Haskell very different from most other languages people are used to. But if you practice like we’ll do today, you’ll be able to do it! and I’m hoping by the end of this talk you’ll have a better understanding of GHC’s errors!  

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

Note:
My experience of other programming languages, (& maybe you can relate) is quickly typing out what I want my program to do. Usually getting an error because I don’t get things right on the first try. & whether it’s a typo, syntax error, invalid memory access, logic errors that make my code crash, I go check out the error. I immediately jump to the line number the error message told me if there is one. I’ll put in print statements to see what I’m really getting & go from there to try to make it work. In Haskell though, since we have types, like Bool, Int, String, so on (& we aren’t putting undefined all over our program) we have greater confidence about what we will get out of our functions. The type signatures on our functions can help guide us to correct code & GHC will use the types to help point out where we’ve gone wrong too.

---

#### GHC Referee
![ref](referee.png)
<span style="font-size: 0.5em;">Drawing by [Lee Baillie](https://twitter.com/_lbaillie)</span>

Note:
However the trade off for that confidence we get with strong and static types is a picky, rule oriented compiler. You’re going to spend a little more time upfront trying to figure out how to fix compiler errors. GHC isn’t going to let you pull any fast ones. I like to think of GHC as a referee. They will tell you where the error occurred, what rule you broke & what was expected by the compiler. This is super helpful. You can read that error & then fix your code to adhere to the rules! 

---

#### Oh, The Kinds of Errors You'll See
<ol>
<li> Parse Errors </li>
<li> Type Errors </li>
</ol>

Note:
Parse errors are when we have broken a formatting rule.
Type errors are when we told the compiler we would do something via our types
and we haven't followed through.

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

#### Error #1
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

#### Error #2
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:14:13: error: parse error on input ‘->’
   |
14 |       False -> "no broken rules here... " ++ truth
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

Note: The error is on the line that starts with False.
But this entire code block has a similar problem.
So keep in mind the where block and the let for the rules we discuss next.

+++

#### Indentation Rules

<ol>
 <li> Code implementations start at least 1 space after the function name on the following line </li>
 <li> Code blocks must spatially align </li>
 <li> New code blocks inside of other functions must be 1 space over to denote a new block </li>
 <li> For readability, may I recommend 2 spaces </li>
</ol>

Note:
My most common one is not having enough spaces between my function name 
and on the next line my implementation. You need to have the 
implementation 1 space over on the next line compared to the function name. 
This rule applies for let expressions, case of expressions, guards, & in where blocks!

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

#### Error #3
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:19:1: error:
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
@[17-18]()

+++

#### Functions are Top Level Declarations
`let` and `where` are meant to define functions
inside other functions within a local scope.


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

#### Error #4
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:10:1: error:
    The type signature for ‘ruleBreaker’ lacks an 
    accompanying binding
   |
10 | ruleBreaker :: Bool -> String
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
@[10-11]()

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

#### Error #5
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
@[1, 7-8]()

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
#### Error #6
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:8:1: error:
    • Couldn't match expected type ‘IO t0’ with actual type ‘Int’
    • In the expression: main
      When checking the type of the IO action ‘main’
   |
 8 | main = print "hello world"
   | ^

format.hs:8:8: error:
    • Couldn't match expected type ‘Int’ with actual type ‘IO ()’
    • In the expression: print "hello world"
      In an equation for ‘main’: main = print "hello world"
   |
 8 | main = print "hello world"
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
@[7-8]()

+++

### main :: IO Type
`main` always, always, always returns `IO` of some type.
Usually we return `IO ()`

Note: In our main function we use print.
print has the type a to IO (). So we know we'll want to return IO ()

+++

#### IO & main, together forever
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

---

### Error Messages are Dishes Best Served Statically

Onto Type Errors! 

Note: 
Assume the code we're looking at is in a file & we're using the REPL to check ourselves as we go!

---

### Module or Define

+++

```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Maybe head xs
```

+++

```
ghci> :l tmr.hs
[1 of 1] Compiling SafeHead ( tmr.hs, interpreted )
tmr.hs:3:15: Not in scope: data constructor ‘Maybe’
Failed, modules loaded: none
```

+++

```
ghci> :i Maybe
data Maybe a = Nothing | Just a -- Defined in ‘Data.Maybe’
```

+++

```haskell
import Data.Maybe

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Maybe head xs
```

+++

### Another Example

+++

```haskell
module Sort where

sortWrapper xs = sort xs
```

+++

```
[1 of 1] Compiling Sort ( tmr.hs, interpreted )
tmr.hs:3:22:
Not in scope: ‘sort’
Perhaps you meant ‘sqrt’ (imported from Prelude)
Failed, modules loaded: none.
```

+++

```haskell
module Sort where

import Data.List (sort)

sortWrapper xs = sort xs
```

---

### Too Many Arguments

+++

```haskell
import Data.Maybe

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Maybe head xs
```

+++

```
ghci> :r tmr.hs
[1 of 1] Compiling SafeHead ( tmr.hs, interpreted )
tmr.hs:4:15:
The function ‘Just’ is applied to two arguments,
but its type ‘a0 -> Maybe a0’ has only one
In the expression: Just head xs
In an equation for ‘safeHead’: safeHead xs = Just head xs
Failed, modules loaded: none.
```

+++

```haskell
import Data.Maybe

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)
```

---

### Type Mismatch

+++

```haskell
data Nat = Zero | Succ Nat deriving (Eq, Show)

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat 1 = Just (Succ Zero)
integerToNat i = if i < 0 
                 then Nothing 
                 else Just (Succ (integerToNat i-1))
```

+++

```
Natural.hs:15:35: error:
    • Couldn't match expected type ‘Nat’ with actual type ‘Maybe Nat’
    • In the first argument of ‘Succ’, namely ‘(integerToNat i - 1)’
      In the first argument of ‘Just’, namely
        ‘(Succ (integerToNat i - 1))’
      In the expression: Just (Succ (integerToNat i - 1))
   |
15 |                  else Just (Succ (integerToNat i-1))
   |                                   ^^^^^^^^^^^^^^^^
```

+++

#### What Would We Expect That To Do?
```
> integerToNat 3
Just (Succ (Just (Succ (Just (Succ Zero))))
```
Note: 
Is that what we want though? Let's look at the info for Maybe

+++

`data Maybe a = Nothing | Just a`

`Just (Succ (Succ (Succ Zero)))`

+++

```haskell
integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat 1 = Just (Succ Zero)
integerToNat i =
  case i < 0 of
    True  -> Nothing 
    False -> (Just (f i))
    where f 0 = Zero
          f 1 = (Succ Zero)
          f n = (Succ (f(n-1)))
```

---

### Not Enough Arguments

+++

#### EnumFromTo Ordering

```haskell
eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd b e = 
  if b <= e
  then b : bs
  else []
    where bs = eftOrd (succ b) 
```

+++

```
eft.hs:10:12: error:
    • Couldn't match expected type ‘[Ordering]’
                  with actual type ‘Ordering -> [Ordering]’
    • Probable cause: ‘bs’ is applied to too few arguments
      In the second argument of ‘(:)’, namely ‘bs’
      In the expression: b : bs
      In the expression: if b <= e then b : bs else []
   |
10 |   then b : bs
   |            ^^
```

+++

```haskell
eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd b e = 
  if b <= e
  then b : bs
  else []
    where bs = eftOrd (succ b) 
```
@[4,6]()
Note:
Why would `bs` expect another argument? Well, what is bs defined to do?
bs is equal to eftOrd and the next value of b, but eftOrd takes 2 arguments!
Ahh! So we need to either have the give eftOrd another argument in the definition
for bs or we need to pass the argument to bs on line 5.

+++

```haskell
eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd b e = 
  if b <= e
  then b : bs
  else []
    where bs = eftOrd (succ b) e
```

+++

```haskell
eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd b e = 
  if b <= e
  then b : (bs e)
  else []
    where bs = eftOrd (succ b) 
```

+++

```
λ> eftOrd GT GT
[GT*** Exception: Prelude.Enum.Ordering.succ: bad argument
```
Note:
You have may noticed that we don't handle all the cases.
If you didn't notice, neither did I when I first wrote this!
The types didn't save me from this error! We still have to be careful
about writing code that can blow up when we run it. 

+++

```haskell
eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd b e  
    | b < e = b : bs
    | b > e = []
    | b == e = [e] 
      where bs = eftOrd (succ b) e
```

Note:
I ended up writing this with a guard & you may find that using
particular control structures for particular problems
may make more sense to you and be more readable. I find that when
my code is more readable it's easier to understand my type errors too.

---

## Believe In Yourself

Note:
We’ve gone over a lot of errors today, but there’s one thing we haven’t covered. 
Getting good at debugging in any new language takes time. For Haskell specifically, 
I think it can be overwhelming as a beginner because there is so much new stuff you feel 
you need to learn to be good at it. I’ve gotten significantly better over the last year 
because I practiced & wrote a lot of code. You don’t necessarily need to know exactly what a function does to use it. You just need to find what fits & solves your problem & you'll learn along the way. 
If you read the compiler errors like we’ve done today 
and you use Hoogle to look up the type signature you need, 
you can play around with it and get it and you will get better at fixing type errors.

---

# Thank You!
