## Seriously, 
## the Haskell Type Checker is Your Friend: 
### a Pragmatic Approach to Working with GHC Errors

#### by J Haigh
#### @DebugSteven

Note:
Hi I’m J & I’m DebugSteven on Twitter if you’d like to follow me there. Today I'm going to talk about compiler errors by iterating over code examples & explaining why we get those errors & how to fix them. Over the last year I’ve become friends with the Glorious Glasgow Haskell Compilation System. For this presentation, I’ll be using GHC version 8.2.2. I started self studying Haskell after I completed my bachelor’s in Computer Science last year and I started a Haskell Book Club, here in Denver, in August. We completed the entire book 3 weeks ago. I struggled along with my fellow attendees to get my Haskell code working, which meant reading the error messages & trying to figure out what they mean. Working with the Haskell compiler errors can be really difficult because Haskell is very different from most other programming languages people are used to. But if you practice, like we’ll do today, you’ll be able to do it! & I’m hoping by the end of this talk you’ll have a better understanding of GHC’s errors!  

---

#### Lovely Haskell Types & What They Look Like

```haskell
data Bool     = False | True
data Char     = GHC.Types.C# GHC.Prim.Char#  
data Int      = GHC.Types.I# GHC.Prim.Int# 
data Ordering = LT | EQ | GT
type String   = [Char]
data (,) a b  = (,) a b      -- definition for a tuple
data [] a     = [] | a : [a] -- definition for a list
-- (:) is the cons operator & appends an element onto a list

```

Note:
Most of the errors we occur when we program are type errors. These type errors can manifest in different ways. In dynamic languages for example, it's with runtimes errors. One of your most useful tools in that instance is peppering your program with print statements to figure out where the error is occuring & why. That's not really efficient though & there are several drawbacks. You can forget about your print statements later on. It's a slow feedback loop to figure out why your program isn't working. & it's subject to user error. In Haskell though, since we have types, like Bool, Int, String, & so on (& we aren’t putting undefined all over our program), we have greater confidence about what we will get out of our functions. The type signatures on our functions can help guide us to correct code & GHC will use the types to help point out where we’ve gone wrong too.

---

#### GHC Referee
![ref](referee.png)
<span style="font-size: 0.5em;">Drawing by [Lee Baillie](https://twitter.com/_lbaillie)</span>

Note:
However the trade off for that confidence we get with strong and static types is a picky, rule oriented compiler. You’re going to spend a little more time upfront trying to figure out how to fix compiler errors. This, in my opinion, is significantly better than spending your time trying to fix type errors after they're discovered at runtime. I like to think of GHC as a referee. They will tell you where the error occurred, what rule you broke & what was expected by the compiler. This is super helpful. You can read that error & then fix your code to adhere to the rules! 

---

#### Oh, The Kinds of Errors You'll See

- Parse Errors |
- Type Errors |

Note:
Parse errors are when we have broken a formatting rule or some convention enforced by the compiler.
Type errors are when we told the compiler we would do something via our types
and we haven't followed through in our function. Let's take a look at parse errors first. 

---

#### Formatting Rules

```haskell
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

module Main where

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
      False -> "no broken rules here... " ++ truth
        where truth = "sorry, that isn't true"

let lie = 
"this code will compile fine"
```

Note:
Here we have a file that's breaking some formatting rules. 
Let's try compiling this & see what error messages we get.

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

Note:
Error. Parse error on input module.
This error occurred on line 5 of our file.
Let's go look at it.

+++

```haskell
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

module Main where

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
      False -> "no broken rules here... " ++ truth
        where truth = "sorry, that isn't true"

let lie = 
"this code will compile fine"
```
@[5]()

Note:
Here is the line that GHC is pointing us to in the error message.

+++

#### Top Of File

- Module name is declared above imports & code | 
- Imported modules listed before functions |
- Language extensions by convention are usually listed at the top of the file |

Note:
If you decide to give your file a module name,
it must be above the imported modules & functions, with a capitalized name.
Then you list modules you would like to import into your file. 
Imported modules must come before your functions. 
Language extensions or pragmas by convention are
at the top of your file but they can be anywhere in the file & I like
to have LANGUAGE in all uppercase because I like to yell as much as
possible in my code.

+++

#### Fix #1
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

-- This was our broken version
-- {-# LANGUAGE InstanceSigs #-}
-- 
-- import Control.Applicative
-- 
-- module Main where
```

+++

#### Order Matters
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
      False -> "no broken rules here... " ++ truth
      where truth = "sorry, that isn't true"

let lie = 
"this code will compile fine"
```

Note:
We can fix the error we got by moving module above 
the import in our file.
Let's reload to see our next error.

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

Note:
Error. Parse error on input right arrow.
It shows us that the error in on line 14.
Let's go check out the code.

+++

```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
      False -> "no broken rules here... " ++ truth
      where truth = "sorry, that isn't true"

let lie = 
"this code will compile fine"
```

@[13-18]()

Note: 
The error is on the line that starts with False, line 14.
But this entire code block has a similar problem.
So keep in mind the where block and the let for the rules we discuss next.

+++

#### Indentation Rules

- Code implementations start at least 1 space after the function name on the following line | 
- Code blocks must spatially align | 
- New code blocks inside of other functions must be 1 space over to denote a new block |
- For readability, may I recommend 2 spaces |

Note:
My most common one is not having enough spaces between my function name 
and on the next line my implementation. You need to have the 
implementation 1 space over on the next line compared to the function name. 
This rule applies for let expressions, case of expressions, guards, & in where blocks!

Corrections:
I'd like to the indentation rules more clear.
I think it'd be useful to show correct examples of indentation using
guards & let in expressions especially. Rule #3 should be specified
to say that the implementation inside of new code blocks must be
at least 1 space over & explain when a new code block is started.

+++
#### Fix #2
```haskell
ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = "sorry, that isn't true"

let lie = 
     "this code will compile fine"
-- rulebraker b = 
--   case b of
--     True -> "yeah this code doesn't follow the rules"
--       False -> "no broken rules here... " ++ truth
--       where truth = "sorry, that isn't true"
-- 
-- let lie = 
-- "this code will compile fine"
```
+++

#### Indentation Fixed
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = "sorry, that isn't true"

let lie = 
     "this code will compile fine"
```
Note:
We fixed all our indentation problems by following
the rules we just talked about! Let's reload the
file & see where we're at with this file now.

+++

#### Error #3
```
[1 of 1] Compiling Main             ( format.hs, interpreted )

format.hs:19:1: error:
    parse error (possibly incorrect indentation or mismatched brackets)
Failed, no modules loaded.
```
Note:
Error. Parse error. Possibly incorrect indentation.
I know what you're thinking, "you said we fixed all our indentation!"
I promise I didn't lie.
This error occurs on line 19 & we don't have a line 19 in our file!
Let's go look at the end of the file instead.

+++

```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = "sorry, that isn't true"

let lie = 
     "this code will compile fine"
```
@[17-18]()
Note:
We have the let expression highlighted here.
Why doesn't this work? Well, let's look at the rule.

+++

#### Functions are Top Level Declarations
`let` and `where` are meant to define functions
inside other functions within a local scope.


Just a function name at the top level will be fine!

Note:
We can't have anything but functions at the top
level of our file. You might be use to declaring
things that look like variables using an identifier
to distinguish it from functions, but in
Haskell everything is a function!

+++
#### Fix #3
```haskell
lie = 
  "this code will compile fine"

-- let lie = 
--      "this code will compile fine"

```

+++

#### Functions at The Top Level
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = 
             "sorry, that isn't true"

lie = 
 "this code will compile fine"
```

Note:
Let's just get rid of our let here & recompile.

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

Note:
Error. The type signature for ruleBreaker doesn't
have a function associated with it.
Let's go look at line 10 of our file.

+++

```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative

mymain :: Int
mymain = print "hello world"

ruleBreaker :: Bool -> String
rulebraker b = 
  case b of
    True -> "yeah this code doesn't follow the rules"
    False -> "no broken rules here... " ++ truth
      where truth = 
             "sorry, that isn't true"

lie = 
 "this code will compile fine"
```
@[10-11]()

Note:
The type signature & the function name are highlighted
here. You may notice that we have a typo
between the 2.

+++

#### If you have a type signature,
#### you must have a function implementation with it

+++
#### Fix #4
```haskell
ruleBreaker :: Bool -> String
ruleBreaker b = ... 

-- ruleBreaker :: Bool -> String
-- rulebraker b = ... 
```
+++

#### Type signatures for functions that exist
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

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

Note:
Let's fix our typo & reload our code!

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

Note:
The next error we get here says that the function main
isn't found in our module Main. Let's look at the code
and see if that's true.

+++

```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

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
@[3, 7-8]()

Note:
We have module Main on line 3 and we don't have a main
function anywhere in our file. Instead we have this function
called mymain.

+++

#### module Main where
`module Main` must have a `main` function

If you don't want a `main` function,
rename your module to anything other than `Main`.

Note:
So the rule we're breaking here is that if you use module Main
you have to have a function called main. If for whatever reason
you don't want to have a main function, just name your module
anything else (that starts with a capital letter).

+++
#### Fix #5
```haskell
module Main where

main :: Int
main = print "hello world"

-- module Main where
-- 
-- mymain :: Int
-- mymain = print "hello world"
```
+++

#### main for Main
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

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

Note:
So we will rename mymain to main & reload our code
to see how we're doing.

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

Note: 
Really this is 2 errors, but they go really well hand in hand.
The first error says we couldn't match the expected type of main,
which is IO of something, with the actual type, in the type signature, Int.
The compiler tells us on the 2nd error that we told them we would
give them an Int, but we are actually providing an IO of Unit.
It points to line 8, specifically at the expression: 
print "hello world". This is expected because the type of print
is a to IO of Unit where a has the constraint to have an instance of Show.
Let's go look at those lines.

+++

```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

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

Note:
We did in fact say we would return an Int here
& we aren't doing it.

+++

### main :: IO Type
`main` always returns `IO` of some type.
Usually main has a return value of `IO ()`

Note: In our main function we use print.
print has the type a to IO (). So we know we'll want to return IO ()

The rule to use function main is that main must return IO of some type.
It doesn't have to be IO (), but print has the return type of IO ().
Let's change our type signature of main to IO ().

+++

#### Fix #6
```haskell
main :: IO ()
main = print "hello world"

-- main :: Int
-- main = print "hello world"
```

+++

#### IO & main, together forever
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

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

Note:
Here we are, main has the type IO of Unit, which reflects
the type that print "hello world" gives us. Let's reload.

+++

```
[1 of 1] Compiling Main             ( format.hs, interpreted )
Ok, one module loaded.
```

Note:
Sweet! Our file is properly formatted now!
There are no more errors in our code & this will run fine.
So... just one more thing. :)

---

#### Properly formatted file!
```haskell
{-# LANGUAGE InstanceSigs #-}

module Main where

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

Note:
Let's change the strings to reflect our code compiling haha.
You can flip back to look at the previous file & compare it
to our fixed file!

---

### Error Messages are Dishes Best Served Statically

Onto Type Errors! 

Note: 
Assume the code we're looking at is in a file & we don't have the implicit prelude.
We'll continue using the REPL to check ourselves as we go!

---

### Module or Define
Note:
The first error we will look at is when you need to define or bring in a module
to fix a function not being in scope.

+++

```haskell
λ> :t head
head :: [a] -> a
λ> head []
*** Exception: Prelude.head: empty list
```

Note:
Here is the type signature for `head`.
Given a list it will give you the first element of the list back.
However, if you give it an empty list it will blow up.
Let's try writing an implementation that protects us from
this exception.

+++

```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Maybe head xs
```

Note:
Here we've defined a safe version of the head function.
Our `safeHead` function should handle the case of an empty list,
by returning `Nothing` instead of giving us a runtime exception.
If we have a list with at least one element we should get back
one value inside of our wrapper.

+++

```
ghci> :l tmr.hs
tmr.hs:1:20: error: Not in scope: type constructor or class ‘Maybe’
  |
1 | safeHead :: [a] -> Maybe a
  |                    ^^^^^
Failed, no modules loaded.
```
Note:
When we compile this we've get an error that the data
constructor `Maybe` isn't in scope.

+++

```
ghci> :i Maybe
data Maybe a = Nothing | Just a -- Defined in ‘Data.Maybe’
```
Note:
Data.Maybe is part of the prelude so if you haven't turned that off you won't
get an error for Maybe not being defined overall. However, let's read this 
data declaration.
data, `Maybe a` is a type constructor that takes one type argument and
the possible values of `Maybe` are the data constructors `Nothing`
and `Just a` where `a` is some type. So in our function, if we gave `safeHead` a 
list of `Int`, the type of `a` would be an `Int`.

+++

```haskell
import Data.Maybe
import Data.List

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Maybe head xs
```
Note:
We can fix the errors we get by importing the module that defines the type constructor
`Maybe` & changing our last line to use the defined data constructor `Just` because
we only use type constructors in our type signatures & we only use data constructors
in our functions.

We still have an error left in this example, but we'll come back to it after we look
at one more example of this kind of error.

Corrections:
I accidentally left `Maybe` in the last line when I should've changed it to `Just`.
(I fixed it here because I'd like to give this talk again soon.)
There should also be a section talking about using the type constructor instead
of the data constructor as an error because that's what the first error was.
Not having the type constructor Maybe in scope is a different error.

+++

### Data Constructors & Type Constructors

+++

```
λ> :r
[1 of 1] Compiling Main             ( tmr.hs, interpreted )
tmr.hs:6:15: error:
    • Data constructor not in scope:
        Maybe :: ([a0] -> a0) -> [a] -> Maybe a
    • Perhaps you meant variable ‘maybe’ (imported from Data.Maybe)
  |
6 | safeHead xs = Maybe head xs
  |               ^^^^^
Failed, no modules loaded.
```

+++

```haskell
import Data.Maybe
import Data.List

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just head xs
```

+++

### Too Many Arguments

+++

```
ghci> :r tmr.hs
[1 of 1] Compiling SafeHead ( tmr.hs, interpreted )
tmr.hs:6:15:
The function ‘Just’ is applied to two arguments,
but its type ‘a0 -> Maybe a0’ has only one
In the expression: Just head xs
In an equation for ‘safeHead’: safeHead xs = Just head xs
Failed, modules loaded: none.
```

+++

```haskell
import Data.Maybe
import Data.List

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)
```

---

### Another Example of Imports
Note:
Let's take a look at another function.
This example has the prelude imported.

+++

```haskell
module Sort where

sortWrapper xs = sort xs
```

Note:
We have a function called sortWrapper that takes one argument called
`xs` & we just want to sort it. `xs` is a list of some type that can be ordered.
Let's compile this.

Correction:
I'd like to show the type signature for sort.

+++

```
[1 of 1] Compiling Sort ( tmr.hs, interpreted )
tmr.hs:3:22:
Not in scope: ‘sort’
Perhaps you meant ‘sqrt’ (imported from Prelude)
Failed, modules loaded: none.
```
Note:
We get an error that says the function sort used on line 3 isn't in scope.
GHC even suggests the closest named function that we have in scope called
`sqrt`. But that isn't what we're looking for. We can use Hoogle to figure
out what module this function is defined though!

(This might be a thing I demo.)

+++

```haskell
module Sort where

import Data.List (sort)

sortWrapper xs = sort xs
```

Note:
We can see that `sort` is defined in `Data.List` & we can just import
that one function by listing it in parentheses after our module name.
This will work. :)

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

Note:
Data declaration for Nat. The deriving part at the end gives us Equality & the ability to print out the constructors for free.

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
λ> :t enumFromTo
enumFromTo :: Enum a => a -> a -> [a]
λ> :i Ordering 
data Ordering = LT | EQ | GT
λ> enumFromTo LT GT
[LT,EQ,GT]
λ> enumFromTo GT LT
[]
λ> :t succ
succ :: Enum a => a -> a
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

## Thank You!

http://tiny.cc/HBCSignup 

Note: If you're interested in learning Haskell by working through
Haskell Programming from first prinicples by Chris Allen & Julie Moronuki
I'm running through this book to the Monad chapter with Pyrrh starting on
June 18th. The signup & details can be found at this link! Thank you! 
