<!-- 
http://ics.p.lodz.pl/~stolarek/_media/pl:research:stolarek_understanding_basic_haskell_error_messages.pdf
-->

```haskell
safeHead [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Maybe head xs
```

```
ghci> :l tmr.hs
[1 of 1] Compiling Main ( tmr.hs, interpreted )
tmr.hs:1:14: parse error on input ‘->’
Failed, modules loaded: none.
```

```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Maybe head xs
```

```
tmr.hs:3:15: Not in scope: data constructor ‘Maybe’
Failed, modules loaded: none
```

```
ghci> :i Maybe
data Maybe a = Nothing | Just a -- Defined in ‘Data.Maybe’
```

```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just head xs
```

```
tmr.hs:3:15:
The function ‘Just’ is applied to two arguments,
but its type ‘a0 -> Maybe a0’ has only one
In the expression: Just head xs
In an equation for ‘safeHead’: safeHead xs = Just head xs
Failed, modules loaded: none.
```

```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)
```

```haskell
-- sortwrapper/01.hs
module Main where

sortWrapper xs = sort xs
```

```
[1 of 1] Compiling Main ( tmr.hs, interpreted )
tmr.hs:2:22:
Not in scope: ‘sort’
Perhaps you meant ‘sqrt’ (imported from Prelude)
Failed, modules loaded: none.
```

```haskell
import Data.List (sort)

module Main where

sortWrapper xs = sort xs
```

```
Prelude> :l 02.hs 
[1 of 1] Compiling Main             ( 02.hs, interpreted )

02.hs:3:1: error: parse error on input ‘module’
Failed, modules loaded: none.
```

```haskell
module Main where

import Data.List (sort)

sortWrapper xs = sort xs
```

```
Prelude> :l 03.hs 
[1 of 1] Compiling Main             ( 03.hs, interpreted )

03.hs:1:1: error:
    The IO action ‘main’ is not defined in module ‘Main’
Failed, modules loaded: none.
```

```haskell
module SortWrapper where

import Data.List (sort)

sortWrapper xs = sort xs
```

Alternately,

```haskell
module Main where

import Data.List (sort)

sortWrapper xs = sort xs

main = print $ sortWrapper [3,2,1]
```
