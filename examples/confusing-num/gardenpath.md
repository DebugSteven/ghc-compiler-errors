```
Prelude> 1 + id

<interactive>:1:1: error:
    • Non type-variable argument in the constraint: Num (a -> a)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a. Num (a -> a) => a -> a
Prelude> :set -XFlexibleContexts 
Prelude> 1 + id

<interactive>:3:1: error:
    • No instance for (Show (a0 -> a0)) arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
Prelude> instance Show (a -> b) where; show _ = "<func>"
Prelude> 1 + id

<interactive>:5:1: error:
    • No instance for (Num (a0 -> a0)) arising from a use of ‘it’
        (maybe you haven't applied a function to enough arguments?)
    • In the first argument of ‘print’, namely ‘it’
      In a stmt of an interactive GHCi command: print it
Prelude> 
```
