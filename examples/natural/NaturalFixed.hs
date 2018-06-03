data Nat = Zero | Succ Nat deriving (Show)

integerToNat :: Integer -> Maybe Nat
integerToNat i =
  case i < 0 of
    True  -> Nothing 
    False -> (Just (f i))
    where f 0 = Zero
          f 1 = (Succ Zero)
          f n = (Succ (f(n-1)))
