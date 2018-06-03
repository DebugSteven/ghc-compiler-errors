data Nat = Zero | Succ Nat deriving (Eq, Show)

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat 1 = Just (Succ Zero)
integerToNat i = if i < 0 
                 then Nothing 
                 else Just (Succ (integerToNat i-1))
