enumOrd :: Ordering -> Ordering -> [Ordering]
enumOrd b e = 
  if b <= e
  then b : bs
  else []
    where bs = enumOrd (succ b) 
