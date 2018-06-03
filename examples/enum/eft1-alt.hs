enumOrd :: Ordering -> Ordering -> [Ordering]
enumOrd b e = 
  if b <= e
  then b : (bs e)
  else []
    where bs = enumOrd (succ b) 

