enumOrd :: Ordering -> Ordering -> [Ordering]
enumOrd b e  
    | b < e = b : bs
    | b > e = []
    | b == e = [e] 
      where bs = enumOrd (succ b) e
