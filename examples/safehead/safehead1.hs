import Data.Maybe

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Maybe head xs
