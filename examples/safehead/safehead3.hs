import Data.Maybe
import Data.List

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just head xs
