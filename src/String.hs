module String (padLeft) where

padLeft :: Int -> Char -> String -> String
padLeft count c s = (replicate (count - (length s)) c) ++ s
