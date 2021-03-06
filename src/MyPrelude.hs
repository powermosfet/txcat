module MyPrelude where

import Numeric (showFFloat)

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err value =
    case value of
        Just success -> Right success
        Nothing -> Left err

prettyAmount :: Rational -> String
prettyAmount r = 
    r
        |> (fromRational :: (Rational -> Float))
        |> (\n -> showFFloat (Just 2) n "") 
        |> (replicate 10 ' ' ++)
        |> reverse
        |> take 9
        |> reverse

rPad :: Int -> String -> String
rPad n s = take n (s ++ (replicate n ' '))

lPad :: Int -> String -> String
lPad n s = reverse $ rPad n (reverse s)
