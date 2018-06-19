module MyPrelude where

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err value =
    case value of
        Just success -> Right success
        Nothing -> Left err
