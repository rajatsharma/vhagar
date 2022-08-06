{-# LANGUAGE OverloadedStrings #-}

module GenUtils where

import Data.Text (Text, pack, replace, toTitle, unpack)

(~>) :: String -> String -> Text -> Text
(~>) a b = replace (pack a) (pack b)

lineline :: String -> String -> String
lineline a b = a ++ "\n\n" ++ b

line :: String -> String -> String
line a b = a ++ "\n" ++ b

linetab :: String -> String -> String
linetab a b = a ++ "\n\t" ++ b

(++>) :: String -> String -> Text -> Text
(++>) marker content = marker ~> (content `lineline` marker)

(+>>) :: String -> String -> Text -> Text
(+>>) marker content = marker ~> (content `linetab` marker)

toTitleString :: String -> String
toTitleString = unpack . toTitle . pack
