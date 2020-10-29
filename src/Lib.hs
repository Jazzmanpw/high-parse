module Lib (
  HPROption(..),
  regex,
  rTail,
  rValue,
  text) where

import qualified Data.ByteString.Char8 as C
import Data.List (nub)
import Data.Text (isPrefixOf, pack)
import Text.Regex.PCRE.Light

data HPROption = IgnoreCase | DotAll

data Result t = Result t String | None deriving (Show, Eq)

rValue :: Result t -> Maybe t
rValue (Result t _) = Just t
rValue None = Nothing

rTail :: Result t -> Maybe String
rTail (Result _ s) = Just s
rTail None = Nothing

text :: String -> String -> Result String
text t source = if pack t `isPrefixOf` pack source
  then Result t (drop (length t) source)
  else None

regex :: String -> [HPROption] -> String -> Result String
regex r options source = doMatch $ match (compile (C.pack r) o) (C.pack source) [] where
  o = anchored : nub (map convertOption options)
  convertOption IgnoreCase = caseless
  convertOption DotAll = dotall
  doMatch (Just (v:_)) = Result (C.unpack v) (drop ((length . C.unpack) v) source)
  doMatch Nothing = None
