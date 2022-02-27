{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HW3.Pretty where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Char (toLower)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Ratio (denominator, numerator)
import Data.Scientific (FPFormat (Fixed))
import qualified Data.Scientific as Sc
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import HW3.Base
import Prettyprinter.Internal (encloseSep, pretty)
import Prettyprinter.Internal.Type (Doc)
import Prettyprinter.Render.Terminal.Internal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueBytes bytes) = prettyArray "[#" "#]" " " (bytesToStr bytes)
prettyValue (HiValueList lst) = prettyArray "[" "]" ", " (prettyValue <$> toList lst)
prettyValue (HiValueString text) = pretty $ show text
prettyValue HiValueNull = "null"
prettyValue (HiValueBool bool) = pretty $ map toLower (show bool)
prettyValue (HiValueFunction fun) = pretty $ functionToString fun
prettyValue (HiValueNumber num) = pretty $ numberToString num
prettyValue (HiValueAction action) = prettyAction action
prettyValue (HiValueTime time) = (pretty $ functionToString HiFunParseTime) <> prettyStringArgs [show time]
prettyValue (HiValueDict dict) = prettyArray "{" "}" ", " $
    map (\(key, value) -> (prettyValue key) <> ": " <> (prettyValue value)) (M.toList dict)

-- | Prettify unperformed actions
prettyAction :: HiAction -> Doc AnsiStyle
prettyAction = \case
  (HiActionRead f) -> "read" <> prettyStringArgs [f]
  (HiActionWrite s f) -> "write" <> encloseSep "(" ")" ", " [pretty $ show s, prettyValue (HiValueBytes f)]
  (HiActionMkDir f) -> "mkdir" <> prettyStringArgs [f]
  (HiActionChDir f) -> "cd" <> prettyStringArgs [f]
  HiActionCwd -> "cwd"
  HiActionNow -> "now"
  (HiActionRand f s) -> "rand" <> encloseSep "(" ")" ", " (map pretty [f, s])
  (HiActionEcho text) -> "echo" <> prettyStringArgs [(T.unpack text)]

-- | Prettify a list of arguments, add "" to string values. E.g ("name", "word")
prettyStringArgs :: [String] -> Doc AnsiStyle
prettyStringArgs args = encloseSep "(" ")" ", " (map (pretty . show) args)

-- | Show rational number with period as a fraction
rationalToString :: Rational -> String
rationalToString num =
  let (res, remainder) = quotRem (numerator num) (denominator num)
      whole
        | remainder > 0 && res /= 0 = (show res) <> " + "
        | remainder > 0 && res == 0 = ""
        | remainder < 0 && res /= 0 = (show res) <> " - "
        | otherwise = "-"
   in whole <> show (abs remainder) <> "/" <> show (denominator num)

-- | Show all numbers
numberToString :: Rational -> String
numberToString num =
  let (sc, repetend) = Sc.fromRationalRepetendUnlimited num
   in case repetend of
        Nothing ->
          case Sc.floatingOrInteger sc of
            Left _    -> Sc.formatScientific Fixed Nothing sc
            Right int -> show int
        Just _ -> rationalToString num

-- | Add whitespaces, opening and closing parenthesis to the arguments. E.g. [ 1 ]
prettyArray
  :: String           -- ^ opening parenthesis
  -> String           -- ^ closing parenthesis
  -> String           -- ^ separator
  -> [Doc AnsiStyle]  -- ^ list of arguments
  -> Doc AnsiStyle
prettyArray open close sep lst =
  case lst of
    [] -> pretty $ open <> " " <> close
    _  -> encloseSep (pretty $ open <> " ") (pretty $ " " <> close) (pretty sep) lst

-- | Prettify bytestring of hexadecimal 2-digit numbers.
bytesToStr :: B.ByteString -> [Doc AnsiStyle]
bytesToStr bytes =
  let nums = B.unpack bytes
   in map (pretty . E.decodeUtf8 . LB.toStrict . BB.toLazyByteString . BB.word8HexFixed) nums
