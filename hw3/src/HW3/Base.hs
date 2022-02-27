{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

module HW3.Base where

import Codec.Serialise
import Data.ByteString.Char8 (ByteString)
import Data.Map.Internal (Map)
import Data.Sequence.Internal (Seq)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data HiFun -- function names (e.g. div, sort, length, ...)
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Ord, Generic, Serialise)

data HiValue -- values (numbers, booleans, strings, ...)
  = HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic, Serialise)

data HiExpr -- expressions (literals, function calls, ...)
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq)

data HiError -- evaluation errors (invalid arguments, ...)
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq, Ord)

data HiAction
  = HiActionRead FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionEcho Text
  | HiActionRand Int Int
  deriving (Show, Eq, Ord, Generic, Serialise)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

functionToString :: HiFun -> String
functionToString = \case
  HiFunLessThan       -> "less-than"
  HiFunGreaterThan    -> "greater-than"
  HiFunEquals         -> "equals"
  HiFunNotLessThan    -> "not-less-than"
  HiFunNotGreaterThan -> "not-greater-than"
  HiFunNotEquals      -> "not-equals"
  HiFunIf             -> "if"
  HiFunLength         -> "length"
  HiFunToUpper        -> "to-upper"
  HiFunToLower        -> "to-lower"
  HiFunReverse        -> "reverse"
  HiFunTrim           -> "trim"
  HiFunList           -> "list"
  HiFunRange          -> "range"
  HiFunFold           -> "fold"
  HiFunPackBytes      -> "pack-bytes"
  HiFunUnpackBytes    -> "unpack-bytes"
  HiFunZip            -> "zip"
  HiFunUnzip          -> "unzip"
  HiFunEncodeUtf8     -> "encode-utf8"
  HiFunDecodeUtf8     -> "decode-utf8"
  HiFunSerialise      -> "serialise"
  HiFunDeserialise    -> "deserialise"
  HiFunAdd            -> "add"
  HiFunSub            -> "sub"
  HiFunMul            -> "mul"
  HiFunDiv            -> "div"
  HiFunNot            -> "not"
  HiFunAnd            -> "and"
  HiFunOr             -> "or"
  HiFunRead           -> "read"
  HiFunWrite          -> "write"
  HiFunMkDir          -> "mkdir"
  HiFunChDir          -> "cd"
  HiFunParseTime      -> "parse-time"
  HiFunRand           -> "rand"
  HiFunEcho           -> "echo"
  HiFunCount          -> "count"
  HiFunKeys           -> "keys"
  HiFunValues         -> "values"
  HiFunInvert         -> "invert"

getArgsNumber :: HiFun -> Int
getArgsNumber fun = case fun of
  HiFunDiv            -> 2
  HiFunMul            -> 2
  HiFunAdd            -> 2
  HiFunSub            -> 2
  HiFunNot            -> 1
  HiFunAnd            -> 2
  HiFunOr             -> 2
  HiFunLessThan       -> 2
  HiFunGreaterThan    -> 2
  HiFunEquals         -> 2
  HiFunNotLessThan    -> 2
  HiFunNotGreaterThan -> 2
  HiFunNotEquals      -> 2
  HiFunIf             -> 3
  HiFunLength         -> 1
  HiFunToUpper        -> 1
  HiFunToLower        -> 1
  HiFunReverse        -> 1
  HiFunTrim           -> 1
  HiFunList           -> -1
  HiFunRange          -> 2
  HiFunFold           -> 2
  HiFunPackBytes      -> 1
  HiFunUnpackBytes    -> 1
  HiFunEncodeUtf8     -> 1
  HiFunDecodeUtf8     -> 1
  HiFunZip            -> 1
  HiFunUnzip          -> 1
  HiFunSerialise      -> 1
  HiFunDeserialise    -> 1
  HiFunRead           -> 1
  HiFunWrite          -> 2
  HiFunMkDir          -> 1
  HiFunChDir          -> 1
  HiFunParseTime      -> 1
  HiFunRand           -> 2
  HiFunEcho           -> 1
  HiFunCount          -> 1
  HiFunValues         -> 1
  HiFunKeys           -> 1
  HiFunInvert         -> 1

functions :: [HiFun]
functions =
  [ HiFunLessThan,
    HiFunGreaterThan,
    HiFunEquals,
    HiFunNotLessThan,
    HiFunNotGreaterThan,
    HiFunNotEquals,
    HiFunIf,
    HiFunLength,
    HiFunToUpper,
    HiFunToLower,
    HiFunReverse,
    HiFunTrim,
    HiFunList,
    HiFunRange,
    HiFunFold,
    HiFunPackBytes,
    HiFunUnpackBytes,
    HiFunEncodeUtf8,
    HiFunDecodeUtf8,
    HiFunZip,
    HiFunUnzip,
    HiFunSerialise,
    HiFunDeserialise,
    HiFunRead,
    HiFunWrite,
    HiFunMkDir,
    HiFunChDir,
    HiFunParseTime,
    HiFunRand,
    HiFunEcho,
    HiFunCount,
    HiFunValues,
    HiFunKeys,
    HiFunInvert,
    HiFunDiv,
    HiFunMul,
    HiFunAdd,
    HiFunSub,
    HiFunNot,
    HiFunAnd,
    HiFunOr
  ]
