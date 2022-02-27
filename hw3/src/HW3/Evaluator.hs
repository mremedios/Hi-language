{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}

module HW3.Evaluator where

import qualified Codec.Compression.Zlib as Zlib
import qualified Codec.Serialise as Serialise
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (foldl', toList)
import qualified Data.Map as M
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import qualified Data.Sequence.Internal as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Time.Clock as C
import Data.Word
import HW3.Base
import Text.Read (readMaybe)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ evalExpr expr

lazyF :: [HiFun]
lazyF = [HiFunIf, HiFunAnd, HiFunOr]

-- | Apply given function to evaluated arguments
applyFunction :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
--- vararg
applyFunction HiFunList args = return $
  HiValueList $ S.fromList args
--- ternary
applyFunction HiFunIf [HiValueBool bool, f, s] = return $
  if bool then f else s
--- binary
applyFunction HiFunEquals [f, s] = return $ HiValueBool $ f == s
applyFunction HiFunNotEquals [f, s] = return $ HiValueBool $ f /= s
applyFunction HiFunLessThan [f, s] = return $ HiValueBool $ f < s
applyFunction HiFunNotGreaterThan [f, s] = return $ HiValueBool $ f <= s
applyFunction HiFunGreaterThan [f, s] = return $ HiValueBool $ f > s
applyFunction HiFunNotLessThan [f, s] = return $ HiValueBool $ f >= s

applyFunction HiFunAnd [HiValueBool f, HiValueBool s] = return $ HiValueBool $ f && s
applyFunction HiFunOr [HiValueBool f, HiValueBool s] = return $ HiValueBool $ f || s

applyFunction HiFunDiv [_, (HiValueNumber 0)] = throwError HiErrorDivideByZero
applyFunction HiFunDiv [HiValueNumber f, HiValueNumber s] = return $ HiValueNumber $ f / s
applyFunction HiFunDiv [HiValueString f, HiValueString s] = return $
  HiValueString $ T.concat [f, T.pack "/", s]

applyFunction HiFunAdd [HiValueNumber f, HiValueNumber s] = return $
  HiValueNumber $ f + s
applyFunction HiFunAdd [HiValueString f, HiValueString s] = return $
  HiValueString $ T.concat [f, s]
applyFunction HiFunAdd [HiValueList left, HiValueList right] = return $
  HiValueList $ left S.>< right
applyFunction HiFunAdd [HiValueBytes left, HiValueBytes right] = return $
  HiValueBytes $ B.concat [left, right]
applyFunction HiFunAdd [HiValueTime time, HiValueNumber num] =
  return $
    HiValueTime $
      C.addUTCTime ((fromRational num) :: C.NominalDiffTime) time

applyFunction HiFunSub [HiValueNumber f, HiValueNumber s] = return $ HiValueNumber $ f - s
applyFunction HiFunSub [HiValueTime f, HiValueTime s] =
  return $
    HiValueNumber $
      toRational $ C.diffUTCTime f s

applyFunction HiFunMul [HiValueNumber f, HiValueNumber s] = return $ HiValueNumber $ f * s
applyFunction HiFunMul [HiValueString str, HiValueNumber num] = repeatBoxed str num
applyFunction HiFunMul [HiValueNumber num, HiValueString str] = repeatBoxed str num
applyFunction HiFunMul [HiValueList lst, HiValueNumber num] = repeatBoxed lst num
applyFunction HiFunMul [HiValueBytes bytes, HiValueNumber num] = repeatBoxed bytes num

applyFunction HiFunFold [HiValueFunction _, HiValueList x] | S.null x = return $ HiValueNull
applyFunction HiFunFold [HiValueFunction fun, HiValueList (x S.:<| xs)] =
  foldl' (foldValue fun) (return x) xs

applyFunction HiFunRange [HiValueNumber start, HiValueNumber end] = return $ range start end

applyFunction HiFunWrite [HiValueString file, HiValueString dat] = return $
  HiValueAction $ HiActionWrite (T.unpack file) (E.encodeUtf8 dat)

applyFunction HiFunWrite [HiValueString file, HiValueBytes dat] = return $
  HiValueAction $ HiActionWrite (T.unpack file) dat

applyFunction HiFunRand [HiValueNumber f, HiValueNumber s] = randFunToAction f s
--- unary
applyFunction HiFunNot [HiValueBool bool] = return $ HiValueBool $ not bool
applyFunction HiFunLength [HiValueString s] = return $
  HiValueNumber $ toRational $ T.length s
applyFunction HiFunLength [HiValueList lst] = return $
  HiValueNumber $ toRational $ length lst
applyFunction HiFunLength [HiValueBytes b]  = return $
  HiValueNumber $ toRational $ len b
applyFunction HiFunToUpper [HiValueString s] = return $ HiValueString $ T.toUpper s
applyFunction HiFunToLower [HiValueString s] = return $ HiValueString $ T.toLower s
applyFunction HiFunReverse [HiValueString s] = return $ HiValueString $ T.reverse s
applyFunction HiFunReverse [HiValueBytes s]  = return $ HiValueBytes $ B.reverse s
applyFunction HiFunReverse [HiValueList lst] = return $ HiValueList $ S.reverse lst
applyFunction HiFunTrim [HiValueString s] = return $ HiValueString $ T.strip s
applyFunction HiFunPackBytes [HiValueList lst] = seqToBytes lst
applyFunction HiFunUnpackBytes [HiValueBytes bytes] = bytesToList bytes
applyFunction HiFunEncodeUtf8 [HiValueString s] = return $ HiValueBytes $ E.encodeUtf8 s
applyFunction HiFunDecodeUtf8 [HiValueBytes bytes] = safeDecode bytes
applyFunction HiFunZip [HiValueBytes bytes] = compress bytes
applyFunction HiFunUnzip [HiValueBytes bytes] = decompress bytes
applyFunction HiFunSerialise [x] = return $ HiValueBytes $ LB.toStrict $ Serialise.serialise x
applyFunction HiFunDeserialise [HiValueBytes bytes] = return
  (Serialise.deserialise $ LB.fromStrict bytes :: HiValue)
applyFunction HiFunRead [HiValueString file] = return $
  HiValueAction $ HiActionRead $ T.unpack file
applyFunction HiFunMkDir [HiValueString dir] = return $
  HiValueAction $ HiActionMkDir $ T.unpack dir
applyFunction HiFunChDir [HiValueString dir] = return $
  HiValueAction $ HiActionChDir $ T.unpack dir
applyFunction HiFunParseTime [HiValueString str] = parseTime $ T.unpack str
applyFunction HiFunEcho [HiValueString text] = return $
  HiValueAction $ HiActionEcho text
applyFunction HiFunKeys [HiValueDict dict] = return $
  HiValueList $ S.fromList $ M.keys dict
applyFunction HiFunValues [HiValueDict dict] = return $
  HiValueList $ S.fromList $ M.elems dict
applyFunction HiFunCount [HiValueString text] = count text
applyFunction HiFunCount [HiValueList lst] = count lst
applyFunction HiFunCount [HiValueBytes b] = count b
applyFunction HiFunInvert [HiValueDict dict] = invert dict
--- other
applyFunction _ _ = throwError HiErrorInvalidArgument

-- | Implementation for lazy functions
lazyApply :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
lazyApply HiFunIf [b, f, s] = do
  val <- evalExpr b
  case val of
    HiValueBool bool -> if bool then (evalExpr f) else (evalExpr s)
    _                -> throwError HiErrorInvalidArgument
lazyApply HiFunAnd [f, s] = do
  f' <- evalExpr f
  case f' of
    HiValueNull       -> return $ f'
    HiValueBool False -> return $ f'
    _                 -> evalExpr s
lazyApply HiFunOr [f, s] = do
  f' <- evalExpr f
  case f' of
    HiValueNull       -> evalExpr s
    HiValueBool False -> evalExpr s
    _                 -> return $ f'
lazyApply _ _ = throwError HiErrorInvalidArgument

invert :: Monad m => M.Map HiValue HiValue -> ExceptT HiError m HiValue
invert dict =
  let mapList = map (\(a, b) -> (b, S.singleton a)) (M.assocs dict)
      mp = M.fromListWith (<>) mapList
   in return $ HiValueDict $ M.map (\s -> HiValueList s) mp

-- | Create a dictionary containing elements of text, bytestring or list with
-- their count
count :: (Monad m, HiArray a) => a -> ExceptT HiError m HiValue
count arr =
  let args = toMapList arr
      countMap = M.fromListWith (+) args
   in return $ HiValueDict $ M.map (\num -> HiValueNumber $ toRational num) countMap

-- | Take two integer numbers and return HiActionRand. If the given values are not
-- integers, return exception
randFunToAction :: Monad m => Rational -> Rational -> ExceptT HiError m HiValue
randFunToAction f s = do
  f' <- valueToInt f
  s' <- valueToInt s
  return $ HiValueAction $ HiActionRand f' s'

parseTime :: Monad m => String -> ExceptT HiError m HiValue
parseTime str =
  case (readMaybe str :: Maybe C.UTCTime) of
    Nothing   -> return $ HiValueNull
    Just time -> return $ HiValueTime time

-- | Function used to fold HiValues
foldValue :: HiMonad m
  => HiFun                      -- ^ function
  -> ExceptT HiError m HiValue  -- ^ accumulator
  -> HiValue                    -- ^ next HiValue
  -> ExceptT HiError m HiValue
foldValue fun acc y = do
  x <- acc
  applyFunction fun [x, y]

-- |  Return indexed elements from text, bytestring or list
slice :: (Monad m, HiArray a) => a -> [HiValue] -> ExceptT HiError m HiValue
slice arr [HiValueNumber num] = do
  pos <- valueToInt num
  let arrLength = len arr
  if (pos < 0) || (pos >= arrLength)
    then return HiValueNull
    else return $ index arr pos

slice _ [_] = throwError HiErrorInvalidArgument

slice arr [start', HiValueNull] = slice arr [start', HiValueNumber $ toRational (len arr)]

slice arr [HiValueNull, end'] = slice arr [HiValueNumber 0, end']

slice arr [HiValueNumber start', HiValueNumber end'] = do
  start <- valueToInt start'
  end <- valueToInt end'
  let getIndex pos = if pos < 0 then (len arr) + pos else pos
      s = getIndex start
      e = getIndex end
  return $ wrap $ substring s e arr

slice _ [_, _] = throwError HiErrorInvalidArgument

slice _ _ = throwError HiErrorArityMismatch

-- | Evaluate arguments and function and apply it
evalExprApply :: HiMonad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
evalExprApply op args = do
    op' <- evalExpr op
    case op' of
      HiValueFunction fun | fun `elem` lazyF -> lazyApply fun args
      _ -> do
        args' <- traverse evalExpr args
        evalFunction op' args'

-- | Continue if the given argument can be used as a function,
-- return HiErrorInvalidFunction otherwise
evalFunction :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
evalFunction val args = case val of
  HiValueFunction fun
    | (getArgsNumber fun == length args) || (getArgsNumber fun == -1) -> applyFunction fun args
    | otherwise -> throwError HiErrorArityMismatch
  HiValueString str -> slice str args
  HiValueList lst   -> slice lst args
  HiValueBytes b    -> slice b args
  HiValueDict dict  -> evalExprDict dict args
  _ -> throwError HiErrorInvalidFunction

-- | Evaluate any expression
evalExpr :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalExpr (HiExprValue x) = return x
evalExpr (HiExprApply op args) = evalExprApply op args
evalExpr (HiExprRun expr) = run expr
evalExpr (HiExprDict dict) = do
  entries <- traverse evalPair dict
  return $ HiValueDict $ M.fromList entries

-- | Evaluate elements in pair and return the pair of HiValue
evalPair :: HiMonad m => (HiExpr, HiExpr) -> ExceptT HiError m (HiValue, HiValue)
evalPair (a, b) = do
  a' <- evalExpr a
  b' <- evalExpr b
  return (a', b')

-- | Run HiValueAction
run :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
run expr = do
  val <- evalExpr expr
  case val of
    (HiValueAction action) -> (ExceptT . fmap Right . runAction) action
    _                      -> throwError HiErrorInvalidArgument

-- | Search the element in dictionary
evalExprDict :: HiMonad m
  => M.Map HiValue HiValue      -- ^ dictionary
  -> [HiValue]                  -- ^ list with one element
  -> ExceptT HiError m HiValue
evalExprDict dict [x] =
  case (M.lookup x dict) of
    Nothing  -> return $ HiValueNull
    Just val -> return val
evalExprDict _ _ = throwError HiErrorArityMismatch

-- | Return an integer number wrapped in HiValue within the specified range inclusive
range :: Rational -> Rational -> HiValue
range start end = HiValueList $ S.fromList $ map (\x -> HiValueNumber x) [start .. end]

substring :: (HiArray a) => Int -> Int -> a -> a
substring start end = takeN (end - start) . dropN start

-- | Convert HiValue to Word8. Returns HiErrorInvalidArgument if the given
-- value is not an integer number or it is not included in [0, 255]
valueToByte :: Monad m => HiValue -> ExceptT HiError m Word8
valueToByte (HiValueNumber val) = do
  x <- valueToInt val
  if (x < 0) || (x > 255)
    then throwError HiErrorInvalidArgument
    else return $ fromIntegral x
valueToByte _ = throwError HiErrorInvalidArgument

seqToBytes :: Monad m => S.Seq HiValue -> ExceptT HiError m HiValue
seqToBytes lst = do
  args <- traverse valueToByte lst
  return $ HiValueBytes $ B.pack (toList args)

bytesToList :: Monad m => B.ByteString -> ExceptT HiError m HiValue
bytesToList str = do
  let bytes = B.unpack str
      nums = map (\x -> HiValueNumber $ toRational x) bytes
  return $ HiValueList $ S.fromList nums

safeDecode :: Monad m => B.ByteString -> ExceptT HiError m HiValue
safeDecode bytes =
  case (E.decodeUtf8' bytes) of
    Right val -> return $ HiValueString $ val
    Left _    -> return $ HiValueNull

-- | Return an exception if the given number is not integer, int otherwise
valueToInt :: Monad m => Rational -> ExceptT HiError m Int
valueToInt num =
  let (res, remainder) = quotRem (numerator num) (denominator num)
   in if remainder /= 0
        then throwError HiErrorInvalidArgument
        else return $ fromIntegral res

-- | Implementation of stimes for text, bytestring and list.
-- The given number should be positive integer.
repeatBoxed :: (Monad m, HiArray a, Semigroup a) => a -> Rational -> ExceptT HiError m HiValue
repeatBoxed bytes num = do
  times <- valueToInt num
  if times <= 0
    then throwError HiErrorInvalidArgument
    else return $ wrap $ stimes times bytes

compress :: Monad m => B.ByteString -> ExceptT HiError m HiValue
compress bytes =
  return $
    HiValueBytes $
      LB.toStrict $
        Zlib.compressWith
          Zlib.defaultCompressParams
            { Zlib.compressLevel = Zlib.bestCompression
            }
          $ LB.fromStrict bytes

decompress :: Monad m => B.ByteString -> ExceptT HiError m HiValue
decompress bytes =
  return $
    HiValueBytes $ LB.toStrict $ Zlib.decompress $ LB.fromStrict bytes

-- | Generalisation for an ordered collection of elements
class HiArray a where
  wrap :: a -> HiValue
  len :: a -> Int
  index :: a -> Int -> HiValue
  takeN :: Int -> a -> a
  dropN :: Int -> a -> a
  toMapList :: a -> [(HiValue, Int)]

instance HiArray T.Text where
  wrap = HiValueString
  len = T.length
  index x pos = wrap $ T.singleton $ T.index x pos
  takeN = T.take
  dropN = T.drop
  toMapList text = map (\c -> (HiValueString $ T.singleton c, 1)) $ T.unpack text

instance HiArray B.ByteString where
  wrap = HiValueBytes
  len = B.length
  index x pos = HiValueNumber $ toRational $ B.index x pos
  takeN = B.take
  dropN = B.drop
  toMapList b = map (\c -> (HiValueNumber $ fromIntegral c, 1)) $ B.unpack b

instance HiArray (S.Seq HiValue) where
  wrap = HiValueList
  len = S.length
  index x pos = S.index x pos
  takeN = S.take
  dropN = S.drop
  toMapList s = map (,1) $ toList s
