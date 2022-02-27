{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia   #-}

module HW3.Action where

import Control.Exception.Base (Exception, throwIO)
import qualified Data.ByteString as B
import qualified Data.Sequence.Internal as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Time.Clock as C
import HW3.Base
import qualified System.Directory as D
import System.Random (randomRIO)

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Enum, Bounded)

data PermissionException
  = PermissionRequired HiPermission
  deriving (Show, Eq, Ord)

instance Exception PermissionException

newtype HIO a = HIO {runHIO :: Set.Set HiPermission -> IO a}
  deriving (Functor)

instance Applicative HIO where
  pure a = HIO $ \_ -> return a

  f <*> a = HIO $ \x ->
    do
      a' <- (runHIO a) x
      f' <- (runHIO f) x
      return $ f' a'

instance Monad HIO where
  a >>= f = HIO $ \x ->
    do
      a' <- (runHIO a) x
      runHIO (f a') x

instance HiMonad HIO where

  runAction HiActionCwd = checkPermission (Just AllowRead) $
    HiValueString . T.pack <$> D.getCurrentDirectory

  runAction (HiActionChDir path) =  checkPermission (Just AllowRead) $
    HiValueNull <$ D.setCurrentDirectory path

  runAction (HiActionMkDir path) = checkPermission (Just AllowWrite) $
    HiValueNull <$ D.createDirectory path

  runAction (HiActionWrite path bytes) = checkPermission (Just AllowWrite) $
    HiValueNull <$ B.writeFile path bytes

  runAction (HiActionRead path) = checkPermission (Just AllowRead) $ do
           isDirectory <- D.doesDirectoryExist path
           case isDirectory of
             False -> do
                 bytes <- B.readFile path
                 let decoded = E.decodeUtf8' bytes
                 case decoded of
                    Left _     -> return $ HiValueBytes bytes
                    Right text -> return $ HiValueString text
             True -> do
                 subdirs <- D.listDirectory path
                 let list = map (\str -> HiValueString $ T.pack str) subdirs
                 return $ HiValueList $ S.fromList list

  runAction HiActionNow = checkPermission (Just AllowTime) $
    HiValueTime <$> C.getCurrentTime

  runAction (HiActionRand from to) = checkPermission Nothing $
    HiValueNumber . toRational <$> randomRIO (from, to)

  runAction (HiActionEcho text) = checkPermission (Just AllowWrite) $
    HiValueNull <$ putStrLn (T.unpack text)

checkPermission :: Maybe HiPermission -> IO a -> HIO a
checkPermission perm action = HIO $ \permissions ->
  case perm of
    Nothing -> action
    Just p ->
      if not $ Set.member p permissions
        then throwIO $ PermissionRequired p
        else action
