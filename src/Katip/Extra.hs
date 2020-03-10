{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
module Katip.Extra
  ( module Katip
  , module Katip.Monadic
  , grabKatipEnv
  , runKatipEnv
  , withFileScribe
  , itemFormatterBuilder
  , jsonBuilder
  ) where

import           Control.Monad (unless)
import           Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as B
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.Encoding as LT
import           System.IO (hPutStrLn, openBinaryFile)
import           UnliftIO
    (BufferMode (..), Handle, IOMode (..), MVar, finally, hClose, hFlush,
    hSetBuffering, handleAny, modifyMVar_, newMVar, stderr, stdout, tryIO,
    withMVar)

import           Katip
import           Katip.Core
import           Katip.Monadic


grabKatipEnv
  :: KatipContext m
  => m KatipContextTState
grabKatipEnv =
  KatipContextTState
    <$> getLogEnv
    <*> getKatipContext
    <*> getKatipNamespace

runKatipEnv
  :: KatipContextTState
  -> KatipContextT m a
  -> m a
runKatipEnv st act =
  runReaderT (unKatipContextT act) st


type ItemBuilder a =
  LogItem a => Item a -> Verbosity -> B.Builder

withFileScribe
  :: (forall a . ItemBuilder a)
  -> PermitFunc
  -> Verbosity
  -> FilePath
  -> (Scribe -> IO () -> IO b)
  -> IO b
withFileScribe formatter permitF verb fname f = do
  mh <- openBinaryFile fname AppendMode >>= newMVar
  flip finally (withMVar mh hClose) $ do
    withMVar mh $ \h ->
      hSetBuffering h LineBuffering
    scribe <- mkHandleScribeWithBuilder formatter permitF verb mh
    f scribe (mkRotater mh)
  where
    mkRotater mv = modifyMVar_ mv $ \oh ->
      handleAny (fmap (const stderr) . onErr) $ do
      unless  (oh == stdout || oh == stderr) $
        () <$ tryIO (hClose oh)
      openBinaryFile fname AppendMode
    onErr e = hPutStrLn stderr $
      "Error opening log file " <> fname <> ": " <> show e
      <> "\nWill continue logging to stderr"

mkHandleScribeWithBuilder
  :: (forall a . ItemBuilder a)
  -> PermitFunc
  -> Verbosity
  -> MVar Handle
  -> IO Scribe
mkHandleScribeWithBuilder builder permitF verb mh =
  pure $ Scribe logger (withMVar mh hFlush) permitF
  where
    logger item = withMVar mh $ \h ->
      B.hPutBuilder h (builder item verb <> B.word8 10)

itemFormatterBuilder
  :: ItemFormatter a
  -> Item a
  -> Verbosity
  -> B.Builder
itemFormatterBuilder formatter item verb =
  LT.encodeUtf8Builder
  $ toLazyText
  $ formatter False verb item

jsonBuilder :: LogItem a => Item a -> Verbosity -> B.Builder
jsonBuilder item verb = A.fromEncoding . itemToEncoding $ preparedItem item
  where
    preparedItem Item{..} = Item{_itemPayload=jp,..}
      where
        jp = payloadObject verb _itemPayload

itemToEncoding :: A.ToJSON v => Item v -> A.Encoding
itemToEncoding Item{..} = A.pairs $ mconcat
      [ "app" A..= _itemApp
      , "env" A..= _itemEnv
      , "sev" A..= _itemSeverity
      , "thread" A..= getThreadIdText _itemThread
      , "host" A..= _itemHost
      , "pid" A..= ProcessIDJs _itemProcess
      , "data" A..= _itemPayload
      , "msg" A..= toLazyText (unLogStr _itemMessage)
      , "at" A..= _itemTime
      , "ns" A..= _itemNamespace
      , "loc" A..= fmap LocJs _itemLoc
      ]

