{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Safe/secure logging

module Pos.Util.LogSafe
       ( SelectiveLogWrapped(..)
       , logDebugS
       , logInfoS
       , logNoticeS
       , logWarningS
       , logErrorS
       , logDebugP
       , logInfoP
       , logNoticeP
       , logWarningP
       , logErrorP
       , logDebugSP
       , logInfoSP
       , logNoticeSP
       , logWarningSP
       , logErrorSP

       , secureF

       , NonSensitive (..)
       , buildNonSensitiveUnsafe
       , buildNonSensitiveMaybe
       ) where

import           Universum

import           Control.Monad.Trans    (MonadTrans)
import           Data.List              (isSuffixOf)
import           Data.Reflection        (Reifies (..), reify)
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (bprint, build, sformat)
import           Formatting.Internal    (Format (..))
import           System.Wlog            (CanLog (..), HasLoggerName (..), Severity (..),
                                         loggerName)
import           System.Wlog.Handler    (LogHandlerTag (HandlerFilelike))
import           System.Wlog.Logger     (logMCond)

import           Pos.Core.Types         (Coin)
import           Pos.Crypto             (PassPhrase)


----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

newtype SelectiveLogWrapped s m a = SelectiveLogWrapped
    { getSecureLogWrapped :: m a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (SelectiveLogWrapped s) where
    lift = SelectiveLogWrapped

type SelectionMode = LogHandlerTag -> Bool

selectPublicLogs :: SelectionMode
selectPublicLogs = \case
    HandlerFilelike p -> ".pub" `isSuffixOf` p
    _ -> False

selectSecretLogs :: SelectionMode
selectSecretLogs = not . selectPublicLogs

instance (MonadIO m, Reifies s SelectionMode) =>
         CanLog (SelectiveLogWrapped s m) where
    dispatchMessage (loggerName -> name) severity msg =
        liftIO $ logMCond name severity msg (reflect (Proxy @s))

instance (HasLoggerName m) => HasLoggerName (SelectiveLogWrapped s m) where
    getLoggerName = SelectiveLogWrapped getLoggerName
    modifyLoggerName foo (SelectiveLogWrapped m) =
        SelectiveLogWrapped (modifyLoggerName foo m)

execSecureLogWrapped :: Proxy s -> SelectiveLogWrapped s m a -> m a
execSecureLogWrapped _ (SelectiveLogWrapped act) = act

-- | Shortcut for 'logMessage' to use according severity.
logDebugS, logInfoS, logNoticeS, logWarningS, logErrorS
    :: (HasLoggerName m, MonadIO m)
    => Text -> m ()
logDebugS   = logMessageS Debug
logInfoS    = logMessageS Info
logNoticeS  = logMessageS Notice
logWarningS = logMessageS Warning
logErrorS   = logMessageS Error

-- | Same as 'logMesssage', but log to two loggers, put only insecure
-- version to memmode.
logMessageS
    :: (HasLoggerName m, MonadIO m)
    => Severity
    -> Text
    -> m ()
logMessageS severity t =
    reify selectSecretLogs $ \s ->
    execSecureLogWrapped s $ do
        name <- getLoggerName
        dispatchMessage name severity t

----------------------------------------------------------------------------
-- Non sensitive buildables
----------------------------------------------------------------------------

data SecLogLevel
data PubLogLevel

data AltLogLevel s where
    AltSecLogLevel :: AltLogLevel SecLogLevel
    AltPubLogLevel :: AltLogLevel PubLogLevel

data ReflectedAltLogLevel rs


data SecureLog s a = SecureLog
    { getSecureLog :: a
    } deriving (Eq, Ord)

type SecretLog = SecureLog SecLogLevel
type PublicLog = SecureLog PubLogLevel

coerce :: forall s2 s1 a. SecureLog s1 a -> SecureLog s2 a
coerce = SecureLog . getSecureLog

instance Buildable a => Buildable (SecretLog a) where
    build = bprint build . getSecureLog

instance ( Buildable a
         , Buildable (PublicLog a)
         , Reifies rs (AltLogLevel s)
         ) =>
         Buildable (SecureLog (ReflectedAltLogLevel rs) a) where
    build = case reflect (Proxy @rs) of
        AltSecLogLevel -> bprint build . coerce @SecLogLevel
        AltPubLogLevel -> bprint build . coerce @PubLogLevel

instance IsString s => IsString (SecureLog __ s) where
    fromString = SecureLog . fromString

secureF :: Proxy s -> Format r (SecureLog s a -> r) -> Format r (a -> r)
secureF _ (Format f) = Format $ \toRes -> f toRes . SecureLog

-- | Same as 'logMesssage', put to public logs only.
logMessageP
    :: (HasLoggerName m, MonadIO m)
    => Severity
    -> (Proxy PubLogLevel -> Text)
    -> m ()
logMessageP severity t =
    reify selectPublicLogs $ \s ->
    execSecureLogWrapped s $ do
        name <- getLoggerName
        dispatchMessage name severity (t Proxy)

-- | Shortcut for 'logMessage' to use according severity.
logDebugP, logInfoP, logNoticeP, logWarningP, logErrorP
    :: (HasLoggerName m, MonadIO m)
    => (Proxy PubLogLevel -> Text) -> m ()
logDebugP   = logMessageP Debug
logInfoP    = logMessageP Info
logNoticeP  = logMessageP Notice
logWarningP = logMessageP Warning
logErrorP   = logMessageP Error

-- | Same as 'logMesssage', put to public logs.
logMessageSP
    :: (HasLoggerName m, MonadIO m)
    => Severity
    -> (forall rs s. Reifies rs (AltLogLevel s) => Proxy (ReflectedAltLogLevel rs) -> Text)
    -> m ()
logMessageSP severity t = do
    reify AltSecLogLevel $ \(Proxy :: Proxy sl) ->
        logMessageS severity (t $ Proxy @(ReflectedAltLogLevel sl))
    reify AltPubLogLevel $ \(Proxy :: Proxy sl) ->
        logMessageP severity $ \_ -> t (Proxy @(ReflectedAltLogLevel sl))

-- | Shortcut for 'logMessage' to use according severity.
logDebugSP, logInfoSP, logNoticeSP, logWarningSP, logErrorSP
    :: (HasLoggerName m, MonadIO m)
    => (forall rs s. Reifies rs (AltLogLevel s) =>
                       Proxy (ReflectedAltLogLevel rs) -> Text)
    -> m ()
logDebugSP   = logMessageSP Debug
logInfoSP    = logMessageSP Info
logNoticeSP  = logMessageSP Notice
logWarningSP = logMessageSP Warning
logErrorSP   = logMessageSP Error


-- | Makes any instance of printing typeclass (e.g. 'Buildable') produce
-- text without sensitive info.
newtype NonSensitive a = NonSensitive
    { getNonSensitive :: a
    } deriving (Eq, Ord)

instance Buildable (NonSensitive Text) where
    build (NonSensitive t) = bprint build t

-- | Useful when there's nothing to hide.
buildNonSensitiveUnsafe
    :: Buildable a
    => NonSensitive a -> Builder
buildNonSensitiveUnsafe (NonSensitive a) = bprint build a

-- | For some types saying whether they are specified or not may be unsafe.
-- But in cases when it isn't, you can use this function to define
-- 'instance Buildable (NonSensitive (Maybe t))'.
buildNonSensitiveMaybe
    :: Buildable (NonSensitive a)
    => NonSensitive (Maybe a) -> Builder
buildNonSensitiveMaybe (NonSensitive m) =
    maybe "" (bprint build . NonSensitive) m


instance Buildable (NonSensitive PassPhrase) where
    build = buildNonSensitiveUnsafe  -- passphrase is indeed hiden by default

-- maybe I'm wrong here, but currently masking it important for wallet servant logs
instance Buildable (NonSensitive Coin) where
    build _ = "? coin(s)"
