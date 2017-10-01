-- | Utility functions

module Pos.Wallet.Web.State.Util
    ( cleanupAcidStatePeriodically
    ) where

import           Universum                  hiding (over)

import           Data.Acid                  (createArchive, createCheckpoint)
import           Data.Time.Clock            (NominalDiffTime, addUTCTime, getCurrentTime)
import           Data.Time.Units            (TimeUnit)
import           Formatting                 (sformat, shown, (%))
import           Mockable                   (Delay, Mockable, delay)
import           Serokell.AcidState         (ExtendedState (..), extendedStateToAcid)
import           Serokell.Util              (sec)
import           System.Directory           (getModificationTime, listDirectory,
                                             removeFile)
import           System.FilePath            ((</>))
import           System.Wlog                (WithLogger, logDebug, logError)

import           Pos.Wallet.Web.State.State (MonadWalletWebDB, getWalletWebState)

type MonadAcidCleanup ctx m =
    ( MonadIO m
    , MonadMask m
    , WithLogger m
    , MonadWalletWebDB ctx m
    , Mockable Delay m
    )

cleanupAcidStatePeriodically
    :: (MonadAcidCleanup ctx m, TimeUnit t)
    => t -> m ()
cleanupAcidStatePeriodically interval = perform
  where
    perform = cleanupAction `catch` handler
    cleanupAction = forever $ do
        logDebug "Starting cleanup"
        est <- getWalletWebState
        let st = extendedStateToAcid est

        -- checkpoint/archive
        liftIO $ createCheckpoint st >> createArchive st
        logDebug "Created checkpoint/archived"

        -- cleanup old archive data
        let dbPathM = case est of
                         ESLocal _ p -> Just p
                         _           -> Nothing
        void $ flip catchAny (\e -> logError $ "Got error while cleaning up archive: " <> show e) $
            whenJust dbPathM $ \dbp -> do
                removed <- liftIO $ cleanupOld dbp
                logDebug $ "Removed " <> pretty removed <> " old archive files"

        delay interval
    handler (e :: SomeException) = do
        let report = do
                logError $ sformat ("acidCleanupWorker failed with error: "%shown%
                                    " restarting in 1m")
                                   e
                delay $ sec 60
        report `finally` perform

    -- Returns how many files were deleted
    cleanupOld :: FilePath -> IO Int
    cleanupOld dbPath = do
        let archiveDir = dbPath </> "Archive"
        archiveCheckpoints <- map (archiveDir </>) <$> listDirectory archiveDir
        withTimestamps <-
            mapM (\f -> (f,) <$> liftIO (getModificationTime f)) archiveCheckpoints
        curTime <- getCurrentTime
        -- 3 days
        let noOlderThan = addUTCTime (negate (3 * 24 * 3600) :: NominalDiffTime) curTime
        let oldFiles = filter ((< noOlderThan) . snd) withTimestamps
        forM_ oldFiles $ removeFile . fst
        pure $ length oldFiles

