{-# LANGUAGE TypeFamilies #-}

-- | Various small endpoints

module Pos.Wallet.Web.Methods.Misc
       ( getUserProfile
       , updateUserProfile

       , isValidAddress

       , nextUpdate
       , postponeUpdate
       , applyUpdate

       , syncProgress

       , testResetAll
       ) where

import           Universum

import           Pos.Aeson.ClientTypes      ()
import           Pos.Core                   (decodeTextAddress)
import           Pos.Util                   (maybeThrow)

import           Pos.Wallet.KeyStorage      (MonadKeys, deleteSecretKey, getSecretKeys)
import           Pos.Wallet.WalletMode      (applyLastUpdate, connectedPeers,
                                             localChainDifficulty, networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes (CProfile (..), CUpdateInfo (..),
                                             SyncProgress (..))
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)
import           Pos.Wallet.Web.State       (getNextUpdate, getProfile, removeNextUpdate,
                                             setProfile, testReset)


----------------------------------------------------------------------------
-- Profile
----------------------------------------------------------------------------

getUserProfile :: MonadWalletWebMode ctx m => m CProfile
getUserProfile = getProfile

updateUserProfile :: MonadWalletWebMode ctx m => CProfile -> m CProfile
updateUserProfile profile = setProfile profile >> getUserProfile

----------------------------------------------------------------------------
-- Address
----------------------------------------------------------------------------

-- NOTE: later we will have `isValidAddress :: CId -> m Bool` which should work for arbitrary crypto
isValidAddress :: MonadWalletWebMode ctx m => Text -> m Bool
isValidAddress sAddr =
    pure . isRight $ decodeTextAddress sAddr

----------------------------------------------------------------------------
-- Updates
----------------------------------------------------------------------------

-- | Get last update info
nextUpdate :: MonadWalletWebMode ctx m => m CUpdateInfo
nextUpdate = getNextUpdate >>=
             maybeThrow (RequestError "No updates available")

-- | Postpone next update after restart
postponeUpdate :: MonadWalletWebMode ctx m => m ()
postponeUpdate = removeNextUpdate

-- | Delete next update info and restart immediately
applyUpdate :: MonadWalletWebMode ctx m => m ()
applyUpdate = removeNextUpdate >> applyLastUpdate

----------------------------------------------------------------------------
-- Sync progress
----------------------------------------------------------------------------

syncProgress :: MonadWalletWebMode ctx m => m SyncProgress
syncProgress =
    SyncProgress
    <$> localChainDifficulty
    <*> networkChainDifficulty
    <*> connectedPeers

----------------------------------------------------------------------------
-- Reset
----------------------------------------------------------------------------

testResetAll :: MonadWalletWebMode ctx m => m ()
testResetAll = deleteAllKeys >> testReset
  where
    deleteAllKeys :: MonadKeys m => m ()
    deleteAllKeys = do
        keyNum <- length <$> getSecretKeys
        replicateM_ keyNum $ deleteSecretKey 0
