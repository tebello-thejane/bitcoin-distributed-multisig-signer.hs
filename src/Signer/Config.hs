{-# language StrictData #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module Signer.Config where

import Data.Aeson
import System.Posix.User
import Control.Monad (when, unless)
import System.Posix.Files
import Network.Haskoin.Script (RedeemScript)
import Network.Haskoin.Crypto (PrvKey, Address(..), fromWif)
import Data.Yaml (decodeFile)
import Data.String.Conv
import Data.String (fromString)
import Safe (fromJustNote)

data Config = Config {
  privateKey       :: PrvKey,
  redeemScript     :: RedeemScript,
  addressWhiteList :: [Address]
} deriving Show

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    prvWif <- o .: "private-key"
    rdm <- o .: "redeem-script"
    whitelist <- fmap fromString <$> o .: "address-whitelist"
    return $ Config (fromJustNote "Error parsing private key as WIF." $ fromWif $ toS @String prvWif) rdm whitelist

getConfig :: FilePath -> IO Config
getConfig configFileName =
  fmap  (fromJustNote "Error decoding config file as YAML.") (decodeFile configFileName)

checkSecurity :: Bool -> FilePath -> IO ()
checkSecurity False configFileName = do
  uid <- getRealUserID
  when (uid == 0) $
    errorWithoutStackTrace "ERROR: This signer should not be run under ROOT user."

  configExists <- fileExist configFileName
  unless configExists $
    errorWithoutStackTrace "ERROR: Cannot find config file in current directory."

  configStatus <- getFileStatus configFileName
  unless (uid == fileOwner configStatus) $
    errorWithoutStackTrace "ERROR: Current user must own config file."

  let badMode = unionFileModes groupModes otherModes

  unless (intersectFileModes badMode (fileMode configStatus) == nullFileMode) $
    errorWithoutStackTrace "ERROR: Config file permissions are too permissive."
checkSecurity True configFileName = do
  uid <- getRealUserID
  when (uid == 0) $
    putStrLn "WARNING: This signer should not be run under ROOT user. Ignoring since we are in testnet mode."

  configExists <- fileExist configFileName
  unless configExists $
    errorWithoutStackTrace "ERROR: Cannot find config file in current directory."

  configStatus <- getFileStatus configFileName
  unless (uid == fileOwner configStatus) $
    putStrLn "WARNING: Current user must own config file. Ignoring since we are in testnet mode."

  let badMode = unionFileModes groupModes otherModes

  unless (intersectFileModes badMode (fileMode configStatus) == nullFileMode) $
    putStrLn "WARNING: Config file permissions are too permissive. Ignoring since we are in testnet mode."
