{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language StrictData #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language MultiParamTypeClasses #-}

module Main where

import Servant
import Data.Aeson
import System.Posix.User
import Control.Monad (when, unless, forM_)
import System.Posix.Files
import Network.Haskoin.Script (RedeemScript, ScriptOutput(..), decodeOutputBS, SigHash(..), scriptAddr)
import Network.Haskoin.Crypto (PrvKey, Address(..), fromWif)
import Network.Haskoin.Internals (switchToTestnet3)
import Network.Haskoin.Transaction (Tx, txIn, txOut, scriptOutput, SigInput(..), TxIn(..), signTx)
import Data.Yaml (decodeFile)
import Data.String.Conv
import Data.String (fromString)
import Safe (fromJustNote)
import Options.Applicative
import Data.Semigroup ((<>))
import Network.Wai.Handler.Warp
import Data.Either (isRight)
import Data.ByteString as BS (length)
import qualified Data.Serialize as Serialise (encode, decode)
import qualified Data.ByteString.Base16 as Base16
import Network.Wai.Middleware.RequestLogger

checkSecurity :: Bool -> FilePath -> IO ()
checkSecurity False configFileName = do
  uid <- getRealUserID
  when (uid == 0) $
    error "ERROR: This signer should not be run under ROOT user."

  configExists <- fileExist configFileName
  unless configExists $
    error "ERROR: Cannot find config file in current directory."

  configStatus <- getFileStatus configFileName
  unless (uid == fileOwner configStatus) $
    error "ERROR: Current user must own config file."

  let badMode = unionFileModes groupModes otherModes

  unless (intersectFileModes badMode (fileMode configStatus) == nullFileMode) $
    error "ERROR: Config file permissions are too permissive."
checkSecurity True configFileName = do
  uid <- getRealUserID
  when (uid == 0) $
    putStrLn "WARNING: This signer should not be run under ROOT user. Ignoring since we are in testnet mode."

  configExists <- fileExist configFileName
  unless configExists $
    error "ERROR: Cannot find config file in current directory."

  configStatus <- getFileStatus configFileName
  unless (uid == fileOwner configStatus) $
    putStrLn "WARNING: Current user must own config file. Ignoring since we are in testnet mode."

  let badMode = unionFileModes groupModes otherModes

  unless (intersectFileModes badMode (fileMode configStatus) == nullFileMode) $
    error "WARNING: Config file permissions are too permissive. Ignoring since we are in testnet mode."

data CmdOptions = CmdOptions {
  port       :: Int,
  testnet    :: Bool,
  configFile :: FilePath
}

cmdOptionsParser :: ParserInfo CmdOptions
cmdOptionsParser =
  info
    (CmdOptions
      <$> option auto
        ( long "port"
      <> short 'p'
      <> metavar "PORT#"
      <> help "Port number to listen on"
      <> showDefault
      <> value 8080
        )
      <*> switch
        ( long "testnet"
      <> short 't'
      <> help "Testnet mode"
        )
      <*> strOption
        ( long "config"
      <> short 'c'
      <> metavar "FILE"
      <> help "Config file location"
      <> showDefault
      <> value "./multisigner.yaml"
        ) <**> helper)
    fullDesc

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
    return $ Config (fromJustNote "WIF" $ fromWif $ strConv @String Strict prvWif) rdm whitelist

getConfig :: FilePath -> IO Config
getConfig configFileName =
  fmap  (fromJustNote "Error decoding config file as YAML.") (decodeFile configFileName)

main :: IO ()
main = do

  options <- execParser cmdOptionsParser

  when (testnet options)
    switchToTestnet3

  cfg <- getConfig (configFile options)

  run (port options) $ logStdoutDev $ serve (Proxy @API) $ server cfg

instance MimeUnrender PlainText Tx where
  mimeUnrender _ bs =
    if BS.length bad > 0
    then
      Left "Received Tx payload was not encoded in proper hex."
    else
      Serialise.decode unHex
    where
      (unHex, bad) = Base16.decode $ strConv Strict bs

type API = "sign" :> ReqBody '[PlainText] Tx :> Post '[JSON] String

server :: Config -> Server API
server (Config prv scr wl) tx = do
  let scrAddr = scriptAddr scr
  let scro = PayScriptHash scrAddr
  forM_ (txOut tx) $ \txo -> do
    let dso = decodeOutputBS $ scriptOutput txo
    unless (isRight dso) $ throwError $ err400 {errBody = "Bad Tx script output."}
    let (Right so) = dso
    case so of
      PayPKHash adr ->
        when (adr `notElem` scrAddr:wl) $ throwError $ err400 {errBody = "Address " <> strConv Strict (show adr) <> " is not in the destination list."}
      PayScriptHash adr ->
        when (adr `notElem` scrAddr:wl) $ throwError $ err400 {errBody = "Address " <> strConv Strict (show adr) <> " is not in the destination list."}
      _ -> throwError $ err400 {errBody = "Bad script output type (not an address): " <> strConv Strict (show so)}
  let sis = fmap (\(TxIn op _ _) -> SigInput scro op (SigAll False) (Just scr)) (txIn tx)

  let signed = signTx tx sis [prv]
  case signed of
    Right t -> return $ strConv Strict $ Base16.encode $ Serialise.encode t
    Left e   -> throwError $ err400 {errBody = "Error signing transaction: " <> strConv Strict e}
