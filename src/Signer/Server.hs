{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language StrictData #-}
{-# language OverloadedStrings #-}
{-# language MultiParamTypeClasses #-}

module Signer.Server where

import Servant
import Control.Monad (when, unless, forM_)
import Network.Haskoin.Script (ScriptOutput(..), decodeOutputBS, SigHash(..))
import Network.Haskoin.Crypto (Address(..))
import Network.Haskoin.Transaction (Tx, txIn, txOut, scriptOutput, SigInput(..), TxIn(..), signTx)
import Data.String.Conv
import Data.Semigroup ((<>))
import Data.Either (isRight)
import Data.ByteString as BS (length)
import qualified Data.Serialize as Serialise (encode, decode)
import qualified Data.ByteString.Base16 as Base16

import Signer.Config

newtype MTx = MTx {unMTx :: Tx}

instance MimeUnrender PlainText MTx where
  mimeUnrender _ bs =
    if BS.length bad > 0
    then
      Left "Received Tx payload was not encoded in proper hex."
    else
      MTx <$> Serialise.decode unHex
    where
      (unHex, bad) = Base16.decode $ toS bs

type API = "sign" :> ReqBody '[PlainText] MTx :> Post '[PlainText] String

server :: Address -> ScriptOutput -> Config -> Server API
server scrAddr scro (Config prv scr wl) (MTx tx) = do
  forM_ (txOut tx) $ \txo -> do
    let dso = decodeOutputBS $ scriptOutput txo
    unless (isRight dso) $ throwError $ err400 {errBody = "Bad Tx script output."}
    let (Right so) = dso
    case so of
      PayPKHash adr ->
        when (adr `notElem` scrAddr:wl) $ throwError
          $ err403 {errBody = "Address " <> toS (show adr) <> " is not in the destination list."}
      PayScriptHash adr ->
        when (adr `notElem` scrAddr:wl) $ throwError
          $ err403 {errBody = "Address " <> toS (show adr) <> " is not in the destination list."}
      _ -> throwError $ err400 {errBody = "Bad script output type (not an address): " <> toS (show so)}
  let sis = fmap (\(TxIn op _ _) -> SigInput scro op (SigAll False) (Just scr)) (txIn tx)

  let signed = signTx tx sis [prv]
  case signed of
    Right t -> return $ toS $ Base16.encode $ Serialise.encode t
    Left e  -> throwError $ err400 {errBody = "Error signing transaction: " <> toS e}
