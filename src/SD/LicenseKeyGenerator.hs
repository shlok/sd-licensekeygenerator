{-# LANGUAGE ScopedTypeVariables #-}

module SD.LicenseKeyGenerator
    ( generateLicenseKey
    ) where

import           Codec.Binary.Base32       (encode)
import           Control.Exception         (SomeException, handle)
import           Control.Monad             (guard)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (readFile)
import           Data.Text                 (Text, chunksOf, dropEnd,
                                               intercalate)
import qualified Data.Text                 as T (concat, length)
import           Data.Text.Encoding        (decodeUtf8)
import           OpenSSL                   (withOpenSSL)
import           OpenSSL.EVP.Digest        (getDigestByName)
import           OpenSSL.DER               (fromDERPriv)
import           OpenSSL.EVP.Sign          (signBS)
import           OpenSSL.RSA               (RSAKeyPair)

type UserName              = ByteString
type DERPrivateKeyFilePath = String
type LicenseKey            = Text

generateLicenseKey :: UserName -> DERPrivateKeyFilePath -> IO (Maybe LicenseKey)
generateLicenseKey name path = runMaybeT $ do
    signatureBS <- MaybeT $ signUserName name path
    license <- MaybeT . return $ signatureBSToLicenseKey signatureBS
    return license

signUserName :: UserName -> DERPrivateKeyFilePath -> IO (Maybe ByteString)
signUserName name path = withOpenSSL . runMaybeT $ do
    privKeyBS <- MaybeT $ privKeyFileToPrivKeyBS path
    rsaKeyPair <- MaybeT . return $ privKeyBSToRSAKeyPair privKeyBS
    digest <- MaybeT $ getDigestByName "sha1"
    signatureBS <- liftIO $ signBS digest rsaKeyPair name
    return signatureBS

signatureBSToLicenseKey :: ByteString -> Maybe Text
signatureBSToLicenseKey signature = do
    let base32Signature = (dropEnd 3) -- Remove last three === chars.
                              . decodeUtf8
                              . encode $ signature
    guard $ T.length base32Signature == 205
    let listOfLines = chunksOf 41 base32Signature
    let listOfChunkedLines = map (concatLastTwo . chunksOf 5) listOfLines
    let licenseKeyLines = map (intercalate "-") listOfChunkedLines
    let licenseKey = intercalate "\n" licenseKeyLines
    return licenseKey

concatLastTwo :: [Text] -> [Text]
concatLastTwo ts =
    let (beg, lastTwo) = splitAt (length ts - 2) ts
        in beg ++ [T.concat lastTwo]

privKeyBSToRSAKeyPair :: ByteString -> Maybe RSAKeyPair
privKeyBSToRSAKeyPair = fromDERPriv

privKeyFileToPrivKeyBS :: DERPrivateKeyFilePath -> IO (Maybe ByteString)
privKeyFileToPrivKeyBS path = do
    handle (\(_ :: SomeException) -> return Nothing) $ do
        bs <- B.readFile path
        return $ Just bs
