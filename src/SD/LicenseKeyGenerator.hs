{-# LANGUAGE ScopedTypeVariables #-}
module SD.LicenseKeyGenerator
    ( generateLicenseKey
    ) where

import qualified Codec.Binary.Base32       as B32
import qualified Control.Exception         as E
import qualified Control.Monad             as M
import qualified Control.Monad.IO.Class    as IOC
import qualified Control.Monad.Trans.Maybe as TM
import qualified Data.ByteString           as B
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified OpenSSL                   as O
import qualified OpenSSL.EVP.Digest        as ED
import qualified OpenSSL.DER               as D
import qualified OpenSSL.EVP.Sign          as S
import qualified OpenSSL.RSA               as R

type UserName              = B.ByteString
type DERPrivateKeyFilePath = String
type LicenseKey            = T.Text

generateLicenseKey :: UserName -> DERPrivateKeyFilePath -> IO (Maybe LicenseKey)
generateLicenseKey name path = TM.runMaybeT $ do
    signatureBS <- TM.MaybeT $ signUserName name path
    license <- TM.MaybeT . return $ signatureBSToLicenseKey signatureBS
    return license

signUserName :: UserName -> DERPrivateKeyFilePath -> IO (Maybe B.ByteString)
signUserName name path = O.withOpenSSL . TM.runMaybeT $ do
    privKeyBS <- TM.MaybeT $ privKeyFileToPrivKeyBS path
    rsaKeyPair <- TM.MaybeT . return $ privKeyBSToRSAKeyPair privKeyBS
    digest <- TM.MaybeT $ ED.getDigestByName "sha1"
    signatureBS <- IOC.liftIO $ S.signBS digest rsaKeyPair name
    return signatureBS

signatureBSToLicenseKey :: B.ByteString -> Maybe T.Text
signatureBSToLicenseKey signature = do
    let base32Signature = (T.dropEnd 3) -- Remove last three === chars.
                              . TE.decodeUtf8
                              . B32.encode $ signature
    M.guard $ T.length base32Signature == 205
    let listOfLines = T.chunksOf 41 base32Signature
    let listOfChunkedLines = map (concatLastTwo . T.chunksOf 5) listOfLines
    let licenseKeyLines = map (T.intercalate "-") listOfChunkedLines
    let licenseKey = T.intercalate "\n" licenseKeyLines
    return licenseKey

concatLastTwo :: [T.Text] -> [T.Text]
concatLastTwo ts =
    let (beg, lastTwo) = splitAt (length ts - 2) ts
        in beg ++ [T.concat lastTwo]

privKeyBSToRSAKeyPair :: B.ByteString -> Maybe R.RSAKeyPair
privKeyBSToRSAKeyPair = D.fromDERPriv

privKeyFileToPrivKeyBS :: DERPrivateKeyFilePath -> IO (Maybe B.ByteString)
privKeyFileToPrivKeyBS path = do
    E.handle (\(_ :: E.SomeException) -> return Nothing) $ do
        bs <- B.readFile path
        return $ Just bs
