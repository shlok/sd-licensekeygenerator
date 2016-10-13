**Note**: [shlok](https://github.com/shlok) created this for use in personal
projects. Use at your own risk.

Exports the module `SD.LicenseKeyGenerator`, which exports one function:
`generateLicenseKey`.

    import Data.ByteString
    import Data.Text
    type UserName              = ByteString
    type DERPrivateKeyFilePath = String
    type LicenseKey            = Text
    generateLicenseKey :: UserName -> DERPrivateKeyFilePath
                              -> IO (Maybe LicenseKey)

Given a user name (i.e., the name of the license key holder, e.g., “Steve
Jobs”) and a full path to a 1024-bit RSA private key file in DER format, this
function produces a license key. The license key looks something like this:

    CYC34-3MTNT-IENDK-22RUB-Q7VZ6-TSTHR-BYTWT-7MH442
    WRYRE-QULC5-OM44P-3AU3Q-YEGRH-XETR6-JRSPO-NMKQVW
    SDRHR-4W7AQ-QXAV6-XBVEX-YDXFG-UQSXT-MJ3H4-LAG44A
    D4TXI-FBYGE-C2BFJ-QXRZ3-3MCIF-EOJVA-GO6T5-SPLMP6
    3NQJG-BM7SO-EOXWR-ODOHX-HI4G3-YLTPM-P5JOF-5VEOHC

(In more detail, this is what is happening: (1) Code equivalent to the command
`echo -n "Steve Jobs" | openssl dgst -sha1 -sign privkey.der -keyform DER` is
run to produce the signature. (2) The signature is Base32-encoded. (3) The
final “===” characters are removed. (4) Dashes and newlines are added, as
shown above.)

**Note:** `generateLicenseKey` does *not* validate `UserName` in any way; it
accepts any `ByteString`. It is up to callers to perform validations (e.g.,
to make sure the name is not empty or to perform unicode normalization).

[shlok](https://github.com/shlok) uses these types of license keys in the
non-Mac App Store version of his
[Scrambler](https://codingturtle.com/scrambler) application.

This repository was created as a part of [shlok](https://github.com/shlok)’s
efforts in 2016 to migrate all of his websites and web apps to be powered by the
[Snap framework](http://snapframework.com).
