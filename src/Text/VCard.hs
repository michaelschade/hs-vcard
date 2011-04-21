module Text.VCard
    ( -- $doc
      VCard(..)
    , VCardProperty(..)
    , AddrType(..)
    , TelType(..)
    , EmailType(..)
    , AgentData(..)
    , Data(..)
    , Class(..)
    ) where

import Data.List (intercalate)
import Data.Time (UTCTime, TimeZone, FormatTime, formatTime)
import System.Locale (defaultTimeLocale)

-- | Calling @show@ on @VCard@ will output a RFC 2426-compliant VCard that
-- can, for example, be easily saved to a file and imported by any supporting
-- program.
data VCard      = VCard [VCardProperty]

data VCardProperty =
    -- | Common name of the represented person. E.g.,
    --
    -- > CommonName "Mr. Michael A. F. Schade"
      CommonName        String
    -- | A list of values corresponding, in sequence, to Family Name, Given
    -- Name, Additional Names, Honorific Prefixes, and Honorific Suffixes. E.g.,
    --
    -- > IndividualNames ["Schade", "Michael", "Anthony", "Fanetti", "Mr."]
    | IndividualNames   [String]
    -- | A list of nicknames belonging to the VCard entity. E.g.,
    --
    -- > Nickname ["Mike", "Mikey"]
    | Nickname          [String]
    -- | A photo of the VCard entity. E.g.,
    --
    -- > Photo (URI "http://accentuate.us/smedia/images/michael.jpg")
    | Photo             { phtType   :: Maybe String -- ^ Registered IANA format
                        , phtData   :: Data
                        }
    -- | Specifies the birth date of the VCard entity. E.g.,
    --
    -- > Birthday $ UTCTime (fromGregorian 1991 10 14) (secondsToDiffTime 0)
    | Birthday          UTCTime
    -- | A physical address associated with the vCard entity. E.g.,
    --
    -- > Address [AddrParcel, AddrPostal] "PO Box 935" "" "" "Fenton" "MO"
    -- >                                  "63026" "USA"
    | Address           { addrType      :: [AddrType]
                        , poBox         :: String
                        , extAddress    :: String
                        , streetAddress :: String
                        , locality      :: String -- ^ City
                        , region        :: String -- ^ State or Province
                        , postalCode    :: String
                        , countryName   :: String
                        }
    -- | Formatted text about the delivery address. This is typically similar
    -- to the information in Address. E.g.,
    --
    -- > Label  [AddrParcel, AddrPostal]
    -- >        ["Michael Schade", "PO Box 935", "Fenton, MO 63026"]
    | Label             { lblType   :: [AddrType]
                        , label     :: [String] -- ^ Will be newline separated
                        }
    -- | A telephone number for the VCard entity, as well as a list of
    -- properties describing the telephone number. E.g.,
    --
    -- > Telephone [TelCell, TelPreferred] "+1-555-555-5555"
    | Telephone         { telType   :: [TelType]
                        , number    :: String
                        }
    -- | An email address for the VCard entity, including a list of properties
    -- describing it. E.g.,
    --
    -- > Email [EmailInternet, EmailPreferred] "hackage@mschade.me"
    | Email             { emailType :: [EmailType]
                        , email     :: String
                        }
    -- | Specifies the mailing agent the vCard entity uses. E.g.,
    --
    -- > Mailer "MichaelMail 4.2" -- Not a real mailing agent, unfortunately :(
    | Mailer            String
    -- | Represents the time zone of the vCard entity. E.g.,
    --
    -- > TZ (hoursToTimeZone (-6))
    | TZ                TimeZone
    -- | Relates to the global positioning of the vCard entity. The value is
    -- (latitude, longitude) and must be specified as decimal degrees,
    -- preferably to six decimal places.
    --
    -- > Geo (37.386013, -122.082932)
    | Geo               (Double, Double)
    -- | The VCard entity's job title or other position. E.g.,
    --
    -- > Title "Co-Founder"
    | Title             String
    -- | Specifies the role associated with the title. E.g.,
    --
    -- > Role "Anything"   -- For the co-founder, or
    -- > Role "Programmer" -- For someone the title "Research and Development"
    | Role              String
    -- | An image of the vCard entity's logo. This would typically relate to
    -- their organization. E.g.,
    --
    -- > Logo (URI "http://spearheaddev.com/smedia/images/logo-trans.png")
    | Logo              { lgoType   :: Maybe String -- ^ Registered IANA format
                        , lgoData   :: Data
                        }
    -- | Indicates the vCard of an assistant or area administrator who is
    -- typically separately addressable. E.g.,
    --
    -- > Agent (AgentURI "CID:JQPUBLIC.part3.960129T083020.xyzMail@host3.com")
    --
    -- or
    --
    -- > Agent (AgentVCard (VCard   [ CommonName "James Q. Helpful"
    -- >                            , Email [EmailInternet] "j@spearheaddev.com"
    -- >                            ]))
    | Agent             AgentData
    -- | The organization to which an entity belongs followed by organizational
    -- unit names. E.g.,
    --
    -- > Organization ["Spearhead Development, L.L.C.", "Executive"]
    | Organization      [String]
    -- | General categories to describe the vCard entity. E.g.,
    --
    -- > Categories ["Internet", "Web Services", "Programmers"]
    | Categories        [String]
    -- | A general note about the vCard entity. E.g.,
    --
    -- > Note "Email is the absolute best contact method."
    | Note              String
    -- | Specifies the identifier of the product that created this vCard. E.g.,
    --
    -- > ProductId "-//ONLINE DIRECTORY//NONSGML Version 1//EN"
    --
    -- Please note well that, by RFC 2426 guidelines, \"implementations SHOULD
    -- use a method such as that specified for Formal Public Identifiers in ISO
    -- 9070 to assure that the text value is unique,\" but this module does not
    -- support that.
    | ProductId         String
    -- | Distinguishes the current revision from other renditions. E.g.,
    --
    -- > Revision $ UTCTime (fromGregorian 2011 04 16) (secondsToDiffTime 0)
    | Revision          UTCTime
    -- | Provides a locale- or national-language-specific formatting of the
    -- formatted name based on the vCard entity's family or given name. E.g.,
    --
    -- > SortString "Schade"
    | SortString        String
    -- | Specifies information in a digital sound format to annotate some
    -- aspect of the vCard. This is typically for the proper pronunciation of the
    -- vCard entity's name. E.g.,
    --
    -- > Sound  "BASIC"
    -- >        (URI "CID:JOHNQPUBLIC.part8.19960229T080000.xyzMail@host1.com")
    | Sound             { sndType   :: Maybe String -- ^ Registered IANA format
                        , sndData   :: Data
                        }
    -- | A value to uniquely identify the vCard. Please note well that this
    -- should be one of the registered IANA formats, but as of this time, this
    -- module does not support listing the UID type. E.g.,
    --
    -- > UID "19950401-080045-40000F192713-0052"
    | UID               String
    -- | A website associated with the vCard entity. E.g.,
    --
    -- > URL "http://spearheaddev.com/"
    | URL               String
    -- | Describes the general intention of the vCard owner as to how
    -- accessible the included information should be. E.g.,
    --
    -- > Class ClassConfidential
    | Class             Class
    -- | Specifies a public key or authentication certificate associated with
    -- the vCard entity. E.g.,
    --
    -- > Key "x509" (Binary "dGhpcyBjb3VsZCBiZSAKbXkgY2VydGlmaWNhdGUK")
    | Key               { keyType   :: Maybe String -- ^ Registered IANA format
                        , keyData   :: Data
                        }

-- | Represents the various types or properties of an address.
data AddrType   = AddrDomestic | AddrInternational | AddrPostal | AddrParcel
                | AddrHome | AddrWork | AddrPreferred

-- | Represents the various types or properties of a telephone number.
data TelType    = TelHome | TelMessage | TelWork | TelVoice | TelFax | TelCell
                | TelVideo | TelPager | TelBBS | TelModem | TelCar | TelISDN
                | TelPCS | TelPreferred

-- | Represents the various types or properties of an email address.
data EmailType  = EmailInternet | EmailX400 | EmailPreferred

-- | Represents the data associated with a vCard's Agent. This could be a URI
-- to such a vCard or the embedded contents of the vCard itself.
data AgentData  = AgentURI String | AgentVCard VCard

-- | Represents the various types of data that can be included in a vCard.
data Data = URI String | Binary String

-- | Classifies the vCard's intended access level.
data Class      = ClassPublic | ClassPrivate | ClassConfidential

instance Show VCard where
    show (VCard vps) = intercalate "\n"
        ["BEGIN:vCard","VERSION:3.0", vps', "END:vCard"]
        where vps' = intercalate "\n" . map show $ vps

instance Show VCardProperty where
    show (CommonName fn)    = "FN:" ++ escape fn
    show (Nickname nn)      = "NICKNAME:" ++ (intercalate "," . map escape) nn
    show (Photo t d)        = "PHOTO" ++ showType t ++ show d
    show (Birthday bd)      = "BDAY:" ++ fmtTime "%Y-%m-%d" bd
    show (Address ts po e s l r ps c) = "ADR;" ++ ts' ++ vps
        where   ts' = showTypes . map show $ ts
                vps = (intercalate ";" . map escape) [po, e, s, l, r, ps, c]
    show (Label ts l)       = "LABEL;" ++ ts' ++ (intercalate "\n" . map escape) l
        where ts' = showTypes . map show $ ts
    show (Telephone ts n)   = "TEL;" ++ ts' ++ escape n
        where ts' = showTypes . map show $ ts
    show (Email ts e)       = "EMAIL;" ++ ts' ++ escape e
        where ts' = showTypes . map show $ ts
    show (Mailer m)         = "MAILER:" ++ escape m
    show (TZ time)          = "TZ:" ++ tz'
        where   (h,m) = splitAt 3 (show time)
                tz'   = h ++ ":" ++ m
    show (Geo (lat,lon))    = "GEO:" ++ intercalate ";" [show lat, show lon]
    show (Title t)          = "TITLE:" ++ escape t
    show (Role r)           = "ROLE:" ++ escape r
    show (Logo t d)         = "LOGO" ++ showType t ++ show d
    show (Agent a)          = "AGENT:" ++ (escape . show) a
    show (Organization os)  = "ORG:" ++ (intercalate ";" . map escape) os
    show (Categories cs)    = "CATEGORIES:" ++ (intercalate "," . map escape) cs
    show (Note n)           = "NOTE:" ++ escape n
    show (ProductId pid)    = "PRODID:" ++ escape pid
    show (Revision r)       = "REV:" ++ fmtTime "%Y-%m-%d" r
    show (SortString s)     = "SORT-STRING:" ++ escape s
    show (Sound t s)        = "SOUND" ++ showType t ++ show s
    show (UID u)            = "UID:" ++ escape u
    show (URL u)            = "URL:" ++ escape u
    show (Class c)          = "CLASS:" ++ show c
    show (Key t k)          = "KEY" ++ showType t ++ show k
    show (IndividualNames fn) = "N:" ++ (intercalate ";" . map escape) fn

instance Show AddrType where
    show AddrDomestic       = "DOM"
    show AddrInternational  = "INTL"
    show AddrPostal         = "POSTAL"
    show AddrParcel         = "PARCEL"
    show AddrHome           = "HOME"
    show AddrWork           = "WORK"
    show AddrPreferred      = "PREF"

instance Show TelType where
    show TelHome        = "HOME"
    show TelMessage     = "MSG"
    show TelWork        = "WORK"
    show TelVoice       = "VOICE"
    show TelFax         = "FAX"
    show TelCell        = "CELL"
    show TelVideo       = "VIDEO"
    show TelPager       = "PAGER"
    show TelBBS         = "BBS"
    show TelModem       = "MODEM"
    show TelCar         = "CAR"
    show TelISDN        = "ISDN"
    show TelPCS         = "PCS"
    show TelPreferred   = "PREF"

instance Show EmailType where
    show EmailInternet  = "INTERNET"
    show EmailX400      = "X400"
    show EmailPreferred = "PREF"

instance Show AgentData where
    show (AgentURI u)   = "VALUE=uri:" ++ escape u
    show (AgentVCard v) = escape . show $ v

instance Show Data where
    show (URI u)    = "VALUE=uri:"  ++ escape u
    show (Binary b) = "ENCODING=b:" ++ b

instance Show Class where
    show ClassPublic        = "PUBLIC"
    show ClassPrivate       = "PRIVATE"
    show ClassConfidential  = "CONFIDENTIAL"

-- | Output RFC 2426-compliant TYPE argument when it is just a singleton
showType         :: Maybe String -> String
showType Nothing  = ";"
showType (Just t) = ";TYPE=" ++ escape t ++ ";"

-- | Output RFC 2426-compliant TYPE arguments read for a given vCard line.
showTypes   :: [String] -> String
showTypes ts = "TYPE=" ++ intercalate "," ts ++ ":"

-- | Convenience function to format time with default system locale information.
fmtTime :: FormatTime t => String -> t -> String
fmtTime  = formatTime defaultTimeLocale

-- | Escapes commas and semicolons per RFC 2426 guidelines.
escape :: String -> String
escape [] = []
escape (x:xs)
    | x == ','  = "\\," ++ xs'
    | x == ';'  = "\\;" ++ xs'
    | otherwise = x:xs'
    where xs' = escape xs

-- $doc
--
-- This package implements the RFC 2426 vCard 3.0 spec
-- (<http://www.ietf.org/rfc/rfc2426.txt>)
--
-- Its usage is fairly simple and intuitive. For example, below is how one
-- would produce a VCard for Frank Dawson, one of the RFC 2426 authors:
--
-- > VCard   [ CommonName "Frank Dawson"
-- >         , Organization ["Lotus Development Corporation"]
-- >         , Address [AddrWork, AddrPostal, AddrParcel] "" ""
-- >                     "6544 Battleford Drive"
-- >                     "Raleigh" "NC" "27613-3502" "U.S.A"
-- >         , Telephone [TelVoice, TelMessage, TelWork] "+1-919-676-9515"
-- >         , Telephone [TelFax, TelWork] "+1-919-676-9564"
-- >         , Email [EmailInternet, EmailPreferred] "Frank_Dawson@Lotus.com"
-- >         , Email [EmailInternet] "fdawson@earthlink.net"
-- >         , URL "http://home.earthlink.net/~fdawson"
-- >         ]
--
-- Although this package is fairly well documented, even with general
-- explanations about the various VCard properties, RFC 2426 should be
-- consulted for the final say on the meaning or application of any of the
-- VCard properties.
