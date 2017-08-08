{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveAnyClass, OverloadedStrings #-}

-- Utility to dump all of the release information as JSON.

module Main where

import GHC.Generics
import Data.Aeson

import Types
import ReleaseFiles
import Releases

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BS

deriving instance Generic OS
deriving instance ToJSON OS

deriving instance Generic Arch
deriving instance ToJSON Arch

deriving instance Generic DistType
deriving instance ToJSON DistType

deriving instance Generic Package
deriving instance ToJSON Package

deriving instance Generic IncludeType
deriving instance ToJSON IncludeType

deriving instance Generic HpVersion
deriving instance ToJSON HpVersion

jlist :: (a -> Value) -> [a] -> Value
jlist tojson as = Array (V.fromList (map tojson as))

jfileinfo (disttype, url, mhash, isfull) =
  object [ "disttype" .= toJSON disttype
         , "url" .= toJSON url
         , "hash" .= toJSON mhash
         , "isfull" .= toJSON isfull
         ]

jdate :: Date -> Value
jdate (month, year) =
  object [ "month" .= toJSON month
         , "year" .= toJSON year
         ]
 
jreleaseFiles (version, date, fileInfos) =
  object [ "version" .= version
         , "date" .= jdate date
         , "fileInfo" .= jlist jfileinfo fileInfos
         ]

jrelease :: Release -> Value
jrelease r =
  object [ "relVersion"         .= toJSON (relVersion r)
         , "relMinimalIncludes" .= jlist jinclude (relMinimalIncludes r)
         , "relIncludes"        .= jlist jinclude (relIncludes r)
         ]

jinclude :: Include -> Value
jinclude (inctype, package) =
  object [ "includeType" .= toJSON inctype
         , "package" .= toJSON package
         ]



jalldata :: [ReleaseFiles] -> [Release] -> Value
jalldata releaseFiles releases =
  object [ "releaseFiles" .= jlist jreleaseFiles releaseFiles
         , "releases"     .= jlist jrelease releases
         ]

main = BS.putStrLn $ encode $ jalldata releaseFiles releases

