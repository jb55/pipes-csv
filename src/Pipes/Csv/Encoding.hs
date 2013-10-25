{-# LANGUAGE OverloadedStrings #-}

{-| This module contains a couple functions copied from Data.Csv.Encoding
    that weren't exported. This file can be removed once they are.
-}

module Pipes.Csv.Encoding (
    encodeRecord
  , namedRecordToRecord
) where

import Blaze.ByteString.Builder (Builder, toByteString, fromByteString,
                                 fromWord8)
import Data.Monoid (mconcat, (<>), mempty)
import Data.Word (Word8)
import Data.Csv (Record, NamedRecord, Header)
import Data.ByteString (ByteString)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

encodeRecord :: Word8 -> Record -> Builder
encodeRecord delim = mconcat . intersperse (fromWord8 delim)
                   . map fromByteString . map escape . V.toList

intersperse :: Builder -> [Builder] -> [Builder]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs

prependToAll :: Builder -> [Builder] -> [Builder]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep <> x : prependToAll sep xs

namedRecordToRecord :: Header -> NamedRecord -> Record
namedRecordToRecord hdr nr = V.map find hdr
  where
    find n = case HM.lookup n nr of
        Nothing -> moduleError "namedRecordToRecord" $
                   "header contains name " ++ show (B8.unpack n) ++
                   " which is not present in the named record"
        Just v  -> v

moduleError :: String -> String -> a
moduleError func msg = error $ "Pipes.Csv.Encoding." ++ func ++ ": " ++ msg
{-# NOINLINE moduleError #-}

escape :: ByteString -> ByteString
escape s
    | B.any (\ b -> b == dquote || b == comma || b == nl || b == cr || b == sp)
        s = toByteString $
            fromWord8 dquote
            <> B.foldl
                (\ acc b -> acc <> if b == dquote
                    then fromByteString "\"\""
                    else fromWord8 b)
                mempty
                s
            <> fromWord8 dquote
    | otherwise = s
  where
    dquote = 34
    comma  = 44
    nl     = 10
    cr     = 13
    sp     = 32

