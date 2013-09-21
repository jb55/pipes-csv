{-| This module allows constant-space CSV parsing.

    It feeds 'ByteString's into cassavas incremental CSV parser to attain true
    constant-space record streaming.
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipes.Csv (
  feedParser,
  decode,
  decodeWith
) where

import qualified Data.Csv.Incremental as CI
import qualified Data.ByteString as B

import Data.Csv.Incremental (Parser(..))
import Data.Csv (DecodeOptions, FromRecord, defaultDecodeOptions)
import Data.ByteString (ByteString)
import Pipes (yield, each, lift, next, Producer)

-- | Create a Record 'Producer' by feeding 'ByteString's into a 'Parser'
feedParser :: Monad m
           => Parser a
           -> Producer ByteString m ()
           -> Producer (Either String a) m ()
feedParser parser source =
  case parser of
    Fail _ e  -> yield (Left e)
    Done es   -> each es
    Some es k -> each es >> continue k source
    Partial k -> continue k source
  where
    continue k producer = do
      x <- lift (next producer)
      case x of
        Left () -> feedParser (k B.empty) (return ())
        Right (bs, producer') ->
          if (B.null bs)
          then continue k producer'
          else feedParser (k bs) producer'

-- | Equivalent to @'decodeWith' 'defaultDecodeOptions'@.
decode :: (Monad m, FromRecord a)
       => Bool
       -> Producer ByteString m ()
       -> Producer (Either String a) m ()
decode = decodeWith defaultDecodeOptions


-- | Create a 'Producer' that takes a 'ByteString' 'Producer' as input,
-- producing either errors or CSV records.
decodeWith :: (Monad m, FromRecord a)
           => DecodeOptions
           -> Bool
           -> Producer ByteString m ()
           -> Producer (Either String a) m ()
decodeWith opts skipHeader src = feedParser (CI.decodeWith opts skipHeader) src

