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

import Data.Csv.Incremental (Parser(..))
import Data.Csv (DecodeOptions, FromRecord, defaultDecodeOptions)
import Data.ByteString (ByteString)
import Pipes (yield, each, await, Pipe)

-- | Create a Pipe from a Record Parser
feedParser :: Monad m 
             => Parser a 
             -> Pipe ByteString (Either String a) m ()
feedParser p =
  case p of
    (Fail _ e)     -> yield (Left e) >> return ()
    (Partial cont) -> continue cont
    (Some es cont) -> each es >> continue cont
    (Done es)      -> each es >> return ()
  where
    continue cont = await >>= feedParser . cont

-- | Equivalent to @'decodeWith' 'defaultDecodeOptions'@.
decode :: (Monad m, FromRecord a) 
       => Bool
       -> Pipe ByteString (Either String a) m ()
decode = decodeWith defaultDecodeOptions


-- | Create a 'Pipe' that takes 'ByteString' input, producing either errors
-- or CSV records.
decodeWith :: (Monad m, FromRecord a)
           => DecodeOptions
           -> Bool
           -> Pipe ByteString (Either String a) m ()
decodeWith opts skipHeader = feedParser (CI.decodeWith opts skipHeader)

