{-| This module allows constant-space CSV parsing.

    It feeds 'ByteString's into cassavas incremental CSV parser to attain true
    constant-space record streaming.
-}

module Pipes.Csv (
  feedParser,
  feedHeaderParser,
  decode,
  decodeWith,
  decodeByName,
  decodeByNameWith
) where

import qualified Data.Csv.Incremental as CI
import qualified Data.ByteString as B

import Data.Csv.Incremental (Parser(..), HeaderParser(..))
import Data.Csv (DecodeOptions, FromRecord,
                 FromNamedRecord, defaultDecodeOptions)
import Data.ByteString (ByteString)
import Pipes

-- | Create a Record 'Producer' by feeding 'ByteString's into a 'Parser'
feedParser :: Monad m
           => Parser a
           -> Producer ByteString m ()
           -> Producer (Either String a) m ()
feedParser parser source = case parser of
    Fail _ e  -> yield (Left e)
    Done es   -> each es
    Some es k -> each es >> cont k source
    Partial k -> cont k source
  where
    cont = continue feedParser


-- | Create a NamedRecord 'Producer' by feeding 'ByteString's into a 'Parser'
feedHeaderParser :: (Monad m, FromNamedRecord a)
                 => HeaderParser (Parser a)
                 -> Producer ByteString m ()
                 -> Producer (Either String a) m ()
feedHeaderParser headerParser source = case headerParser of
    FailH _bs e -> yield (Left e)
    PartialH k -> cont k source
    DoneH _ p  -> feedParser p source
  where
    cont = continue feedHeaderParser


-- | Handle continuations properly within a Producer
continue :: (Monad (t m), Monad m, MonadTrans t)
         => (a -> Producer ByteString m () -> t m b)
         -> (ByteString -> a)
         -> Producer ByteString m ()
         -> t m b
continue feed k producer = do
  x <- lift (next producer)
  case x of
    Left () -> feed (k B.empty) (return ())
    Right (bs, producer') ->
      if (B.null bs)
      then continue feed k producer'
      else feed (k bs) producer'



-- | Equivalent to @'decodeWith' 'defaultDecodeOptions'@.
decode :: (Monad m, FromRecord a)
       => Bool
       -> Producer ByteString m ()
       -> Producer (Either String a) m ()
decode = decodeWith defaultDecodeOptions


-- | Create a 'Producer' that takes a 'ByteString' 'Producer' as input,
-- producing either errors or 'FromRecord's.
decodeWith :: (Monad m, FromRecord a)
           => DecodeOptions
           -> Bool
           -> Producer ByteString m ()
           -> Producer (Either String a) m ()
decodeWith opts skipHeader src = feedParser (CI.decodeWith opts skipHeader) src


-- | Equivalent to @'decodeByNameWith' 'defaultDecodeOptions'@.
decodeByName :: (Monad m, FromNamedRecord a)
             => Producer ByteString m ()
             -> Producer (Either String a) m ()
decodeByName = decodeByNameWith defaultDecodeOptions


-- | Create a 'Producer' that takes a 'ByteString' 'Producer' as input,
-- producing either errors or 'FromNamedRecord's.
decodeByNameWith :: (Monad m, FromNamedRecord a)
                 => DecodeOptions
                 -> Producer ByteString m ()
                 -> Producer (Either String a) m ()
decodeByNameWith opts src = feedHeaderParser (CI.decodeByNameWith opts) src
