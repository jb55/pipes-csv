{-# LANGUAGE OverloadedStrings #-}

{-| This module allows constant-space CSV parsing.

    It feeds 'ByteString's into cassavas incremental CSV parser to attain true
    constant-space record streaming.
-}


module Pipes.Csv (
  -- * Example
  -- $example

  -- * Decode records
  decode,
  decodeWith,

  -- * Decode named records
  decodeByName,
  decodeByNameWith,

  -- * Decode parsed records
  feedParser,
  feedHeaderParser,

  -- * Encode records
  encode,
  encodeWith,

  -- * Encode named records
  encodeByName,
  encodeByNameWith,

-- * Re-exports
-- $reexports
  module Data.Csv,
  HasHeader(..)
) where


import qualified Data.Csv.Incremental as CI
import qualified Data.Csv.Conversion as Conversion
import qualified Data.ByteString as B
import qualified Pipes.Prelude as P

import Pipes
import Pipes.Csv.Encoding
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Blaze.ByteString.Builder (toByteString, fromByteString)
import Data.Monoid ((<>))
import Data.Csv.Incremental (Parser(..), HeaderParser(..), HasHeader(..))
import Data.Csv (
  Header, DecodeOptions, EncodeOptions(encDelimiter), FromRecord(..),
  FromNamedRecord(..), ToRecord(..), ToField(..), FromField(..),
  defaultDecodeOptions, defaultEncodeOptions, ToNamedRecord(..), Record, Field,
  NamedRecord, (.!), (.:), (.=))

-- $example
--
-- Heres a simple example that reads from stdin and writes to a file
--
-- @
--import Pipes.Safe (runSafeT)
--import qualified Pipes.Safe.Prelude  as PS
--import qualified Pipes.ByteString    as PB
--import Data.Vector (fromList)
--import System.IO (IOMode(WriteMode))
--
--data Person = Person String Int
--            deriving (Show)
--
--instance FromNamedRecord Person where
--  parseNamedRecord p =
--    Person \<$\> p .: \"name\"
--           \<*\> p .: \"age\"
--
--personRec ~(Person name age) = [\"name\" .= name, \"age\" .= age]
--
--instance ToNamedRecord Person where
--  toNamedRecord = 'namedRecord' . personRec
--
--persons :: Monad m => Producer ByteString m () -> Producer Person m ()
--persons p = 'decodeByName' p >-> right
--
---- note: right can be replaced with Pipes.Prelude.concat in ghc-7.8,
----       thanks to a Foldable instance for Either
--right :: (Monad m) => Pipe (Either a b) b m r
--right = loop
--  where
--    loop = await >>= \s -> case s of
--      Left _  -> loop
--      Right v -> yield v >> loop
--
--write f = PS.withFile f WriteMode PB.toHandle
--
--main = 'runSafeT' $ runEffect $ pipeline
--  where
--    header = fromList $ map fst $ personRec undefined
--    pipeline = persons stdin
--           \>-> right
--           \>-> 'encodeByName' header
--           \>-> write \"persons_out.csv\"
-- @

-- | Create a Record 'Producer' by feeding 'ByteString's into a 'Parser'
feedParser :: Monad m
           => Parser a
           -> Producer ByteString m ()
           -> Producer (Either String a) m ()
feedParser parser source = case parser of
    Fail _ e  -> yield (Left e)
    Done es   -> each es
    Many es k -> each es >> cont k source
  where
    cont = continue feedParser


-- | Create a NamedRecord 'Producer' by feeding 'ByteString's into a 'Parser'
feedHeaderParser :: (Monad m)
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
       => CI.HasHeader
       -> Producer ByteString m ()
       -> Producer (Either String a) m ()
decode = decodeWith defaultDecodeOptions


-- | Same as @'decode', except you can pass an explicit parser value instead
-- of using the typeclass
decode' :: Monad m
        => (Record -> Conversion.Parser a)
        -> CI.HasHeader
        -> Producer ByteString m ()
        -> Producer (Either String a) m ()
decode' p = decodeWith' p defaultDecodeOptions


-- | Create a 'Producer' that takes a 'ByteString' 'Producer' as input,
-- producing either errors or 'FromRecord's.
decodeWith :: (Monad m, FromRecord a)
           => DecodeOptions
           -> HasHeader
           -> Producer ByteString m ()
           -> Producer (Either String a) m ()
decodeWith opts hasHeader src = feedParser (CI.decodeWith opts hasHeader) src


-- | Create a 'Producer' that takes a 'ByteString' 'Producer' as input,
-- producing either errors or 'FromRecord's.
decodeWith' :: Monad m
            => (Record -> Conversion.Parser a)
            -> DecodeOptions
            -> HasHeader
            -> Producer ByteString m ()
            -> Producer (Either String a) m ()
decodeWith' p opts hasHeader src = feedParser (CI.decodeWith' p opts hasHeader) src


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

-- | Encode records as strict 'ByteString's
encode :: (Monad m, ToRecord a) => Pipe a ByteString m r
encode = encodeWith defaultEncodeOptions

-- | Encode named records as strict 'ByteString's
encodeByName :: (Monad m, ToNamedRecord a)
             => Header -> Pipe a ByteString m r
encodeByName = encodeByNameWith defaultEncodeOptions

-- | Encode a record with a trailing CrLf
encodeWithCrLf :: Word8 -> Record -> ByteString
encodeWithCrLf d = toByteString . (<> fromByteString "\r\n") . encodeRecord d

-- | Encode records as strict 'ByteString's
encodeWith :: (Monad m, ToRecord a)
           => EncodeOptions
           -> Pipe a ByteString m r
encodeWith opts = P.map (encodeWithCrLf delim . toRecord)
  where
    delim = encDelimiter opts

-- | Encode named records as strict 'ByteString's
encodeByNameWith :: (Monad m, ToNamedRecord a)
                 => EncodeOptions
                 -> Header
                 -> Pipe a ByteString m r
encodeByNameWith opts hdr = do
  yield $ toByteString $ encodeRecord delim hdr <> fromByteString "\r\n"
  P.map (encodeWithCrLf delim . namedRecordToRecord hdr . toNamedRecord)
  where
    delim = encDelimiter opts

-- $reexports
--
-- "Data.Csv" re-exports common types and operators:
--
--    * 'FromRecord', 'FromNamedRecord', 'ToRecord', 'ToNamedRecord'
--
--    * 'ToField', 'FromField'
--
--    * 'Record', 'Field', 'NamedRecord'
--
--    * '(.!)', '(.:)', '(.=)'
--
--    * 'DecodeOptions', 'defaultDecodeOptions'
--
