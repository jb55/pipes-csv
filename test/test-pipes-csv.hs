{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-unused-do-bind -fno-warn-orphans -fno-warn-unused-imports -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
 ) where

import           Test.Framework as TF
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit

import           Data.List.Utils (replace)
import           Control.Applicative
import           Control.Monad
import           Control.Arrow (left)
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as DV

import           Data.Csv ((.:), (.!), FromNamedRecord(..), FromRecord(..),
                           Record, HasHeader(..))
import           Pipes.ByteString (stdin, ByteString)
import           Pipes.Csv (decode, decodeByName)
import           Pipes
import qualified Pipes.Prelude as P

-- helper functions
right :: (Monad m) => Pipe (Either a b) b m r
right = loop
  where
    loop = await >>= \s -> case s of
      Left _  -> loop
      Right v -> yield v >> loop

-- decode to vector
decoder :: Monad m
        => Producer ByteString m ()
        -> Producer (Either String Record) m ()
decoder = decode NoHeader

decoderStream =
    [ ""
    , "1"
    , "\n"
    , "1,2\n"
    , "1,2,3\n"
    , "\n" -- ignores empty line
    , "no line end needed on last chunk"
    ]

decoderResult =
    [
        ["1"]
      , ["1","2"]
      , ["1","2","3"]
      , ["no line end needed on last chunk"]
    ]

testDecoder :: Assertion
testDecoder =
    map DV.fromList decoderResult @=?
    P.toList (decoder (each $ map C.pack decoderStream) >-> right)

ioDecoder = runEffect $
    for (decoder (each $ map C.pack decoderStream)) (lift . print)

-- decode to Int tuple
decoderTuple :: Monad m
         => Producer ByteString m ()
         -> Producer (Either String (Int, Int)) m ()
decoderTuple = decode NoHeader

decoderTupleStream =
    [ ""
    , "1,2"
    , "\n"
    , "1,2\n"
    , "2,3\n"
    , "\n" -- ignores empty line
    , "one,two\n"
    , "no line end needed on last chunk\n"
    , "0,0"
    ]

decoderTupleResult =
    [ Right (1,2)
    , Right (1,2)
    , Right (2,3)
    , Left "expected Int, got \"one\" (Failed reading: takeWhile1)"
    , Left "cannot unpack array of length 1 into a pair. Input record: [\"no line end needed on last chunk\"]"
    , Right (0,0)
    ]

testDecoderTuple :: Assertion
testDecoderTuple =
    let res = P.toList (decoderTuple (each $ map C.pack decoderTupleStream))
        fixup = replace "fromList " "" -- vector <0.11 compatibility
    in decoderTupleResult @=? map (left fixup) res

data Person = Person String Int
            deriving (Show, Eq)

instance FromRecord Person where
  parseRecord v =
    Person <$> v .! 0
           <*> v .! 1

decoderPerson :: Monad m
        => Producer ByteString m ()
        -> Producer (Either String Person) m ()
decoderPerson = decode NoHeader

decoderPersonStream =
    [ ""
    , "Jack,21\nJill,55\n"
    , "Jimbo,21,beyond this is discarded,and this\n"
    , "Bill,1\n"
    , "Bill"
    , "y"
    , ","
    , "2"
    , "\n"
    , "Sue,22abc\n" -- consumes 22 and discards "abc"
    -- , "zz\n" -- throws an exception
    , "space allowed, 99\n"
    , "eof,99"
    ]

decoderPersonResult =
    [ Right (Person "Jack" 21)
    , Right (Person "Jill" 55)
    , Right (Person "Jimbo" 21)
    , Right (Person "Bill" 1)
    , Right (Person "Billy" 2)
    , Left "expected Int, got \"22abc\" (incomplete field parse, leftover: [97,98,99])"
    , Right (Person "space allowed" 99)
    , Right (Person "eof" 99)
    ]

testDecoderPerson :: Assertion
testDecoderPerson =
    decoderPersonResult @=?
    P.toList (decoderPerson (each $ map C.pack decoderPersonStream))

ioDecoderPerson = runEffect $
    for (decoderPerson (each $ map C.pack decoderPersonStream)) (lift . print)

-- data record version of Person

data Person' = Person' {
    name :: String
  , age :: Int
  } deriving (Show)

instance FromRecord Person' where
  parseRecord v =
    Person' <$> v .! 0
            <*> v .! 1

decoderPerson' :: Monad m
        => Producer ByteString m ()
        -> Producer (Either String Person') m ()
decoderPerson' = decode NoHeader

ioDecoderPerson' = runEffect $
    for (decoderPerson' (each $ map C.pack decoderPersonStream)) (lift . print)

decoderPerson'Result = [21,55,21,1,2,99,99]

testDecoderPerson' :: Assertion
testDecoderPerson' =
    decoderPerson'Result @=?
    P.toList (decoderPerson' (each $ map C.pack decoderPersonStream)
        >-> right >-> P.map age)

-- named person record
instance FromNamedRecord Person where
  parseNamedRecord p =
    Person <$> p .: "name"
           <*> p .: "age"

decoderNamedPerson :: Monad m
        => Producer ByteString m ()
        -> Producer (Either String Person) m ()
decoderNamedPerson = decodeByName

decoderNamedPersonStream = "Name,Age\n" : decoderPersonStream

ioDecoderNamedPerson = runEffect $
    for (decoderNamedPerson (each $ map C.pack decoderNamedPersonStream))
        (lift . print)

-- test frame
main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests =
  [
    testGroup "Pipes.Csv." decodeTests
  ]

decodeTests = [ testCase "HUnit" testDecoder
              , testCase "HUnit" testDecoderTuple
              , testCase "HUnit" testDecoderPerson
              , testCase "HUnit" testDecoderPerson'
              ]
