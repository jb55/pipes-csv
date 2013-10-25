{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Csv            (FromNamedRecord (..), Record,
                                      ToNamedRecord (..), namedRecord, (.:),
                                      (.=))
import           Data.Vector         (fromList)
import           Pipes
import           Pipes.ByteString    (ByteString, stdin)
import           Pipes.Csv           (decode, decodeByName, encodeByName)

import Pipes.Safe (runSafeT)
import qualified Pipes.Safe.Prelude  as PS
import qualified Pipes.ByteString    as PB
import System.IO (IOMode(WriteMode))

data Person = Person String Int
            deriving (Show)

instance FromNamedRecord Person where
  parseNamedRecord p =
    Person <$> p .: "name"
           <*> p .: "age"

personRec ~(Person name age) = ["name" .= name, "age" .= age]

instance ToNamedRecord Person where
  toNamedRecord = namedRecord . personRec

decoder :: Monad m
        => Producer ByteString m ()
        -> Producer (Either String Record) m ()
decoder = decode False

decoder2 :: Monad m
         => Producer ByteString m ()
         -> Producer (Either String (Int, Int)) m ()
decoder2 = decode False

persons :: Monad m => Producer ByteString m () -> Producer Person m ()
persons p = decodeByName p >-> right

right :: (Monad m) => Pipe (Either a b) b m r
right = loop
  where
    loop = await >>= \s -> case s of
      Left _  -> loop
      Right v -> yield v >> loop

write f = PS.withFile f WriteMode PB.toHandle

main = runSafeT $ runEffect $ pipeline
  where
    pipeline = persons stdin
           >-> encodeByName (fromList . map fst . personRec $ undefined)
           >-> write "persons_out.csv"
