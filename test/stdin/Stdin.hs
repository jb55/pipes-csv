{-# LANGUAGE OverloadedStrings #-}

import Pipes.Csv (decode, decodeByName)
import Pipes.ByteString (stdin, ByteString)
import Data.Csv ((.:), FromNamedRecord(..), Record)
import Pipes
import Control.Applicative

data Person = Person String Int
            deriving (Show)

instance FromNamedRecord Person where
  parseNamedRecord p =
    Person <$> p .: "name"
           <*> p .: "age"

decoder :: Monad m
        => Producer ByteString m ()
        -> Producer (Either String Record) m ()
decoder = decode False

decoder2 :: Monad m
         => Producer ByteString m ()
         -> Producer (Either String (Int, Int)) m ()
decoder2 = decode False

persons :: Monad m
        => Producer ByteString m ()
        -> Producer (Either String Person) m ()
persons = decodeByName

main = runEffect $ for (persons stdin) (lift . print)
