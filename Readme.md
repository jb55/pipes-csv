
# pipes-csv

  Constant-space streaming csv parsing with cassava and pipes

  [![build status](https://secure.travis-ci.org/jb55/pipes-csv.png)](http://travis-ci.org/jb55/pipes-csv)

## Installation

  Install with cabal

    $ cabal install pipes-csv

## Example

```haskell
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

persons :: Monad m
        => Producer ByteString m ()
        -> Producer (Either String Person) m ()
persons = decodeByName

main = runEffect $ for (persons stdin) (lift . print)
```


## License

  MIT
