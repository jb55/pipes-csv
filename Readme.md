
# pipes-csv

  Constant-space streaming csv parsing with cassava and pipes

## Installation

  Install with cabal

    $ cabal install pipes-csv

## Example

```haskell
import Pipes.Csv (decode)
import Pipes.ByteString (stdin, ByteString)
import Data.Csv (Record)
import Pipes

decoder :: Monad m => Pipe ByteString (Either String Record) m ()
decoder = decode False

main = runEffect $ for (stdin >-> decoder) (lift . print)
```


## License

  MIT
