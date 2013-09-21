
import qualified Pipes.Csv as C
import Pipes.ByteString
import Data.Csv
import Pipes
import Pipes.Core

decoder :: Monad m => Pipe ByteString (Either String Record) m ()
decoder = C.decodeWith defaultDecodeOptions False

main = runEffect $ for (stdin >-> decoder) (lift . print)
