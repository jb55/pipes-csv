
import Pipes.Csv (decode)
import Pipes.ByteString (stdin, ByteString)
import Data.Csv (Record)
import Pipes

decoder :: Monad m => Pipe ByteString (Either String Record) m ()
decoder = decode False

main = runEffect $ for (stdin >-> decoder) (lift . print)
