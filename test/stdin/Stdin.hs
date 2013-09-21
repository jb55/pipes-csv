
import Pipes.Csv (decode)
import Pipes.ByteString (stdin, ByteString)
import Data.Csv (Record)
import Pipes

decoder :: Monad m => Producer ByteString m () -> Producer (Either String Record) m ()
decoder = decode False

decoder2 :: Monad m => Producer ByteString m () -> Producer (Either String (Int, Int)) m ()
decoder2 = decode False

main = runEffect $ for (decoder stdin) (lift . print)
