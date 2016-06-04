import Pipes
import Control.Monad (void)
import qualified Pipes.ByteString as PBS
import qualified Pipes.Lzma as Lzma

main :: IO ()
main = do
    Pipes.runEffect $ void (Lzma.decompress PBS.stdin) >-> PBS.stdout
    return ()
