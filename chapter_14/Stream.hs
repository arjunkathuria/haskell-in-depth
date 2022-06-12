import Streaming
import qualified Streaming.ByteString as BS
import Control.Monad.Trans.Resource
import Data.Function ((&))

copyFile :: FilePath -> FilePath -> IO Int
copyFile fIn fOut = runResourceT $ do
  ( len :> () ) <- BS.readFile fIn
                 & BS.copy
                 & BS.length
                 & BS.writeFile fOut
  pure $ len
