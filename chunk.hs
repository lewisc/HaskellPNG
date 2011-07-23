module Chunks ( Chunk ) where
import Data.Array
import Data.Bits
import Data.List
import Data.Word
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString.Lazy as B

data Chunk = Chunk { header :: [String],
                     chunkType :: [String],
                     crc :: [String] 
                   } deriving (Show)
                     
                     
