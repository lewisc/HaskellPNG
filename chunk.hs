module Chunks ( Chunk ) where
import Data.Word

--TODO: create a "readByte" class that defines a function
-- Readbyte 'a :: Word8 -> 'a

data ImageLayout = IndexedColor Bool
                  | Grayscale Bool
                  | Truecolor

--The number of bits per per sample or palette index
data BitDepth =  One
               | Two
               | Four
               | Eight
               | Sixteen

instance Show BitDepth where
    show x = case x of
             One -> "1"
             Two -> "2"
             Four -> "4"
             Eight -> "8"
             Sixteen -> "16"

--The color type allowed
data ColorType =  Normal
                | Color
                | Alpha
                | PaletteAndColor
                | ColorAndAlpha

instance Show ColorType where
    show x = case x of
             Normal          -> "0"
             Color           -> "2"
             Alpha           -> "4"
             PaletteAndColor -> "3"
             ColorAndAlpha   -> "6"

--The Compression methods allowed, currently only method 0 is defined
data Compression = DeflateInflate

instance Show Compression where
    show x = case x of
             DeflateInflate -> "0"

--The filter type that is supported, currently only method 0 is supported
data Filter = Adaptive

instance Show Filter where
    show x = case x of
             Adaptive -> "0"

--Interlace technique, currently either none or Adam 7 is supported
data Interlace =  NoInterlace
                | Adam7

instance Show Interlace where
    show x = case x of
             NoInterlace -> "0"
             Adam7       -> "1"

--a private data type for Palette representing the data
data Entry = Entry { blue  :: Word8,
                     red   :: Word8,
                     green :: Word8
                   } deriving (Show)

--Width and height are the goofy 2^31-1 size
data ChunkContent = 
               Header { 
                       width       :: !Word32,
                       height      :: !Word32,
                       depth       :: BitDepth,
                       colorType   :: ColorType,
                       compression :: Compression,
                       filter      :: Filter,
                       interlace   :: Interlace 
                      }
             | Palette [Entry]
             | DataChunk [Word8]
             | End
             | Unprocessed [Word8]
             | Private
             deriving (Show)

--Note, I am going to need to make my own derivations of
--word32 for this since it's an unsigned word32 with 2^31-1 
--as it's range(for compatibility reasons)
data Chunk = Chunk { header    :: !Word32,
                     chunkType :: !Word32,
                     content   :: [ChunkContent],
                     crc       :: !Word32 
                   } deriving (Show)

                    
