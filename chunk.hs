-- |Exposes the types that are necessary to process a PNG into
--various different forms, including raw access to the underlying
--constructions
module Chunks ( Chunk, ImageLayout, BitDepth, ColorType, Compression,
                Filter, Interlace, ChunkContent, Entry ) where
import Data.Word

--TODO: create a "readByte" class that defines a function
-- Readbyte 'a :: Word8 -> 'a

-- |The different layout options available, and whether it has an
--Alpha channel
data ImageLayout = IndexedColor Bool -- ^Represents an indexed color image
                                     --with a bool representing the presence 
                                     --alpha channel
                  | Grayscale Bool   -- ^Represents a grayscale image with a
                                     --bool representing the presence of an
                                     --alpha channel
                  | Truecolor        -- ^represents a true color image

-- |The number of bits per per sample or palette index
data BitDepth =  One
               | Two
               | Four
               | Eight
               | Sixteen
-- |Returns the depth as a numeric in a string
instance Show BitDepth where
    show x = case x of
             One -> "1"
             Two -> "2"
             Four -> "4"
             Eight -> "8"
             Sixteen -> "16"

-- |The color type used in the image
data ColorType =  Normal
                | Color
                | Alpha
                | PaletteAndColor
                | ColorAndAlpha

-- |Returns the colortype as a numeric in a string, (as is specified in the 
--PNG specification) 
--
-- * Normal = 0
-- * Color = 2
-- * Alpha = 4
-- * Palette and Color = 6
-- * Color and Alpha = 8
instance Show ColorType where
    show x = case x of
             Normal          -> "0"
             Color           -> "2"
             Alpha           -> "4"
             PaletteAndColor -> "3"
             ColorAndAlpha   -> "6"

-- |The Compression methods allowed, currently only method 0 is defined
--(Deflate/Inflate)
data Compression = DeflateInflate

-- |Returns a numeric in a string as specified in the PNG specification
-- * Deflate/Inflate = 0
instance Show Compression where
    show x = case x of
             DeflateInflate -> "0"

-- |The filter type that is supported, currently only method 0 is supported
--(Adaptive filtering)
data Filter = Adaptive

-- |Returns a numeric in a string representing the filter method
-- * Adaptive = 0
instance Show Filter where
    show x = case x of
             Adaptive -> "0"

-- |Interlace technique, currently either none or Adam 7 is supported
data Interlace =  NoInterlace
                | Adam7

-- |Returns a numeric in a string representing the interlace method
-- * No interlacing = 0
-- * Adam 7 =1
instance Show Interlace where
    show x = case x of
             NoInterlace -> "0"
             Adam7       -> "1"

-- | Represents an entry in the palette
data Entry = Entry { blue  :: Word8, -- ^Blue channel
                     red   :: Word8, -- ^Red channel
                     green :: Word8  -- ^Green channel
                   } deriving (Show)

-- | Represents a chunk, different types are represented by different elements
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

-- |Represents a raw PNG image
data Chunk = Chunk { header    :: !Word32,
                     chunkType :: !Word32,
                     content   :: [ChunkContent],
                     crc       :: !Word32 
                   } deriving (Show)

                    
