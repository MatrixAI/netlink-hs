{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module System.Linux.NetlinkUtil where

import Control.Applicative ((<$>))
import Control.Monad (when, replicateM_)
import Control.Monad.Loops (whileM)
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)
import qualified Data.Serialize.Get as SG
import qualified Data.Serialize.Put as SP
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import Data.Word (Word16)
import qualified Data.ByteString as BS (length)


type AttributeType = Word16

newtype AttributeHeader = AttributeHeader
  { attributeType :: AttributeType } deriving (Show, Eq)

type Attributes = [Attribute]

data Attribute = Attribute AttributeHeader
  (Either B.ByteString (NE.NonEmpty Attribute))
  deriving (Show, Eq)

isEmpty :: SG.Get Bool
isEmpty = SG.isEmpty

isNotEmpty :: SG.Get Bool
isNotEmpty = not <$> SG.isEmpty

-- inclusive of length
msgHeaderSize :: Int
msgHeaderSize = 16

type MessagePaySize = Int


attrAlign :: Int
attrAlign = 4

attrAlignRemainder :: Int -> Int
attrAlignRemainder attrSize = attrSize `mod` attrAlign

attrPadding :: Int -> Int
attrPadding attrSize = attrAlign - attrAlignRemainder attrSize

-- inclusive of length
attrHeaderSize :: Int
attrHeaderSize = 4

-- /*
--  *  <------- NLA_HDRLEN ------> <-- NLA_ALIGN(payload)-->
--  * +---------------------+- - -+- - - - - - - - - -+- - -+
--  * |        Header       | Pad |     Payload       | Pad |
--  * |   (struct nlattr)   | ing |                   | ing |
--  * +---------------------+- - -+- - - - - - - - - -+- - -+
--  *  <-------------- nlattr->nla_len -------------->
--  */

-- #define NLA_ALIGNTO             4
-- #define NLA_ALIGN(len)          (((len) + NLA_ALIGNTO - 1) & ~(NLA_ALIGNTO - 1))
-- #define NLA_HDRLEN              ((int) NLA_ALIGN(sizeof(struct nlattr)))

-- "Sometimes there could be Padding inserted to the attributes.
-- Usually it'll be taken care by macro,
-- but check alignments and padding if you face issue when parsing raw memory dump."

putAttributes :: Attributes -> Put
putAttributes = mapM_ putAttr
  where
    putAttr :: Attribute -> Put
    -- (Either B.ByteString (NE.NonEmpty Attribute))
    putAttr (Attribute _ (Right (_ NE.:| (_:_)))) = error ""
    putAttr (Attribute header (Right  (attribute NE.:| []) )) = do
      putAttrHeader header attrHeaderSize
      putAttr attribute
    putAttr (Attribute header (Left value)) = do
      let size = BS.length value
      putAttrHeader header size
      SP.putByteString value
      when (attrAlignRemainder size /= 0) $ do
        replicateM_ (attrPadding size) (SP.putWord8 0)

-- warning: body recursion
getAttr :: Set AttributeType -> Get Attribute
getAttr nestedAttrTypes = do
  (attrHeader, attrPaySize) <- getAttrHeader
  if (attributeType attrHeader) `Set.notMember` nestedAttrTypes then do
    attrVal <- SG.getByteString attrPaySize
    getAttrPadding attrPaySize
    return $ Attribute attrHeader (Left attrVal)
  else do
    nestedAttrs <- SG.isolate attrPaySize $ getAttrs nestedAttrTypes
    getAttrPadding attrPaySize
    return $ case NE.nonEmpty nestedAttrs of
      Just nestedAttrs' -> Attribute attrHeader (Right $ nestedAttrs')
      Nothing           -> Attribute attrHeader (Left "")



getAttrPadding :: Int -> Get ()
getAttrPadding size = do
  let remainder = attrAlignRemainder size
  e <- SG.isEmpty
  if not e && remainder /= 0
  then SG.skip $ attrAlign - remainder
  else return ()

-- putAttrPadding

getAttrs :: Set AttributeType -> Get Attributes
getAttrs nestedAttrTypes = whileM isNotEmpty $ getAttr nestedAttrTypes

putAttrHeader :: AttributeHeader -> Int -> Put
putAttrHeader (AttributeHeader attrType) size = do
  SP.putWord16host (fromIntegral $ size + attrAlign)
  SP.putWord16host attrType

getAttrHeader :: Get (AttributeHeader, Int)
getAttrHeader = SG.isolate attrHeaderSize $ do
  attrSize <- fromIntegral <$> SG.getWord16host
  attrType <- fromIntegral <$> SG.getWord16host
  return (AttributeHeader attrType, attrSize - attrHeaderSize)
