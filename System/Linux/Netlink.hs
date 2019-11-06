{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module System.Linux.Netlink where

import Control.Applicative ((<$>))
import Data.List (intersperse)
import Control.Monad (when, replicateM_, unless)
import Control.Monad.Loops (whileM)
import Data.Bits (Bits, (.&.))
import Hexdump (prettyHex)
import Foreign.C.Types (CInt)
import Data.Word (Word16, Word32)
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)
import qualified Data.Serialize.Get as SG
import qualified Data.Serialize.Put as SP
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
import qualified System.Linux.Netlink.Constants as NLC
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int32)
import qualified Data.ByteString as BS (length)

data MessageHeader = MessageHeader
  { messageType :: NLC.MessageType
  , messageFlags :: Word16
  , messageSeqNum :: Word32
  , messagePid :: Word32
  } deriving (Show, Eq)

type Messages a = [Message a]

data Message a where
  Message :: (FamilyHeader a) => {
    messageHeader :: MessageHeader,
    messageFamily :: Maybe a,
    messageAttributes :: Attributes
  } -> Message a
  MessageError :: (FamilyHeader a) => {
    messageHeader :: MessageHeader,
    messageErrorCode :: CInt,
    messageError :: Message a
  } -> Message a
  MessageDone :: (FamilyHeader a) => {
    messageHeader :: MessageHeader
  } -> Message a


deriving instance Show (Message a)
deriving instance Eq (Message a)

data MessageType = Standard NLC.MessageType | Route NLC.RouteMessageType

class (Show a, Eq a) => FamilyHeader a where
  -- getFamilyHeader :: NLC.MessageType -> Get a
  getFamilyHeader :: MessageType -> Get a
  putFamilyHeader :: a -> Put

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
getMsgHeader:: Get (MessageHeader, MessagePaySize)
getMsgHeader = SG.isolate msgHeaderSize $ do
  msgSize   <- fromIntegral <$> SG.getWord32host
  msgType   <- (toEnum . fromIntegral) <$> SG.getWord16host
  msgFlags  <- fromIntegral <$> SG.getWord16host
  msgSeqNum <- fromIntegral <$> SG.getWord32host
  msgPid    <- fromIntegral <$> SG.getWord32host
  return
    (MessageHeader
    { messageType = msgType
    , messageFlags = msgFlags
    , messageSeqNum = msgSeqNum
    , messagePid = msgPid
    }, msgSize - msgHeaderSize)

getMsg :: (FamilyHeader a) => Set AttributeType -> Get (Message a)
getMsg nestedAttrTypes = do
  (msgHeader, msgPaySize) <- getMsgHeader
  getMsg' msgHeader
  where
    getMsg' msgHeader
      | messageType msgHeader == NLC.NLMSG_DONE = do
        SG.skip 4
        return $ MessageDone msgHeader
      | messageType msgHeader == NLC.NLMSG_ERROR = do
        msgErrorCode <- fromIntegral <$> SG.getWord32host
        msgError <- getMsg nestedAttrTypes
        return $ MessageError msgHeader msgErrorCode msgError
      | otherwise = do
        msgFamily <- getFamilyHeader $ Standard (messageType msgHeader)
        msgAttrs <- getAttrs nestedAttrTypes
        return $ Message msgHeader (Just msgFamily) msgAttrs

getMsgs :: (FamilyHeader a) => Set AttributeType -> Get (Messages a)
getMsgs nestedAttrTypes = whileM isNotEmpty $ getMsg nestedAttrTypes

-- |'Put' the netlink 'Header'
putMsgHeader
  :: Int -- ^The length of the message
  -> MessageHeader -- ^The header itself
  -> Put
putMsgHeader len (MessageHeader ty flags seqnum pid) = do
    SP.putWord32host (fromIntegral len)
    SP.putWord16host (fromIntegral . fromEnum $ ty)
    SP.putWord16host (fromIntegral flags)
    SP.putWord32host seqnum
    SP.putWord32host pid

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

-- |'Put' a 'Message' so it can be sent
putMessage :: Message a -> [ByteString]
putMessage (Message header family attributes) =
  let attrs = SP.runPut $ putAttributes attributes
      cus   = case family of
        Nothing -> B.empty
        Just family -> SP.runPut $ putFamilyHeader family
      hdr   = SP.runPut $ putMsgHeader (BS.length attrs + BS.length cus + msgHeaderSize) header
  in [hdr, cus, attrs]
putMessage etc = error $ "Cannot convert this message" ++ show etc

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
