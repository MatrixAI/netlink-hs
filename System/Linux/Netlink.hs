{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
module System.Linux.Netlink where

import System.Linux.NetlinkUtil


import Control.Monad.Loops (whileM)
import Foreign.C.Types (CInt)
import Data.Word (Word16, Word32)
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)
import qualified Data.Serialize.Get as SG
import qualified Data.Serialize.Put as SP
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Set (Set)
import qualified System.Linux.Netlink.Constants as NLC
import qualified Data.ByteString as BS (length)


data MessageHeader a = MessageHeader
  { messageType :: MessageType a
  , messageFlags :: Word16
  , messageSeqNum :: Word32
  , messagePid :: Word32
  } deriving (Show, Eq)

type Messages a = [Message a]

data Message a where
  Message :: (FamilyHeader a) => {
    messageHeader :: (MessageHeader a),
    messageFamily :: Maybe a,
    messageAttributes :: Attributes
  } -> Message a
  MessageError :: (FamilyHeader a) => {
    messageHeader :: (MessageHeader a),
    messageErrorCode :: CInt,
    messageError :: Message a
  } -> Message a
  MessageDone :: (FamilyHeader a) => {
    messageHeader :: (MessageHeader a)
  } -> Message a


deriving instance Show (Message a)
deriving instance Eq (Message a)

-----------------------------------
-- add type families features
instance Enum (MessageType a) 
-- instance Enum (MessageType a) where
--   toEnum a = toEnum a
--   fromEnum a = fromEnum a

instance Show (MessageType a)
instance Eq (MessageType a)

class (Show a, Eq a) => FamilyHeader a where
  data MessageType a :: *
  reifyMsg :: a -> MessageType a
  getFamilyHeader :: MessageType a -> Get a
  putFamilyHeader :: a -> Put

data StandardMessageType = Standard NLC.MessageType deriving (Show, Eq)

instance FamilyHeader StandardMessageType where
  data MessageType StandardMessageType = StandardMessageType'
  reifyMsg = undefined
  getFamilyHeader StandardMessageType' = undefined
  putFamilyHeader = undefined
-----------------------------------

getMsgHeader:: Get (MessageHeader a, MessagePaySize)
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

-- getMsg :: (FamilyHeader a) => Set AttributeType -> Get (Message a)
getMsg :: Set AttributeType -> Get (Message StandardMessageType)
getMsg nestedAttrTypes = do
  (msgHeader, msgPaySize) <- getMsgHeader
  getMsg' msgHeader
  where
    getMsg' msgHeader

      | messageType msgHeader == reifyMsg (Standard NLC.NLMSG_DONE) = do
        SG.skip 4
        return $ MessageDone msgHeader

      | messageType msgHeader == reifyMsg (Standard NLC.NLMSG_DONE)  = do
        msgErrorCode <- fromIntegral <$> SG.getWord32host
        msgError <- getMsg nestedAttrTypes
        return $ MessageError msgHeader msgErrorCode msgError

      | otherwise = do
        msgFamily <- getFamilyHeader $ messageType msgHeader
        msgAttrs <- getAttrs nestedAttrTypes
        return $ Message msgHeader (Just msgFamily) msgAttrs

-- getMsgs :: (FamilyHeader a) => Set AttributeType -> Get (Messages a)
getMsgs :: Set AttributeType -> Get [Message StandardMessageType]
getMsgs nestedAttrTypes = whileM isNotEmpty $ getMsg nestedAttrTypes


-- |'Put' the netlink 'Header'
putMsgHeader
  :: Int -- ^The length of the message
  -> (MessageHeader a) -- ^The header itself
  -> Put
putMsgHeader len (MessageHeader ty flags seqnum pid) = do
    SP.putWord32host (fromIntegral len)
    SP.putWord16host (fromIntegral . fromEnum $ ty)
    SP.putWord16host (fromIntegral flags)
    SP.putWord32host seqnum
    SP.putWord32host pid

-- |'Put' a 'Message' so it can be sent
putMessage :: Message a -> [ByteString]
putMessage (Message header family attributes) =
  let attrs = SP.runPut $ putAttributes attributes
      cus   = case family of
        Nothing -> B.empty
        Just family' -> SP.runPut $ putFamilyHeader family'
      hdr   = SP.runPut $ putMsgHeader (BS.length attrs + BS.length cus + msgHeaderSize) header
  in [hdr, cus, attrs]
putMessage etc = error $ "Cannot convert this message" ++ show etc
