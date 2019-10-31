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
import qualified Data.List.NonEmpty as NE
import qualified System.Linux.Netlink.Constants as NLC

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

class (Show a, Eq a) => FamilyHeader a where
  getFamilyHeader :: NLC.MessageType -> Get a
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

attrAlign :: Int
attrAlign = 4

attrAlignRemainder :: Int -> Int
attrAlignRemainder attrSize = attrSize `mod` attrAlign

attrPadding :: Int -> Int
attrPadding attrSize = attrAlign - attrAlignRemainder attrSize

-- inclusive of length
attrHeaderSize :: Int
attrHeaderSize = 4

getAttrHeader :: Get (AttributeHeader, Int)
getAttrHeader = SG.isolate attrHeaderSize $ do
  attrSize <- fromIntegral <$> SG.getWord16host
  attrType <- fromIntegral <$> SG.getWord16host
  return (AttributeHeader attrType, attrSize - attrHeaderSize)

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

getAttrs :: Set AttributeType -> Get Attributes
getAttrs nestedAttrTypes = whileM isNotEmpty $ getAttr nestedAttrTypes

getAttrPadding :: Int -> Get ()
getAttrPadding size = do
  let remainder = attrAlignRemainder size
  e <- SG.isEmpty
  if not e && remainder /= 0
  then SG.skip $ attrAlign - remainder
  else return ()

-- inclusive of length
msgHeaderSize :: Int
msgHeaderSize = 16

getMsgHeader:: Get (MessageHeader, Int)
getMsgHeader = SG.isolate msgHeaderSize $ do
  msgSize   <- fromIntegral <$> SG.getWord32host
  msgType   <- fromIntegral <$> SG.getWord16host
  msgFlags  <- fromIntegral <$> SG.getWord16host
  msgSeqNum <- fromIntegral <$> SG.getWord32host
  msgPid    <- fromIntegral <$> SG.getWord32host
  return
    (MessageHeader { messageType = msgType
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
      | messageType msgHeader == NLC.eNLMSG_DONE = do
        SG.skip 4
        return $ MessageDone msgHeader
      | messageType msgHeader == NLC.eNLMSG_ERROR = do
        msgErrorCode <- fromIntegral <$> SG.getWord32host
        msgError <- getMsg nestedAttrTypes
        return $ MessageError msgHeader msgErrorCode msgError
      | otherwise = do
        msgFamily <- getFamilyHeader (messageType msgHeader)
        msgAttrs <- getAttrs nestedAttrTypes
        return $ Message msgHeader (Just msgFamily) msgAttrs

getMsgs :: (FamilyHeader a) => Set AttributeType -> Get (Messages a)
getMsgs nestedAttrTypes = whileM isNotEmpty $ getMsg nestedAttrTypes
