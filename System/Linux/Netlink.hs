{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- MessageType is defined inside Constants
-- those are constants there but there are attribute word16 as well
-- in fact we get those things occurring here

data MessageHeader = MessageHeader
  { messageType :: NLC.MessageType
  , messageFlags :: Word16
  , messageSeqNum :: Word32
  , messagePID :: Word32
  } deriving (Show, Eq)

-- inclusive of length
messageHeaderSize :: Int
messageHeaderSize = 16

type AttributeType = Word16

newtype AttributeHeader = AttributeHeader
  { attributeType :: AttributeType } deriving (Show, Eq)

-- inclusive of length
attributeHeaderSize :: Int
attributeHeaderSize = 4

type Attributes = [Attribute]

data Attribute = Attribute AttributeHeader
  (Either B.ByteString (NE.NonEmpty Attribute))
  deriving (Show, Eq)

data Message a = Message
  { messageHeader :: MessageHeader
  , messageCustom :: a
  , messageAttributes :: Attributes
  } |
  MessageError
  { messageHeader :: MessageHeader
  , messageErrorCode :: CInt
  , messageError :: Message a
  } |
  MessageDone
  { messageHeader :: MessageHeader
  }
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

parseAttrs :: Set AttributeType -> Get Attributes
parseAttrs containerTypes = whileM isNotEmpty $ parseAttr containerTypes

-- this kind of recursion is not tail recursive
-- so it can blow up on large sizes
-- but i doubt that netlink messages are that big
parseAttr :: Set AttributeType -> Get Attribute
parseAttr containerTypes = do
  attrSize <- fromIntegral <$> SG.getWord16host
  attrType <- fromIntegral <$> SG.getWord16host
  if Set.notMember attrType containerTypes then do
    attrVal <- SG.getByteString (attrSize - attributeHeaderSize)
    parseAttrPadding attrSize
    return $ Attribute (AttributeHeader attrType) (Left attrVal)
  else do
    nestedAttrs <- SG.isolate attrSize $ parseAttrs containerTypes
    parseAttrPadding attrSize
    case NE.nonEmpty nestedAttrs of
      Just nestedAttrs' -> return $ Attribute (AttributeHeader attrType) (Right $ nestedAttrs')
      Nothing -> return $ Attribute (AttributeHeader attrType) (Left "")

parseAttrPadding :: Int -> Get ()
parseAttrPadding attrSize = do
  let remainder = attrAlignRemainder attrSize
  e <- SG.isEmpty
  if not e && remainder /= 0
  then SG.skip $ attrAlign - remainder
  else return ()




-- I think then we cannot consume the attrVal


--
-- wait this is problematic
-- we now need a tree parser
-- since getting 1 attribute may go into getting another attribute
-- so how do we know when to do this?
-- well we apply getting an attribute to it again
-- until it's an arbitrary bytestring
-- well some attributes contain more attributes
-- some attributes don't
-- it depends on the protocol
-- so what we could do is to try.. and parse against an attribute
-- so how do we know when an arbitrary bytestring may contain nested attributes?
-- what if we are given a "map"
-- essentially a nested map that allows us to know if something means that its nested
-- specifically if the type allows type allows it to be nested
-- so we need something we can Check
--



-- size for binary size
-- ty

-- this is really parse a packet
-- we are using Get as a "parser" here
-- rather than returning eithers
-- size...
-- getGenPacket :: Get (Packet a)
-- getGenPacket = do
--   (size, hdr) <- getHeader
--   SG.isolate size $ getGenPacketContent hdr

-- -- you need to know the header to get the packet
-- getGenPacketContent :: Header -> Get (Packet a)
-- getGenPacketContent hdr
--   | messageType hdr == NLC.eNLMSG_DONE  = SG.skip 4 >> return $ PacketDone hdr
--   | messageType hdr == NLC.eNLMSG_ERROR = getError hdr
--   | otherwise = do
--     msg <- getGet (messageType hdr)
--     attrs <- getAttributes
--     return $ Packet hdr msg attrs



-- import Data.Word (Word16, Word32)

-- import System.Posix.Types (Fd(Fd))
-- import qualified System.Linux.Netlink.C as C
-- import System.Linux.Netlink.Helpers
-- import System.Linux.Netlink.Constants

--Generic protocol stuff

{- |Typeclase used by the system. Basically 'Storable' for 'Get' and 'Put'
getGet Returns a 'Get' function for the convertable. 
The MessageType is passed so that the function can parse different data structures
based on the message type.
-}
-- class Convertable a where
--   getGet :: MessageType -> Get a -- ^get a 'Get' function for the static data
--   getPut :: a -> Put -- ^get a 'Put' function for the static data

-- -- |Datatype to be used when there is no additional static header
-- data NoData = NoData deriving (Show, Eq)

-- instance Convertable NoData where
--   getPut _ = return ()
--   getGet _ = return NoData


-- instance Show Header where
--   show (Header t f s p) = 
--     "Type: " ++ show t ++ ", Flags: " ++ (show f) ++ ", Seq: " ++ show s ++ ", Pid: " ++ show p


-- -- |Helperfunction for show instance of 'Packet' and further specializations
-- showPacket :: Show a => Packet a -> String
-- showPacket (ErrorMsg hdr code pack) = 
--   "Error packet: \n" ++
--   show hdr ++ "\n" ++
--   "Error code: " ++ (show code) ++ "\n" ++
--   (show pack)
-- showPacket (DoneMsg hdr) = "Done: " ++ show hdr
-- showPacket (Packet hdr cus attrs) =
--   "NetlinkPacket: " ++ show hdr ++ "\n" ++
--   "Custom data: " ++ show cus ++ "\n" ++
--   "Attrs: \n" ++ showNLAttrs attrs

-- instance {-# OVERLAPPABLE #-} Show a => Show (Packet a) where
--   showList xs = ((concat . intersperse "===\n" . map show $xs) ++)
--   show = showPacket

-- -- |Convert generic NLAttrs into a string (# and hexdump)
-- showNLAttrs :: Attributes -> String
-- showNLAttrs = showAttrs show 

-- -- |Helper function to convert attributes into a string
-- showAttrs 
--   :: (Int -> String) -- ^A function from element id to its name
--   -> Attributes -- ^The attributes
--   -> String -- ^A string with Element name and hexdump of element
-- showAttrs sh = showAttrs' . toList
--   where
--     showAttrs' [] = []
--     showAttrs' (x:xs) = showAttr sh x ++ showAttrs' xs

-- -- |Helper function to generically show a single attribute
-- showAttr :: (Int -> String) -> (Int, ByteString) -> String
-- showAttr sh (i,v) = sh i ++ ": " ++ prettyHex v


-- -- |'Get' Attributes
-- getAttributes :: Get Attributes
-- getAttributes = fromList <$> whileM (not <$> isEmpty) getSingleAttribute

-- -- |'Get' a single 'Attribute'
-- getSingleAttribute :: Get (Int, ByteString)
-- getSingleAttribute = do
--     len <- fromIntegral <$> g16
--     ty <- fromIntegral <$> g16
--     val <- getByteString (len - 4)
--     isEmpty >>= \e -> when (not e && len `mod` 4 /= 0) $ skip (4 - (len `mod` 4))
--     return (ty, val)

-- -- |'Get' the netlink 'Header'
-- getHeader :: Get (Int, Header)
-- getHeader = isolate 16 $ do
--       len    <- fromIntegral <$> g32
--       ty     <- fromIntegral <$> g16
--       flags  <- fromIntegral <$> g16
--       seqnum <- g32
--       pid    <- g32
--       return (len - 16, Header ty flags seqnum pid)

-- -- |'Put' the netlink 'Header'
-- putHeader
--   :: Int -- ^The length of the message
--   -> Header -- ^The header itself
--   -> Put
-- putHeader len (Header ty flags seqnum pid) = do
--     p32 (fromIntegral len)
--     p16 (fromIntegral ty)
--     p16 (fromIntegral flags)
--     p32 seqnum
--     p32 pid


-- -- |'Put' a 'Map' of 'Attributes'
-- putAttributes :: Attributes -> Put
-- putAttributes = mapM_ putAttr . toList
--   where
--     putAttr (ty, value) = do
--         p16 (fromIntegral $BS.length value + 4)
--         p16 (fromIntegral ty)
--         putByteString value
--         when (BS.length value `mod` 4 /= 0) $replicateM_ (4 - (BS.length value `mod` 4)) (p8 0)

-- -- |'Put' a 'Packet' so it can e sent
-- putPacket :: (Convertable a, Eq a, Show a) => Packet a -> [ByteString]
-- putPacket (Packet header custom attributes) =
--   let attrs = runPut $putAttributes attributes
--       cus   = runPut $getPut custom
--       hdr   = runPut $putHeader (BS.length attrs + BS.length cus + 16) header
--   in [hdr, cus, attrs]
-- putPacket _ = error "Cannot convert this for transmission"


-- -- |'Get' an error message
-- getError :: (Convertable a, Eq a, Show a) => Header -> Get (Packet a)
-- getError hdr = do
--   code <- fromIntegral <$> getWord32host
--   content <- getBytes =<< remaining
--   -- packet <- getGenPacket
--   return $ ErrorMsg hdr code content -- packet


-- -- | 'Get' the body of a packet (the 'Header' is already read from the buffer
-- getGenPacketContent :: (Convertable a, Eq a, Show a) => Header -> Get (Packet a)
-- getGenPacketContent hdr
--   | messageType hdr == eNLMSG_DONE  = skip 4 >> return (DoneMsg hdr)
--   | messageType hdr == eNLMSG_ERROR = getError hdr
--   | otherwise  = do
--       msg    <- getGet (messageType hdr)
--       attrs  <- getAttributes
--       return $ Packet hdr msg attrs

-- {- | 'Get' a packet

-- This returns a 'Get' function for a netlink message.
-- The message may have additional static data defined by the protocol.
-- -}
-- getGenPacket :: (Convertable a, Eq a, Show a) => Get (Packet a)
-- getGenPacket = do
--     (len, header) <- getHeader
--     isolate len $ getGenPacketContent header

-- {- | Read all 'Packet's from a buffer

-- Variant of 'getPackets' that allows to handle single failed packets
-- -}
-- getPackets' :: (Convertable a, Eq a, Show a) => ByteString -> [Either String (Packet a)]
-- getPackets' bytes = case runGetPartial getGenPacket bytes of
--     Partial _ -> [Left "Too short input for last message =.="] -- We got a
--     Fail msg _ -> Left msg : []
--     Done r "" -> pure $ Right r
--     Done r bs -> Right r : getPackets' bs

-- {- | Read all 'Packet's from a buffer

-- The packets may have additional static data defined by the protocol.
-- -}
-- getPackets :: (Convertable a, Eq a, Show a) => ByteString -> Either String [Packet a]
-- getPackets = sequence . getPackets'

-- -- | Typesafe wrapper around a 'CInt' (fd)
-- newtype NetlinkSocket = NS CInt

-- -- |Open and return a 'NetlinkSocket', for legacy reasons this opens a route socket
-- makeSocket :: IO NetlinkSocket
-- makeSocket = NS <$> C.makeSocket

-- -- |Open a 'NetlinkSocket'. This is the generic function
-- makeSocketGeneric 
--   :: Int -- ^The netlink family to use
--   -> IO NetlinkSocket
-- makeSocketGeneric = fmap NS . C.makeSocketGeneric

-- -- |Get the raw 'Fd' used for netlink communcation (this can be plugged into eventing)
-- getNetlinkFd :: NetlinkSocket -> Fd
-- getNetlinkFd (NS f) = Fd f

-- {- |Send a Message over netlink.

-- This is an internal function.
-- The prototype directly reflects the interface of the C functions.
-- -}
-- sendmsg :: NetlinkSocket -> [ByteString] -> IO ()
-- sendmsg (NS fd) = C.sendmsg fd

-- {- |Receive a Message over netlink.

-- This is an internal function.
-- The prototype directly reflects the interface of the C functions.
-- -}
-- recvmsg :: NetlinkSocket -> Int -> IO ByteString
-- recvmsg (NS fd) = C.recvmsg fd

-- recvBuffer :: NetlinkSocket -> IO ByteString
-- recvBuffer = flip recvmsg bufferSize

-- -- |Close a 'NetlinkSocket' when it is no longer used
-- closeSocket :: NetlinkSocket -> IO ()
-- closeSocket (NS fd) = C.closeSocket fd

-- -- |Join a netlink multicast group
-- joinMulticastGroup
--   :: NetlinkSocket -- ^The socket to join with
--   -> Word32  -- ^The id of the group to join, values of System.Linux.Netlink.Constants.eRTNLGRP_*
--   -> IO ()
-- joinMulticastGroup (NS fd) = C.joinMulticastGroup fd

-- -- |Leave a netlink multicast group
-- leaveMulticastGroup
--   :: NetlinkSocket -- ^The socket to leave
--   -> Word32  -- ^The id of the group to leave, values of System.Linux.Netlink.Constants.eRTNLGRP_*
--   -> IO ()
-- leaveMulticastGroup (NS fd) = C.leaveMulticastGroup fd

-- sendPacket :: (Convertable a, Eq a, Show a) => NetlinkSocket -> Packet a -> IO ()
-- sendPacket sock req = sendmsg sock (putPacket req)

-- -- generic query functions
-- {- |Query data over netlink.

-- This sends a 'Packet' over netlink and returns the answer.
-- This blocks in a safe foregin function until the other side replies.
-- -}
-- query :: (Convertable a, Eq a, Show a) => NetlinkSocket -> Packet a -> IO [Packet a]
-- query sock req = do
--     sendmsg sock (putPacket req)
--     recvMulti sock

-- query' :: (Convertable a, Eq a, Show a) => NetlinkSocket -> Packet a -> IO (Either String [Packet a])
-- query' sock req = do
--     sendmsg sock (putPacket req)
--     recvMulti' sock

-- -- |The same as 'query' but requires the answer to be a single message
-- queryOne :: (Convertable a, Eq a, Show a) => NetlinkSocket -> Packet a -> IO (Packet a)
-- queryOne sock req = do
--     sendmsg sock (putPacket req)
--     pkts <- recvMulti sock
--     case pkts of
--       [x] -> return x
--       _ -> fail ("Expected one packet, received " ++ (show . length $pkts))

-- recvMulti' :: (Convertable a, Eq a, Show a) => NetlinkSocket -> IO (Either String [Packet a])
-- recvMulti' sock =  fmap sequence (recvOne' sock) >>= \case
--     Left err -> pure $ Left err
--     Right pkts -> Right <$> do
--       if isMulti (first pkts)
--           then if isDone (last pkts)
--                -- This is fine because first would have complained before
--                then pure $ init pkts
--                else (pkts ++) <$> recvMulti sock
--           else return pkts
--   where
--     isMulti = isFlagSet fNLM_F_MULTI . messageFlags . packetHeader
--     isDone  = (== eNLMSG_DONE) . messageType . packetHeader
--     first (x:_) = x
--     first [] = error "Got empty list from recvOne' in recvMulti', this shouldn't happen"


-- -- |Internal function to receive multiple netlink messages
-- recvMulti :: (Convertable a, Eq a, Show a) => NetlinkSocket -> IO [Packet a]
-- recvMulti sock = do
--     pkts <- recvOne sock
--     if isMulti (first pkts)
--         then if isDone (last pkts)
--              -- This is fine because first would have complained before
--              then return $ init pkts
--              else (pkts ++) <$> recvMulti sock
--         else return pkts
--   where
--     isMulti = isFlagSet fNLM_F_MULTI . messageFlags . packetHeader
--     isDone  = (== eNLMSG_DONE) . messageType . packetHeader
--     first (x:_) = x
--     first [] = error "Got empty list from recvOne in recvMulti, this shouldn't happen"

-- {- | Calls recvmsg once and returns all received messages

-- Safe version of recvOne
-- -}
-- recvOne' :: (Convertable a, Eq a, Show a) => NetlinkSocket -> IO [Either String (Packet a)]
-- recvOne' sock = getPackets' <$> recvmsg sock bufferSize

-- {- | Calls recvmsg once and returns all received messages

-- This should only be used outside of the package when reading multicast messages.

-- The prototype of this function is unintuitive, but this cannot be avoided without
-- buffering in userspace with the netlink api.
-- -}
-- recvOne :: (Convertable a, Eq a, Show a) => NetlinkSocket -> IO [Packet a]
-- recvOne sock = recvmsg sock bufferSize >>= \b -> case getPackets b of
--     Left err   -> fail err
--     Right pkts -> return pkts


-- isFlagSet :: Bits a => a -> a -> Bool
-- isFlagSet f v = (f .&. v) == f

-- bufferSize :: Num a => a
-- bufferSize = 4096
