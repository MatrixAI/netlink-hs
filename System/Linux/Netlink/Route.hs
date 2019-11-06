{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module System.Linux.Netlink.Route where

import Data.Serialize.Get (Get, skip)
import Data.Serialize.Put (Put)
import qualified Data.Serialize.Get as SG
import qualified Data.Serialize.Put as SP
import qualified Data.List
import Data.ByteString.Char8 (ByteString, append, init, pack, unpack)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int32)
import qualified Data.List.NonEmpty as NE
import qualified System.Linux.Netlink as NL
import qualified System.Linux.Netlink.Constants as NLC
import System.Linux.Netlink.Helpers (g8, g16, g32, p8, p16, p32)
import Data.Char (chr, ord)
import Prelude hiding (init, lookup)

type RouteMessage = NL.Message RouteHeader

data RouteHeader =
  RouteHeaderLink
  { linkType :: NLC.LinkType
  , linkIndex :: Word32
  , linkFlags :: Word32
  } |
  RouteHeaderAddr
  { addrFamily :: NLC.AddressFamily
  , addrMaskLength :: Word8
  , addrFlags :: Word8
  , addrScope :: Word8
  , addrLinkIndex :: Word32
  } |
  RouteHeaderNeigh
  { neighFamily :: Word8
  , neighLinkIndex :: Int32
  , neighState :: Word16
  , neighFlags :: Word8
  , neighType :: Word8
  }
  deriving (Show, Eq)

instance NL.FamilyHeader RouteHeader where
  getFamilyHeader = getRouteHeader
  putFamilyHeader = putRouteHeader


-- pattern matching only works on constructors
-- for constants we need to instead...
-- what if theey were in fact constructors
-- that would be the right way to go ahead though
-- data ... = RTM_NEWLINK | ...
-- and then something else that converted to integral using enum
-- but deriving Enum is interesting as that means you need to give it numbers either way
-- but we sure do know how to do this right!?

getRouteHeader :: NL.MessageType -> Get RouteHeader
getRouteHeader (NL.Route msgType)
 | msgType == NLC.RTM_NEWLINK = getRouteHeaderLink
 | msgType == NLC.RTM_GETLINK = getRouteHeaderLink
 | msgType == NLC.RTM_DELLINK = getRouteHeaderLink
 | msgType == NLC.RTM_NEWADDR = getRouteHeaderAddr
 | msgType == NLC.RTM_GETADDR = getRouteHeaderAddr
 | msgType == NLC.RTM_DELADDR = getRouteHeaderAddr
 | msgType == NLC.RTM_NEWNEIGH = getRouteHeaderNeigh
 | msgType == NLC.RTM_GETNEIGH = getRouteHeaderNeigh
 | msgType == NLC.RTM_DELNEIGH = getRouteHeaderNeigh
 | otherwise = error $ "Cannot decode message " ++ show msgType


getRouteHeaderLink :: Get RouteHeader
getRouteHeaderLink = do
    skip 2
    ty    <- (toEnum . fromIntegral) <$> SG.getWord16host
    index <- SG.getWord32host
    flags <- SG.getWord32host
    skip 4
    pure $ RouteHeaderLink ty index flags

getRouteHeaderAddr :: Get RouteHeader
getRouteHeaderAddr = do
    family <- (toEnum . fromIntegral) <$> SG.getWord8  -- Address type  (AF_INET or AF_INET6)
    maskLength <- SG.getWord8                          -- Prefixlength of address
    flags <- SG.getWord8                               -- Address flags
    scope <- fromIntegral <$> SG.getWord8              -- Address scope
    index <- SG.getWord32host                             -- Interface index
    return $ RouteHeaderAddr family maskLength flags scope index

getRouteHeaderNeigh :: Get RouteHeader
getRouteHeaderNeigh = RouteHeaderNeigh
    <$> SG.getWord8
    <*> (skip 3 >> fromIntegral <$> SG.getWord32host)
    <*> SG.getWord16host
    <*> SG.getWord8
    <*> SG.getWord8

-- struct ifinfomsg
putRouteHeader :: RouteHeader -> Put
putRouteHeader (RouteHeaderLink deviceType index flags) = do
    SP.putWord8 (fromIntegral $ fromEnum NLC.AF_UNSPEC) >> p8 0  -- AF_UNSPEC  -- p8 0 >> p8 0
    SP.putWord16host (fromIntegral $ fromEnum deviceType)  -- Device type
    SP.putWord32host index                                 -- Interface index
    SP.putWord32host flags                                 -- Device flags
    SP.putWord32host ifiChange                             -- change mask
    where ifiChange = 0xFFFFFFFF                           -- ifi_change is reserved for future use and should be always set to 0xFFFFFFFF.
-- struct ifaddrmsg
putRouteHeader (RouteHeaderAddr family maskLength flags scope index) = do
    SP.putWord8 (fromIntegral $ fromEnum family)  -- Address type
    SP.putWord8 maskLength                        -- Prefixlength of address
    SP.putWord8 flags                             -- Address flags
    SP.putWord8 (fromIntegral scope)              -- Address scope
    SP.putWord32host index                        -- Interface index
--  struct ndmsg
putRouteHeader (RouteHeaderNeigh f i s fl t) = do
    SP.putWord8 f                                   -- ndm_family (u8)
    SP.putWord8 0 >> SP.putWord8 0 >> SP.putWord8 0 -- padding: ndm_pad1(u8) + ndm_pad2(u16)
    SP.putWord32host (fromIntegral i)               -- ndm_ifindex (s32)
    SP.putWord16host s                              -- ndm_state (u16)
    SP.putWord8 fl                                  -- ndm_flags (u8)
    SP.putWord8 t                                   -- ndm_type (u8)

-- we say that these types expose the particular kind of
-- type?
-- ok I see...
-- alternatively message
-- we state
-- messageFamily :: FamilyHeader
-- data FamilyHeader = ...
-- but it's not extensible
-- so we need to use typeclasses instead
-- so you can easily add new families instead

-- getRouteMessages :: ByteString -> Either String [RoutePacket]

-- -- |'Get' a route message or an error
-- getRoutePackets :: ByteString -> Either String [RoutePacket]
-- getRoutePackets = getPackets

data RouteAttrType = Addr NLC.RouteAddrAttrType
                   | Link NLC.RouteLinkAttrType
                   | Neigh NLC.RouteNeighAttrType
                   deriving (Show, Eq)

type RouteAttribute = (RouteAttrType, ByteString)

type RouteAttributes = [RouteAttribute]

-- |typedef for utility functions
type AttributeReader a = RouteAttributes -> Maybe a

-- |typedef for utility functions
type AttributeWriter a = a -> RouteAttributes -> RouteAttributes

lookup :: RouteAttrType -> RouteAttributes -> Maybe ByteString
lookup x attrs = Data.List.lookup x attrs

insert :: RouteAttrType -> ByteString -> RouteAttributes -> RouteAttributes
insert a b attrs = (a, b) : attrs
--
-- Link message attributes
--
type LinkAddress = (Word8, Word8, Word8, Word8, Word8, Word8)

-- | get L2 address from netlink attributes
getLinkAddress :: AttributeReader LinkAddress
getLinkAddress attrs = decodeMAC <$> lookup (Link NLC.IFLA_ADDRESS) attrs

-- | get L2 broadcast address from netlink attributes
getLinkBroadcast :: AttributeReader LinkAddress
getLinkBroadcast attrs = decodeMAC <$> lookup (Link NLC.IFLA_BROADCAST) attrs

-- | get interface name from netlink attributes
getLinkName :: AttributeReader String
getLinkName attrs = getString <$> lookup (Link NLC.IFLA_IFNAME) attrs

-- | get mtu from netlink attributes
getLinkMTU :: AttributeReader Word32
getLinkMTU attrs = get32 =<< lookup (Link NLC.IFLA_MTU) attrs

-- | I actually have no idea what QDisc is
getLinkQDisc :: AttributeReader String
getLinkQDisc attrs = getString <$> lookup (Link NLC.IFLA_QDISC) attrs

-- | I should look this up
getLinkTXQLen :: AttributeReader Word32
getLinkTXQLen attrs = get32 =<< lookup (Link NLC.IFLA_TXQLEN) attrs

-- |get interface address from netlink attributes of 'NAddrMsg'
getIFAddr :: AttributeReader ByteString
getIFAddr = lookup ( Addr NLC.IFA_ADDRESS)

-- |get L2 address from netlink attributes of 'NNeighMsg'
getLLAddr :: AttributeReader LinkAddress
getLLAddr attrs = decodeMAC <$> lookup (Neigh NLC.NDA_LLADDR) attrs

-- |get destination address from netlink attributes of 'NNeighMsg'
getDstAddr :: AttributeReader ByteString
getDstAddr = lookup (Neigh NLC.NDA_DST)

-- | set L2 address on netlink attributes
putLinkAddress :: AttributeWriter LinkAddress
putLinkAddress addr = insert (Link NLC.IFLA_ADDRESS) (encodeMAC addr)

-- | set L2 broadcast address on netlink attributes
putLinkBroadcast :: AttributeWriter LinkAddress
putLinkBroadcast addr = insert (Link NLC.IFLA_BROADCAST) (encodeMAC addr)

-- | set interface name on netlink attributes
putLinkName :: AttributeWriter String
putLinkName ifname = insert (Link NLC.IFLA_IFNAME) (putString ifname)

-- | set mtu on netlink attributes
putLinkMTU :: AttributeWriter Word32
putLinkMTU mtu = insert (Link NLC.IFLA_MTU) (put32 mtu)

-- TODO: IFLA_LINK - need to understand what it does

-- | I actually have no idea what QDisc is
putLinkQDisc :: AttributeWriter String
putLinkQDisc disc = insert (Link NLC.IFLA_QDISC) (putString disc)

-- TODO: IFLA_STATS - bloody huge message, will deal with it later.

-- TODO: IFLA_{COST,PRIORITY,MASTER,WIRELESS,PROTINFO} - need to
-- understand what they do.



-- | I should look this up
putLinkTXQLen :: AttributeWriter Word32
putLinkTXQLen len = insert (Link NLC.IFLA_TXQLEN) (put32 len)

-- TODO: IFLA_{MAP,WEIGHT} - need to figure out

-- TODO: IFLA_{LINKMODE,LINKINFO} - see Documentation/networking/operstates.txt

-- TODO: IFLA_{NET_NS_PID,IFALIAS} - need to figure out

-- --
-- -- Helpers
-- --

decodeMAC :: ByteString -> LinkAddress
decodeMAC = tuplify . map (fromIntegral . ord) . unpack
  where tuplify [a,b,c,d,e,f] = (a,b,c,d,e,f)
        tuplify _ = error "Bad encoded MAC"

encodeMAC :: LinkAddress -> ByteString
encodeMAC = pack . map (chr . fromIntegral) . listify
  where listify (a,b,c,d,e,f) = [a,b,c,d,e,f]

getString :: ByteString -> String
getString b = unpack (init b)

putString :: String -> ByteString
putString s = append (pack s) "\0"

get32 :: ByteString -> Maybe Word32
get32 bs = case SG.runGet SG.getWord32host bs of
    Left  _ -> Nothing
    Right w -> Just w

put32 :: Word32 -> ByteString
put32 w = SP.runPut (SP.putWord32host w)
