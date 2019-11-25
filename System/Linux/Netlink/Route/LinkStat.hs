{-# LANGUAGE CPP #-}
{-|
Module      : System.Linux.Netlink.Route.LinkStat
Description : The implementation for netlinks route linkstat portion
Maintainer  : ongy
Stability   : testing
Portability : Linux
-}
module System.Linux.Netlink.Route.LinkStat
  ( LinkStat
  , getLinkStat64
  , putLinkStat64
  , getLinkStat32
  , putLinkStat32
  )
where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)

import System.Linux.Netlink.Helpers (g64, p64, g32, p32)
import Data.Word (Word64)

-- | Data Structure for rtnl link stats
data LinkStat = LinkStat {
    rxPackets         :: Word64    -- ^ Total packets received.
  , txPackets         :: Word64    -- ^ Total packets transmitted.
  , rxBytes           :: Word64    -- ^ Total bytes received.
  , txBytes           :: Word64    -- ^ Total bytes transmitted.
  , rxErrors          :: Word64    -- ^ Bad packets received.
  , txErrors          :: Word64    -- ^ Packet transmission problems.
  , rxDropped         :: Word64    -- ^ Dropped due to full buffers.
  , txDropped         :: Word64    -- ^ Out of memory.
  , multicast         :: Word64    -- ^ Multicast packets received.
  , collisions        :: Word64    -- ^ Packet collisions.

  , rxLengthErrors    :: Word64    -- ^ Size/header mismatch.
  , rxOverErrors      :: Word64    -- ^ Receive ring-buffer overflow.
  , rxCRCErrors       :: Word64    -- ^ CRC errors.
  , rxFrameErrors     :: Word64    -- ^ Frame-alignment errors.
  , rxFifoErrors      :: Word64    -- ^ Receiver FIFO overrun.
  , rxMissedErrors    :: Word64    -- ^ Receiver missed packets.


  , txAbortedErrors   :: Word64
  , txCarrierErrors   :: Word64
  , txFifoErrors      :: Word64
  , txHeartbeatErrors :: Word64
  , txWindowErrors    :: Word64

  , rxCompressed      :: Word64
  , txCompressed      :: Word64
  , rxNoHandler       :: Word64    -- ^ Dropped due to lack of handler.
} deriving (Eq, Show)


-- |Get a 'LinkStat' object from a 64bit C struct
getLinkStat64 :: Get LinkStat
getLinkStat64 = getLinkStat g64

-- |Put a 'LinkStat' object into a 64bit C struct
putLinkStat64 :: LinkStat -> Put
putLinkStat64 = putLinkStat p64

-- |Get a 'LinkStat' object from a 32bit C struct
getLinkStat32 :: Get LinkStat
getLinkStat32 = getLinkStat g32

-- |Put a 'LinkStat' object into a 32bit C struct
putLinkStat32 :: LinkStat -> Put
putLinkStat32 = putLinkStat p32


-- Internal helper functions:
getLinkStat :: Integral a => Get a -> Get LinkStat
getLinkStat get = do
  let g = fromIntegral <$> get
  LinkStat
            <$>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g
            <*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g<*>g

putLinkStat :: Num a => (a -> Put) -> LinkStat -> Put
putLinkStat put msg = do
  let p = put . fromIntegral
  p $ rxPackets msg
  p $ txPackets msg
  p $ rxBytes msg
  p $ txBytes msg
  p $ rxErrors msg
  p $ txErrors msg
  p $ rxDropped msg
  p $ txDropped msg
  p $ multicast msg
  p $ collisions msg

  p $ rxLengthErrors msg
  p $ rxOverErrors msg
  p $ rxCRCErrors msg
  p $ rxFrameErrors msg
  p $ rxFifoErrors msg
  p $ rxMissedErrors msg

  p $ txAbortedErrors msg
  p $ txCarrierErrors msg
  p $ txFifoErrors msg
  p $ txHeartbeatErrors msg
  p $ txWindowErrors msg

  p $ rxCompressed msg
  p $ txCompressed msg
  p $ rxNoHandler