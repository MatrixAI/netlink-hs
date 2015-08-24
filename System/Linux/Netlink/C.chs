{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module System.Linux.Netlink.C
    (
      NetlinkSocket
    , makeSocket
    , makeSocketGeneric
    , closeSocket
    , sendmsg
    , recvmsg
    , joinMulticastGroup

    , cFromEnum
    , cToEnum
    ) where

import Control.Applicative ((<$>), (<*))
import Control.Monad (when)
import Data.Bits ((.|.), shiftL)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (createAndTrim, toForeignPtr)
import Data.Unique (hashUnique, newUnique)
import Data.Word (Word32)
import Foreign.C.Error (throwErrnoIf, throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.Types
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import System.Posix.Process (getProcessID)

import System.Linux.Netlink.Constants (eAF_NETLINK)

#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <linux/netlink.h>

#c
typedef struct msghdr msdhdr;
typedef struct iovec iovec;
typedef struct sockaddr_nl sockaddr_nl;
#endc

newtype NetlinkSocket = NS CInt

makeSocket :: IO NetlinkSocket
makeSocket = do
    print "Test version"
    (NS fd) <- makeSocketGeneric (cFromEnum Route)
    unique <- fromIntegral . hashUnique <$> newUnique
    pid <- fromIntegral <$> getProcessID
    let sockId = (unique `shiftL` 16) .|. pid
    with (SockAddrNetlink sockId) $ \addr ->
        throwErrnoIfMinus1_ "makeSocket.bind" $ do
            {#call bind #} fd (castPtr addr) {#sizeof sockaddr_nl #}
    return $ NS fd

makeSocketGeneric :: Int -> IO NetlinkSocket
makeSocketGeneric prot = do
  fd <- throwErrnoIfMinus1 "makeSocket.socket" $
          ({#call socket #}
           eAF_NETLINK
           (cFromEnum Raw)
           (fromIntegral prot))
  return $ NS fd

closeSocket :: NetlinkSocket -> IO ()
closeSocket (NS fd) = throwErrnoIfMinus1_ "closeSocket" $ {#call close #} fd

sendmsg :: NetlinkSocket -> [ByteString] -> IO ()
sendmsg (NS fd) bs =
    useManyAsPtrLen bs $ \ptrs ->
    withArrayLen (map IoVec ptrs) $ \iovlen iov ->
    with (MsgHdr (castPtr iov, iovlen)) $ \msg ->
    throwErrnoIfMinus1_ "sendmsg" $ do
        {#call sendmsg as _sendmsg #} fd (castPtr msg) (0 :: CInt)

recvmsg :: NetlinkSocket -> Int -> IO ByteString
recvmsg (NS fd) len =
    createAndTrim len $ \ptr ->
    with (IoVec (castPtr ptr, len)) $ \vec ->
    with (MsgHdr (castPtr vec, 1)) $ \msg ->
    fmap fromIntegral . throwErrnoIf (<= 0) "recvmsg" $ do
        {#call recvmsg as _recvmsg #} fd (castPtr msg) (0 :: CInt)

{#enum define PF { NETLINK_ROUTE as Route } #}
{#enum define ST { SOCK_RAW as Raw } #}

data IoVec = IoVec (Ptr (), Int)

instance Storable IoVec where
    sizeOf    _ = {#sizeof iovec #}
    alignment _ = 4
    peek p = do
        addr <- {#get iovec.iov_base #} p
        len  <- {#get iovec.iov_len #}  p
        return $ IoVec (addr, (fromIntegral len))
    poke p (IoVec (addr, len)) = do
        zero p
        {#set iovec.iov_base #} p addr
        {#set iovec.iov_len  #} p (fromIntegral len)

data MsgHdr = MsgHdr (Ptr (), Int)

instance Storable MsgHdr where
    sizeOf    _ = {#sizeof iovec #}
    alignment _ = 4
    peek p = do
        iov     <- {#get msghdr.msg_iov     #} p
        iovlen  <- {#get msghdr.msg_iovlen  #} p
        return $ MsgHdr (iov, fromIntegral iovlen)
    poke p (MsgHdr (iov, iovlen)) = do
        zero p
        {#set msghdr.msg_iov     #} p iov
        {#set msghdr.msg_iovlen  #} p (fromIntegral iovlen)

data SockAddrNetlink = SockAddrNetlink Word32

instance Storable SockAddrNetlink where
    sizeOf    _ = {#sizeof sockaddr_nl #}
    alignment _ = 4
    peek p = do
        family <- {#get sockaddr_nl.nl_family #} p
        when (family /= eAF_NETLINK) $ fail "Bad address family"
        SockAddrNetlink . fromIntegral <$> {#get sockaddr_nl.nl_pid #} p
    poke p (SockAddrNetlink pid) = do
        zero p
        {#set sockaddr_nl.nl_family #} p eAF_NETLINK
        {#set sockaddr_nl.nl_pid    #} p (fromIntegral pid)

useManyAsPtrLen :: [ByteString] -> ([(Ptr (), Int)] -> IO a) -> IO a
useManyAsPtrLen bs act =
    let makePtrLen (fptr, off, len) =
            let ptr = plusPtr (unsafeForeignPtrToPtr fptr) off
            in (ptr, len)
        touchByteStringPtr (fptr, _, _) = touchForeignPtr fptr
        foreigns = map toForeignPtr bs
    in act (map makePtrLen foreigns) <* mapM_ touchByteStringPtr foreigns

sizeOfPtr :: (Storable a, Integral b) => Ptr a -> b
sizeOfPtr = fromIntegral . sizeOf . (undefined :: Ptr a -> a)

zero :: Storable a => Ptr a -> IO ()
zero p = void $ {#call memset #} (castPtr p) 0 (sizeOfPtr p)

void :: Monad m => m a -> m ()
void act = act >> return ()

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum = fromIntegral . fromEnum

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum = toEnum . fromIntegral

joinMulticastGroup :: NetlinkSocket -> Word32 -> IO ()
joinMulticastGroup (NS fd) fid = do
  _ <- throwErrnoIfMinus1 "joinMulticast" $alloca ( \ptr -> do
    poke ptr fid
    {#call setsockopt as _setsockopt #} fd sol_netlink 1 (castPtr ptr) size)
  return ()
  where size = (fromIntegral $sizeOf (undefined :: CInt))
        sol_netlink = 270 :: CInt
