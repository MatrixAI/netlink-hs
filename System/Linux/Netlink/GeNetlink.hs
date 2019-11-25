{-|
Module      : System.Linux.Netlink.GeNetlink
Description : The base module for genetlink implementations
Maintainer  : ongy
Stability   : testing
Portability : Linux

GeNetlink is used as multiplexer since netlink only supports 32 families.

This module provides the basic datatypes used by genetlink.
-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module System.Linux.Netlink.GeNetlink
where

import Data.List (intersperse)
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import qualified System.Linux.Netlink.C as C
-- Hide makeSocket since we will defien our own
import System.Linux.Netlink hiding (makeSocket)

{- |The static data used by genetlink

For more information about genetlink look into /usr/include/linux/genetlink.h
-}
data GenlHeader = GenlHeader
    {
      genlCmd     :: Word8
    , genlVersion :: Word8
    } deriving (Eq)

instance FamilyHeader GenlHeader where
  getFamilyHeader = getGenlHeader
  putFamilyHeader = putGeHeader

{- |A wrapper around 'GenlHeader'

This may be used by actual implementations to handle additional static data
placed after the genl header by the protocol they implement.
-}
data GenlData a = GenlData
    {
      genlDataHeader :: GenlHeader
    , genlDataData   :: a
    } deriving (Eq)

-- |The 'Convertable' instance for 'GenlData'
instance FamilyHeader a => FamilyHeader (GenlData a) where
  putFamilyHeader (GenlData h a) = putGeHeader h >> putFamilyHeader a
  getFamilyHeader t = do
    hdr <- getGenlHeader
    dat <- getFamilyHeader t
    return $ GenlData hdr dat

-- |Type declaration for genetlink packets
type GenlPacket a = Message (GenlData a)

-- |Show isntance of GenlHeader
instance Show GenlHeader where
  show (GenlHeader cmd ver) =
    "Header: Cmd = " ++ show cmd ++ ", Version: " ++ show ver ++ "\n"

-- |Show instance of GenlData
instance {-# OVERLAPPABLE #-} Show a => Show (GenlData a) where
  show (GenlData hdr content) =
    show hdr ++ show content

-- |Show Instance for GenlPacket
instance {-# OVERLAPPABLE #-} Show a => Show (GenlPacket a) where
  showList xs = ((concat . intersperse "===\n" . map show $ xs) ++)
  show (Message _ cus attrs) =
    "GenlPacket: " ++ show cus ++ "\n" ++
    "Attrs: \n" ++ (concat . intersperse " " . map show $ attrs)
  -- show p = show p

-- |'Get' function for 'GenlHeader'
getGenlHeader :: Get GenlHeader
getGenlHeader = do
    cmd <- getWord8
    version <- getWord8
    _ <- getWord16host
    return $ GenlHeader cmd version

-- |'Put' function for 'GenlHeader'
putGeHeader :: GenlHeader -> Put
putGeHeader gehdr = do
  putWord8 $ genlCmd gehdr
  putWord8 $ genlVersion gehdr
  putWord16host 0

-- |Open a 'NetlinkSocket'. This is the generic function
makeSocketGeneric
  :: Int -- ^The netlink family to use
  -> IO NetlinkSocket
makeSocketGeneric = fmap NS . C.makeSocketGeneric

-- | Typesafe wrapper around a 'CInt' (fd)
newtype NetlinkSocket = NS CInt
-- |'makeSocketGeneric' preapplied for genetlink family
makeSocket :: IO NetlinkSocket
makeSocket = makeSocketGeneric 16
