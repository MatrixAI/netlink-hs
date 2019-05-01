-- this script performs some generation
-- on the haskell code...
-- i'm hoping instead of constant numbers
-- we generate data types
-- and then derive Enum instead...

module Main where

import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.List (intersperse)
import Data.Map (Map, union)
import System.Environment (getArgs)

import Helpers

main :: IO ()
main = do
    [out] <- getArgs
    let inc = mkIncludeBlock includeFiles
    defines <- getDefinitions inc
    enums <- getEnums inc
    let (exports, definitions) = outputs defines enums
        prelude = [
            "{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}",
            "{-# LANGUAGE GeneralizedNewtypeDeriving #-}",
            "module System.Linux.Netlink.Constants (" ++
            join (intersperse ", " $ join exports) ++
            ") where",
            "",
            "import Data.Bits",
            ""]
    writeFile out $ unlines (prelude ++ join definitions)

outputs :: Map String Integer -> [Map String Integer] -> ([[String]], [[String]])
outputs d e = let define r = selectDefines r d
                  enum r = selectEnum r e
              in map fst &&& map snd $
    [mkADT "AddressFamily" $ define "^AF_",
     mkADT "NetlinkFamily" $ define   "^NETLINK_",
     mkADT "MessageType" $ define "^NLMSG_(?!ALIGNTO)",
     mkADT "MessageFlags"  $ define "^NLM_F_",
     mkADT "LinkType"      $ define "^ARPHRD_",
     mkADT "LinkFlags" $ union (define "^IFF_") (enum "^IFF_"),
     -- route netlink
     mkADT "RouteMessageType" $ enum "^RTM_",
     mkADT "RouteMessageFlags"    $ define "^RTM_F_",
     mkADT "RouteScope"         $ enum   "^RT_SCOPE_",
     mkADT "RouteTableId"  $ enum   "^RT_TABLE_",
     mkADT "RouteProto"    $ define "^RTPROT_",
     mkADT "RouteType"     $ enum   "^RTN_",
     mkADT "RouteLinkAttrType"  $ enum   "^IFLA_",
     mkADT "RouteLinkAttrInfoType" $ enum "^IFLA_INFO_",
     mkADT "RouteAddrAttrType"  $ enum   "^IFA_",
     mkADT "RouteAddrFlags"     $ define "^IFA_F_",
     mkADT "RouteAttrType" $ enum   "^RTA_",
     mkADT "RouteNeighAttrType" $ enum   "^NDA_",
     mkADT "RouteNeighStateFlags" $ define   "^NUD_",
     mkADT "RouteNetlinkGroups" $ enum   "^RTNLGRP_",
     mkADT "VethType" $ enum "^VETH_INFO_"]

includeFiles :: [String]
includeFiles = [ "sys/types.h"
               , "sys/socket.h"
               , "linux/if.h"
               , "linux/if_tun.h"
               , "linux/if_arp.h"
               , "linux/if_link.h"
               , "linux/netlink.h"
               , "linux/rtnetlink.h"
               , "linux/neighbour.h"
               , "linux/veth.h"
               ]

