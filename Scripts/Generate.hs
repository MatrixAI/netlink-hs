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
    [mkEnum "AddressFamily" $ define "^AF_",
     mkEnum "MessageType" $ define "^NLMSG_(?!ALIGNTO)",
     -- these are families
     -- RouteFamily really... so it's the family header
     -- but it's the message type under the Route family
     --
     mkEnum "RouteMessageType" $ enum "^RTM_",

     mkFlag "MessageFlags"  $ define "^NLM_F_",
     mkEnum "LinkType"      $ define "^ARPHRD_",
     mkFlag "LinkFlags"     $
       union (define "^IFF_") (enum "^IFF_"),
     mkEnum "LinkAttrType"  $ enum   "^IFLA_",
     mkEnum "LinkAttrInfoType" $ enum "^IFLA_INFO_",
     mkFlag "AddrFlags"     $ define "^IFA_F_",
     mkEnum "Scope"         $ enum   "^RT_SCOPE_",
     mkEnum "AddrAttrType"  $ enum   "^IFA_",
     mkEnum "RouteTableId"  $ enum   "^RT_TABLE_",
     mkEnum "RouteProto"    $ define "^RTPROT_",
     mkEnum "RouteType"     $ enum   "^RTN_",
     mkFlag "RouteFlags"    $ define "^RTM_F_",
     mkEnum "RouteAttrType" $ enum   "^RTA_",
     mkEnum "NeighAttrType" $ enum   "^NDA_",
     mkFlag "NeighStateFlags" $ define   "^NUD_",
     mkEnum "VethAttrInfoType" $ enum "^VETH_INFO_",
     mkEnum "NetlinkFamily" $ define   "^NETLINK_",
     mkEnum "RtNetlinkGroups" $ enum   "^RTNLGRP_"]

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

