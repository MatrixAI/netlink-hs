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
     mkADT "MessageType" $ define "^NLMSG_(?!ALIGNTO)",
     mkADT "RouteFamilyType" $ enum "^RTM_",

     mkADT "MessageFlags"  $ define "^NLM_F_",
     mkADT "LinkType"      $ define "^ARPHRD_",
     mkADT "LinkFlags"     $
       union (define "^IFF_") (enum "^IFF_"),
     mkADT "LinkAttrType"  $ enum   "^IFLA_",
     mkADT "LinkAttrInfoType" $ enum "^IFLA_INFO_",
     mkADT "AddrFlags"     $ define "^IFA_F_",
     mkADT "Scope"         $ enum   "^RT_SCOPE_",
     mkADT "AddrAttrType"  $ enum   "^IFA_",
     mkADT "RouteTableId"  $ enum   "^RT_TABLE_",
     mkADT "RouteProto"    $ define "^RTPROT_",
     mkADT "RouteType"     $ enum   "^RTN_",
     mkADT "RouteFlags"    $ define "^RTM_F_",
     mkADT "RouteAttrType" $ enum   "^RTA_",
     mkADT "NeighAttrType" $ enum   "^NDA_",
     mkADT "NeighStateFlags" $ define   "^NUD_",
     mkADT "VethAttrInfoType" $ enum "^VETH_INFO_",
     mkADT "NetlinkFamily" $ define   "^NETLINK_",
     mkADT "RtNetlinkGroups" $ enum   "^RTNLGRP_"]

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

