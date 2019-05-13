module Main where

import qualified Data.Map                      as M
import qualified Helpers                       as H
import           Control.Arrow                  ( (&&&) )
import           Control.Monad                  ( join )
import           Data.List                      ( intersperse )
import           Data.Map                       ( Map )
import           System.Environment             ( getArgs )

includeFiles :: [String]
includeFiles =
  [ "sys/types.h"
  , "sys/socket.h"
  , "linux/if.h"
  , "linux/if_tun.h"
  , "linux/if_arp.h"
  , "linux/if_link.h"
  , "linux/netlink.h"
  , "linux/rtnetlink.h"
  , "linux/neighbour.h"
  , "linux/veth.h"
  , "linux/genetlink.h"
  ]

outputs
  :: Map String Integer -> [Map String Integer] -> ([[String]], [[String]])
outputs d e =
  let define r = H.selectDefines r d
      enum r = H.selectEnum r e
  in  map fst
      &&& map snd
      $   [ H.mkADT "AddressFamily" $ define "^AF_"
          , H.mkADT "NetlinkFamily" $ define "^NETLINK_"
          , H.mkADT "MessageType" $ define "^NLMSG_(?!ALIGNTO)"
          , H.mkADT "MessageFlags" $ define "^NLM_F_"
          , H.mkADT "LinkType" $ define "^ARPHRD_"
          , H.mkADT "LinkFlags" $ M.union (define "^IFF_") (enum "^IFF_")
            -- route netlink
          , H.mkADT "RouteMessageType" $ enum "^RTM_"
          , H.mkADT "RouteMessageFlags" $ define "^RTM_F_"
          , H.mkADT "RouteScope" $ enum "^RT_SCOPE_"
          , H.mkADT "RouteTableId" $ enum "^RT_TABLE_"
          , H.mkADT "RouteProto" $ define "^RTPROT_"
          , H.mkADT "RouteType" $ enum "^RTN_"
          , H.mkADT "RouteLinkAttrType" $ enum "^IFLA_"
          , H.mkADT "RouteLinkAttrInfoType" $ enum "^IFLA_INFO_"
          , H.mkADT "RouteAddrAttrType" $ enum "^IFA_"
          , H.mkADT "RouteAddrFlags" $ define "^IFA_F_"
          , H.mkADT "RouteAttrType" $ enum "^RTA_"
          , H.mkADT "RouteNeighAttrType" $ enum "^NDA_"
          , H.mkADT "RouteNeighStateFlags" $ define "^NUD_"
          , H.mkADT "RouteNetlinkGroups" $ enum "^RTNLGRP_"
          , H.mkADT "VethType" $ enum "^VETH_INFO_"
            -- genetlink
          , H.mkADT "ControlCommand" $ enum "^CTRL_CMD_[^_]+"
          , H.mkADT "ControlAttr" $ enum "^CTRL_ATTR_"
          , H.mkADT "ControlAttrOp" $ enum "^CTRL_ATTR_OP_"
          , H.mkADT "ControlAttrMCast" $ enum "^CTRL_ATTR_MCAST_"
          ]

main :: IO ()
main = do
  [out] <- getArgs
  let inc = H.mkIncludeBlock includeFiles
  defines <- H.getDefinitions inc
  enums   <- H.getEnums inc
  let (exports, definitions) = outputs defines enums
      prelude =
        [ "{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}"
        , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
        , "module System.Linux.Netlink.Constants ("
          ++ join (intersperse ", " $ join exports)
          ++ ") where"
        , ""
        ]
  writeFile out $ unlines (prelude ++ join definitions)
