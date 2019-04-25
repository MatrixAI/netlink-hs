{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Linux.Netlink.Constants (AddressFamily, MessageType, RouteMessageType, MessageFlags, fNLM_F_REQUEST, fNLM_F_MULTI, fNLM_F_ACK, fNLM_F_ECHO, fNLM_F_DUMP_INTR, fNLM_F_DUMP_FILTERED, fNLM_F_CAPPED, fNLM_F_NONREC, fNLM_F_REPLACE, fNLM_F_ROOT, fNLM_F_ACK_TLVS, fNLM_F_EXCL, fNLM_F_MATCH, fNLM_F_ATOMIC, fNLM_F_CREATE, fNLM_F_APPEND, LinkType, LinkFlags, fIFF_TUN, fIFF_UP, fIFF_BROADCAST, fIFF_TAP, fIFF_DEBUG, fIFF_LOOPBACK, fIFF_NAPI, fIFF_POINTOPOINT, fIFF_NAPI_FRAGS, fIFF_NOTRAILERS, fIFF_RUNNING, fIFF_NOARP, fIFF_MULTI_QUEUE, fIFF_PROMISC, fIFF_ALLMULTI, fIFF_ATTACH_QUEUE, fIFF_DETACH_QUEUE, fIFF_MASTER, fIFF_PERSIST, fIFF_SLAVE, fIFF_MULTICAST, fIFF_NOFILTER, fIFF_NO_PI, fIFF_ONE_QUEUE, fIFF_PORTSEL, fIFF_AUTOMEDIA, fIFF_VNET_HDR, fIFF_DYNAMIC, fIFF_TUN_EXCL, fIFF_LOWER_UP, fIFF_DORMANT, fIFF_ECHO, LinkAttrType, LinkAttrInfoType, AddrFlags, fIFA_F_SECONDARY, fIFA_F_TEMPORARY, fIFA_F_NODAD, fIFA_F_OPTIMISTIC, fIFA_F_DADFAILED, fIFA_F_HOMEADDRESS, fIFA_F_DEPRECATED, fIFA_F_TENTATIVE, fIFA_F_PERMANENT, fIFA_F_MANAGETEMPADDR, fIFA_F_NOPREFIXROUTE, fIFA_F_MCAUTOJOIN, fIFA_F_STABLE_PRIVACY, Scope, AddrAttrType, RouteTableId, RouteProto, RouteType, RouteFlags, fRTM_F_NOTIFY, fRTM_F_CLONED, fRTM_F_EQUALIZE, fRTM_F_PREFIX, fRTM_F_LOOKUP_TABLE, fRTM_F_FIB_MATCH, RouteAttrType, NeighAttrType, NeighStateFlags, fNUD_NONE, fNUD_INCOMPLETE, fNUD_REACHABLE, fNUD_STALE, fNUD_DELAY, fNUD_PROBE, fNUD_FAILED, fNUD_NOARP, fNUD_PERMANENT, VethAttrInfoType, NetlinkFamily, RtNetlinkGroups) where

import Data.Bits

data AddressFamily = AF_UNSPEC
                   | AF_FILE
                   | AF_LOCAL
                   | AF_UNIX
                   | AF_INET
                   | AF_AX25
                   | AF_IPX
                   | AF_APPLETALK
                   | AF_NETROM
                   | AF_BRIDGE
                   | AF_ATMPVC
                   | AF_X25
                   | AF_INET6
                   | AF_ROSE
                   | AF_DECnet
                   | AF_NETBEUI
                   | AF_SECURITY
                   | AF_KEY
                   | AF_NETLINK
                   | AF_ROUTE
                   | AF_PACKET
                   | AF_ASH
                   | AF_ECONET
                   | AF_ATMSVC
                   | AF_RDS
                   | AF_SNA
                   | AF_IRDA
                   | AF_PPPOX
                   | AF_WANPIPE
                   | AF_LLC
                   | AF_IB
                   | AF_MPLS
                   | AF_CAN
                   | AF_TIPC
                   | AF_BLUETOOTH
                   | AF_IUCV
                   | AF_RXRPC
                   | AF_ISDN
                   | AF_PHONET
                   | AF_IEEE802154
                   | AF_CAIF
                   | AF_ALG
                   | AF_NFC
                   | AF_VSOCK
                   | AF_KCM
                   | AF_QIPCRTR
                   | AF_SMC
                   | AF_MAX
                   deriving (Eq, Show)

instance Enum AddressFamily where
  toEnum 0 = AF_UNSPEC
  toEnum 1 = AF_FILE
  toEnum 2 = AF_INET
  toEnum 3 = AF_AX25
  toEnum 4 = AF_IPX
  toEnum 5 = AF_APPLETALK
  toEnum 6 = AF_NETROM
  toEnum 7 = AF_BRIDGE
  toEnum 8 = AF_ATMPVC
  toEnum 9 = AF_X25
  toEnum 10 = AF_INET6
  toEnum 11 = AF_ROSE
  toEnum 12 = AF_DECnet
  toEnum 13 = AF_NETBEUI
  toEnum 14 = AF_SECURITY
  toEnum 15 = AF_KEY
  toEnum 16 = AF_NETLINK
  toEnum 17 = AF_PACKET
  toEnum 18 = AF_ASH
  toEnum 19 = AF_ECONET
  toEnum 20 = AF_ATMSVC
  toEnum 21 = AF_RDS
  toEnum 22 = AF_SNA
  toEnum 23 = AF_IRDA
  toEnum 24 = AF_PPPOX
  toEnum 25 = AF_WANPIPE
  toEnum 26 = AF_LLC
  toEnum 27 = AF_IB
  toEnum 28 = AF_MPLS
  toEnum 29 = AF_CAN
  toEnum 30 = AF_TIPC
  toEnum 31 = AF_BLUETOOTH
  toEnum 32 = AF_IUCV
  toEnum 33 = AF_RXRPC
  toEnum 34 = AF_ISDN
  toEnum 35 = AF_PHONET
  toEnum 36 = AF_IEEE802154
  toEnum 37 = AF_CAIF
  toEnum 38 = AF_ALG
  toEnum 39 = AF_NFC
  toEnum 40 = AF_VSOCK
  toEnum 41 = AF_KCM
  toEnum 42 = AF_QIPCRTR
  toEnum 43 = AF_SMC
  toEnum 44 = AF_MAX
  fromEnum AF_UNSPEC = 0
  fromEnum AF_FILE = 1
  fromEnum AF_LOCAL = 1
  fromEnum AF_UNIX = 1
  fromEnum AF_INET = 2
  fromEnum AF_AX25 = 3
  fromEnum AF_IPX = 4
  fromEnum AF_APPLETALK = 5
  fromEnum AF_NETROM = 6
  fromEnum AF_BRIDGE = 7
  fromEnum AF_ATMPVC = 8
  fromEnum AF_X25 = 9
  fromEnum AF_INET6 = 10
  fromEnum AF_ROSE = 11
  fromEnum AF_DECnet = 12
  fromEnum AF_NETBEUI = 13
  fromEnum AF_SECURITY = 14
  fromEnum AF_KEY = 15
  fromEnum AF_NETLINK = 16
  fromEnum AF_ROUTE = 16
  fromEnum AF_PACKET = 17
  fromEnum AF_ASH = 18
  fromEnum AF_ECONET = 19
  fromEnum AF_ATMSVC = 20
  fromEnum AF_RDS = 21
  fromEnum AF_SNA = 22
  fromEnum AF_IRDA = 23
  fromEnum AF_PPPOX = 24
  fromEnum AF_WANPIPE = 25
  fromEnum AF_LLC = 26
  fromEnum AF_IB = 27
  fromEnum AF_MPLS = 28
  fromEnum AF_CAN = 29
  fromEnum AF_TIPC = 30
  fromEnum AF_BLUETOOTH = 31
  fromEnum AF_IUCV = 32
  fromEnum AF_RXRPC = 33
  fromEnum AF_ISDN = 34
  fromEnum AF_PHONET = 35
  fromEnum AF_IEEE802154 = 36
  fromEnum AF_CAIF = 37
  fromEnum AF_ALG = 38
  fromEnum AF_NFC = 39
  fromEnum AF_VSOCK = 40
  fromEnum AF_KCM = 41
  fromEnum AF_QIPCRTR = 42
  fromEnum AF_SMC = 43
  fromEnum AF_MAX = 44

data MessageType = NLMSG_NOOP
                 | NLMSG_ERROR
                 | NLMSG_DONE
                 | NLMSG_OVERRUN
                 | NLMSG_MIN_TYPE
                 deriving (Eq, Show)

instance Enum MessageType where
  toEnum 1 = NLMSG_NOOP
  toEnum 2 = NLMSG_ERROR
  toEnum 3 = NLMSG_DONE
  toEnum 4 = NLMSG_OVERRUN
  toEnum 16 = NLMSG_MIN_TYPE
  fromEnum NLMSG_NOOP = 1
  fromEnum NLMSG_ERROR = 2
  fromEnum NLMSG_DONE = 3
  fromEnum NLMSG_OVERRUN = 4
  fromEnum NLMSG_MIN_TYPE = 16

data RouteMessageType = RTM_BASE
                      | RTM_NEWLINK
                      | RTM_DELLINK
                      | RTM_GETLINK
                      | RTM_SETLINK
                      | RTM_NEWADDR
                      | RTM_DELADDR
                      | RTM_GETADDR
                      | RTM_NEWROUTE
                      | RTM_DELROUTE
                      | RTM_GETROUTE
                      | RTM_NEWNEIGH
                      | RTM_DELNEIGH
                      | RTM_GETNEIGH
                      | RTM_NEWRULE
                      | RTM_DELRULE
                      | RTM_GETRULE
                      | RTM_NEWQDISC
                      | RTM_DELQDISC
                      | RTM_GETQDISC
                      | RTM_NEWTCLASS
                      | RTM_DELTCLASS
                      | RTM_GETTCLASS
                      | RTM_NEWTFILTER
                      | RTM_DELTFILTER
                      | RTM_GETTFILTER
                      | RTM_NEWACTION
                      | RTM_DELACTION
                      | RTM_GETACTION
                      | RTM_NEWPREFIX
                      | RTM_GETMULTICAST
                      | RTM_GETANYCAST
                      | RTM_NEWNEIGHTBL
                      | RTM_GETNEIGHTBL
                      | RTM_SETNEIGHTBL
                      | RTM_NEWNDUSEROPT
                      | RTM_NEWADDRLABEL
                      | RTM_DELADDRLABEL
                      | RTM_GETADDRLABEL
                      | RTM_GETDCB
                      | RTM_SETDCB
                      | RTM_NEWNETCONF
                      | RTM_DELNETCONF
                      | RTM_GETNETCONF
                      | RTM_NEWMDB
                      | RTM_DELMDB
                      | RTM_GETMDB
                      | RTM_NEWNSID
                      | RTM_DELNSID
                      | RTM_GETNSID
                      | RTM_NEWSTATS
                      | RTM_GETSTATS
                      | RTM_NEWCACHEREPORT
                      deriving (Eq, Show)

instance Enum RouteMessageType where
  toEnum 16 = RTM_BASE
  toEnum 17 = RTM_DELLINK
  toEnum 18 = RTM_GETLINK
  toEnum 19 = RTM_SETLINK
  toEnum 20 = RTM_NEWADDR
  toEnum 21 = RTM_DELADDR
  toEnum 22 = RTM_GETADDR
  toEnum 24 = RTM_NEWROUTE
  toEnum 25 = RTM_DELROUTE
  toEnum 26 = RTM_GETROUTE
  toEnum 28 = RTM_NEWNEIGH
  toEnum 29 = RTM_DELNEIGH
  toEnum 30 = RTM_GETNEIGH
  toEnum 32 = RTM_NEWRULE
  toEnum 33 = RTM_DELRULE
  toEnum 34 = RTM_GETRULE
  toEnum 36 = RTM_NEWQDISC
  toEnum 37 = RTM_DELQDISC
  toEnum 38 = RTM_GETQDISC
  toEnum 40 = RTM_NEWTCLASS
  toEnum 41 = RTM_DELTCLASS
  toEnum 42 = RTM_GETTCLASS
  toEnum 44 = RTM_NEWTFILTER
  toEnum 45 = RTM_DELTFILTER
  toEnum 46 = RTM_GETTFILTER
  toEnum 48 = RTM_NEWACTION
  toEnum 49 = RTM_DELACTION
  toEnum 50 = RTM_GETACTION
  toEnum 52 = RTM_NEWPREFIX
  toEnum 58 = RTM_GETMULTICAST
  toEnum 62 = RTM_GETANYCAST
  toEnum 64 = RTM_NEWNEIGHTBL
  toEnum 66 = RTM_GETNEIGHTBL
  toEnum 67 = RTM_SETNEIGHTBL
  toEnum 68 = RTM_NEWNDUSEROPT
  toEnum 72 = RTM_NEWADDRLABEL
  toEnum 73 = RTM_DELADDRLABEL
  toEnum 74 = RTM_GETADDRLABEL
  toEnum 78 = RTM_GETDCB
  toEnum 79 = RTM_SETDCB
  toEnum 80 = RTM_NEWNETCONF
  toEnum 81 = RTM_DELNETCONF
  toEnum 82 = RTM_GETNETCONF
  toEnum 84 = RTM_NEWMDB
  toEnum 85 = RTM_DELMDB
  toEnum 86 = RTM_GETMDB
  toEnum 88 = RTM_NEWNSID
  toEnum 89 = RTM_DELNSID
  toEnum 90 = RTM_GETNSID
  toEnum 92 = RTM_NEWSTATS
  toEnum 94 = RTM_GETSTATS
  toEnum 96 = RTM_NEWCACHEREPORT
  fromEnum RTM_BASE = 16
  fromEnum RTM_NEWLINK = 16
  fromEnum RTM_DELLINK = 17
  fromEnum RTM_GETLINK = 18
  fromEnum RTM_SETLINK = 19
  fromEnum RTM_NEWADDR = 20
  fromEnum RTM_DELADDR = 21
  fromEnum RTM_GETADDR = 22
  fromEnum RTM_NEWROUTE = 24
  fromEnum RTM_DELROUTE = 25
  fromEnum RTM_GETROUTE = 26
  fromEnum RTM_NEWNEIGH = 28
  fromEnum RTM_DELNEIGH = 29
  fromEnum RTM_GETNEIGH = 30
  fromEnum RTM_NEWRULE = 32
  fromEnum RTM_DELRULE = 33
  fromEnum RTM_GETRULE = 34
  fromEnum RTM_NEWQDISC = 36
  fromEnum RTM_DELQDISC = 37
  fromEnum RTM_GETQDISC = 38
  fromEnum RTM_NEWTCLASS = 40
  fromEnum RTM_DELTCLASS = 41
  fromEnum RTM_GETTCLASS = 42
  fromEnum RTM_NEWTFILTER = 44
  fromEnum RTM_DELTFILTER = 45
  fromEnum RTM_GETTFILTER = 46
  fromEnum RTM_NEWACTION = 48
  fromEnum RTM_DELACTION = 49
  fromEnum RTM_GETACTION = 50
  fromEnum RTM_NEWPREFIX = 52
  fromEnum RTM_GETMULTICAST = 58
  fromEnum RTM_GETANYCAST = 62
  fromEnum RTM_NEWNEIGHTBL = 64
  fromEnum RTM_GETNEIGHTBL = 66
  fromEnum RTM_SETNEIGHTBL = 67
  fromEnum RTM_NEWNDUSEROPT = 68
  fromEnum RTM_NEWADDRLABEL = 72
  fromEnum RTM_DELADDRLABEL = 73
  fromEnum RTM_GETADDRLABEL = 74
  fromEnum RTM_GETDCB = 78
  fromEnum RTM_SETDCB = 79
  fromEnum RTM_NEWNETCONF = 80
  fromEnum RTM_DELNETCONF = 81
  fromEnum RTM_GETNETCONF = 82
  fromEnum RTM_NEWMDB = 84
  fromEnum RTM_DELMDB = 85
  fromEnum RTM_GETMDB = 86
  fromEnum RTM_NEWNSID = 88
  fromEnum RTM_DELNSID = 89
  fromEnum RTM_GETNSID = 90
  fromEnum RTM_NEWSTATS = 92
  fromEnum RTM_GETSTATS = 94
  fromEnum RTM_NEWCACHEREPORT = 96

newtype MessageFlags = MessageFlags Int deriving (Bits, Eq, Enum, Integral, Num, Ord, Real, Show)

fNLM_F_REQUEST :: (Num a, Bits a) => a
fNLM_F_REQUEST = 1
fNLM_F_MULTI :: (Num a, Bits a) => a
fNLM_F_MULTI = 2
fNLM_F_ACK :: (Num a, Bits a) => a
fNLM_F_ACK = 4
fNLM_F_ECHO :: (Num a, Bits a) => a
fNLM_F_ECHO = 8
fNLM_F_DUMP_INTR :: (Num a, Bits a) => a
fNLM_F_DUMP_INTR = 16
fNLM_F_DUMP_FILTERED :: (Num a, Bits a) => a
fNLM_F_DUMP_FILTERED = 32
fNLM_F_CAPPED :: (Num a, Bits a) => a
fNLM_F_CAPPED = 256
fNLM_F_NONREC :: (Num a, Bits a) => a
fNLM_F_NONREC = 256
fNLM_F_REPLACE :: (Num a, Bits a) => a
fNLM_F_REPLACE = 256
fNLM_F_ROOT :: (Num a, Bits a) => a
fNLM_F_ROOT = 256
fNLM_F_ACK_TLVS :: (Num a, Bits a) => a
fNLM_F_ACK_TLVS = 512
fNLM_F_EXCL :: (Num a, Bits a) => a
fNLM_F_EXCL = 512
fNLM_F_MATCH :: (Num a, Bits a) => a
fNLM_F_MATCH = 512
fNLM_F_ATOMIC :: (Num a, Bits a) => a
fNLM_F_ATOMIC = 1024
fNLM_F_CREATE :: (Num a, Bits a) => a
fNLM_F_CREATE = 1024
fNLM_F_APPEND :: (Num a, Bits a) => a
fNLM_F_APPEND = 2048
data LinkType = ARPHRD_NETROM
              | ARPHRD_ETHER
              | ARPHRD_EETHER
              | ARPHRD_AX25
              | ARPHRD_PRONET
              | ARPHRD_CHAOS
              | ARPHRD_IEEE802
              | ARPHRD_ARCNET
              | ARPHRD_APPLETLK
              | ARPHRD_DLCI
              | ARPHRD_ATM
              | ARPHRD_METRICOM
              | ARPHRD_IEEE1394
              | ARPHRD_EUI64
              | ARPHRD_INFINIBAND
              | ARPHRD_SLIP
              | ARPHRD_CSLIP
              | ARPHRD_SLIP6
              | ARPHRD_CSLIP6
              | ARPHRD_RSRVD
              | ARPHRD_ADAPT
              | ARPHRD_ROSE
              | ARPHRD_X25
              | ARPHRD_HWX25
              | ARPHRD_CAN
              | ARPHRD_PPP
              | ARPHRD_CISCO
              | ARPHRD_HDLC
              | ARPHRD_LAPB
              | ARPHRD_DDCMP
              | ARPHRD_RAWHDLC
              | ARPHRD_RAWIP
              | ARPHRD_TUNNEL
              | ARPHRD_TUNNEL6
              | ARPHRD_FRAD
              | ARPHRD_SKIP
              | ARPHRD_LOOPBACK
              | ARPHRD_LOCALTLK
              | ARPHRD_FDDI
              | ARPHRD_BIF
              | ARPHRD_SIT
              | ARPHRD_IPDDP
              | ARPHRD_IPGRE
              | ARPHRD_PIMREG
              | ARPHRD_HIPPI
              | ARPHRD_ASH
              | ARPHRD_ECONET
              | ARPHRD_IRDA
              | ARPHRD_FCPP
              | ARPHRD_FCAL
              | ARPHRD_FCPL
              | ARPHRD_FCFABRIC
              | ARPHRD_IEEE802_TR
              | ARPHRD_IEEE80211
              | ARPHRD_IEEE80211_PRISM
              | ARPHRD_IEEE80211_RADIOTAP
              | ARPHRD_IEEE802154
              | ARPHRD_IEEE802154_MONITOR
              | ARPHRD_PHONET
              | ARPHRD_PHONET_PIPE
              | ARPHRD_CAIF
              | ARPHRD_IP6GRE
              | ARPHRD_NETLINK
              | ARPHRD_6LOWPAN
              | ARPHRD_VSOCKMON
              deriving (Eq, Show)

instance Enum LinkType where
  toEnum 0 = ARPHRD_NETROM
  toEnum 1 = ARPHRD_ETHER
  toEnum 2 = ARPHRD_EETHER
  toEnum 3 = ARPHRD_AX25
  toEnum 4 = ARPHRD_PRONET
  toEnum 5 = ARPHRD_CHAOS
  toEnum 6 = ARPHRD_IEEE802
  toEnum 7 = ARPHRD_ARCNET
  toEnum 8 = ARPHRD_APPLETLK
  toEnum 15 = ARPHRD_DLCI
  toEnum 19 = ARPHRD_ATM
  toEnum 23 = ARPHRD_METRICOM
  toEnum 24 = ARPHRD_IEEE1394
  toEnum 27 = ARPHRD_EUI64
  toEnum 32 = ARPHRD_INFINIBAND
  toEnum 256 = ARPHRD_SLIP
  toEnum 257 = ARPHRD_CSLIP
  toEnum 258 = ARPHRD_SLIP6
  toEnum 259 = ARPHRD_CSLIP6
  toEnum 260 = ARPHRD_RSRVD
  toEnum 264 = ARPHRD_ADAPT
  toEnum 270 = ARPHRD_ROSE
  toEnum 271 = ARPHRD_X25
  toEnum 272 = ARPHRD_HWX25
  toEnum 280 = ARPHRD_CAN
  toEnum 512 = ARPHRD_PPP
  toEnum 513 = ARPHRD_CISCO
  toEnum 516 = ARPHRD_LAPB
  toEnum 517 = ARPHRD_DDCMP
  toEnum 518 = ARPHRD_RAWHDLC
  toEnum 519 = ARPHRD_RAWIP
  toEnum 768 = ARPHRD_TUNNEL
  toEnum 769 = ARPHRD_TUNNEL6
  toEnum 770 = ARPHRD_FRAD
  toEnum 771 = ARPHRD_SKIP
  toEnum 772 = ARPHRD_LOOPBACK
  toEnum 773 = ARPHRD_LOCALTLK
  toEnum 774 = ARPHRD_FDDI
  toEnum 775 = ARPHRD_BIF
  toEnum 776 = ARPHRD_SIT
  toEnum 777 = ARPHRD_IPDDP
  toEnum 778 = ARPHRD_IPGRE
  toEnum 779 = ARPHRD_PIMREG
  toEnum 780 = ARPHRD_HIPPI
  toEnum 781 = ARPHRD_ASH
  toEnum 782 = ARPHRD_ECONET
  toEnum 783 = ARPHRD_IRDA
  toEnum 784 = ARPHRD_FCPP
  toEnum 785 = ARPHRD_FCAL
  toEnum 786 = ARPHRD_FCPL
  toEnum 787 = ARPHRD_FCFABRIC
  toEnum 800 = ARPHRD_IEEE802_TR
  toEnum 801 = ARPHRD_IEEE80211
  toEnum 802 = ARPHRD_IEEE80211_PRISM
  toEnum 803 = ARPHRD_IEEE80211_RADIOTAP
  toEnum 804 = ARPHRD_IEEE802154
  toEnum 805 = ARPHRD_IEEE802154_MONITOR
  toEnum 820 = ARPHRD_PHONET
  toEnum 821 = ARPHRD_PHONET_PIPE
  toEnum 822 = ARPHRD_CAIF
  toEnum 823 = ARPHRD_IP6GRE
  toEnum 824 = ARPHRD_NETLINK
  toEnum 825 = ARPHRD_6LOWPAN
  toEnum 826 = ARPHRD_VSOCKMON
  fromEnum ARPHRD_NETROM = 0
  fromEnum ARPHRD_ETHER = 1
  fromEnum ARPHRD_EETHER = 2
  fromEnum ARPHRD_AX25 = 3
  fromEnum ARPHRD_PRONET = 4
  fromEnum ARPHRD_CHAOS = 5
  fromEnum ARPHRD_IEEE802 = 6
  fromEnum ARPHRD_ARCNET = 7
  fromEnum ARPHRD_APPLETLK = 8
  fromEnum ARPHRD_DLCI = 15
  fromEnum ARPHRD_ATM = 19
  fromEnum ARPHRD_METRICOM = 23
  fromEnum ARPHRD_IEEE1394 = 24
  fromEnum ARPHRD_EUI64 = 27
  fromEnum ARPHRD_INFINIBAND = 32
  fromEnum ARPHRD_SLIP = 256
  fromEnum ARPHRD_CSLIP = 257
  fromEnum ARPHRD_SLIP6 = 258
  fromEnum ARPHRD_CSLIP6 = 259
  fromEnum ARPHRD_RSRVD = 260
  fromEnum ARPHRD_ADAPT = 264
  fromEnum ARPHRD_ROSE = 270
  fromEnum ARPHRD_X25 = 271
  fromEnum ARPHRD_HWX25 = 272
  fromEnum ARPHRD_CAN = 280
  fromEnum ARPHRD_PPP = 512
  fromEnum ARPHRD_CISCO = 513
  fromEnum ARPHRD_HDLC = 513
  fromEnum ARPHRD_LAPB = 516
  fromEnum ARPHRD_DDCMP = 517
  fromEnum ARPHRD_RAWHDLC = 518
  fromEnum ARPHRD_RAWIP = 519
  fromEnum ARPHRD_TUNNEL = 768
  fromEnum ARPHRD_TUNNEL6 = 769
  fromEnum ARPHRD_FRAD = 770
  fromEnum ARPHRD_SKIP = 771
  fromEnum ARPHRD_LOOPBACK = 772
  fromEnum ARPHRD_LOCALTLK = 773
  fromEnum ARPHRD_FDDI = 774
  fromEnum ARPHRD_BIF = 775
  fromEnum ARPHRD_SIT = 776
  fromEnum ARPHRD_IPDDP = 777
  fromEnum ARPHRD_IPGRE = 778
  fromEnum ARPHRD_PIMREG = 779
  fromEnum ARPHRD_HIPPI = 780
  fromEnum ARPHRD_ASH = 781
  fromEnum ARPHRD_ECONET = 782
  fromEnum ARPHRD_IRDA = 783
  fromEnum ARPHRD_FCPP = 784
  fromEnum ARPHRD_FCAL = 785
  fromEnum ARPHRD_FCPL = 786
  fromEnum ARPHRD_FCFABRIC = 787
  fromEnum ARPHRD_IEEE802_TR = 800
  fromEnum ARPHRD_IEEE80211 = 801
  fromEnum ARPHRD_IEEE80211_PRISM = 802
  fromEnum ARPHRD_IEEE80211_RADIOTAP = 803
  fromEnum ARPHRD_IEEE802154 = 804
  fromEnum ARPHRD_IEEE802154_MONITOR = 805
  fromEnum ARPHRD_PHONET = 820
  fromEnum ARPHRD_PHONET_PIPE = 821
  fromEnum ARPHRD_CAIF = 822
  fromEnum ARPHRD_IP6GRE = 823
  fromEnum ARPHRD_NETLINK = 824
  fromEnum ARPHRD_6LOWPAN = 825
  fromEnum ARPHRD_VSOCKMON = 826

newtype LinkFlags = LinkFlags Int deriving (Bits, Eq, Enum, Integral, Num, Ord, Real, Show)

fIFF_TUN :: (Num a, Bits a) => a
fIFF_TUN = 1
fIFF_UP :: (Num a, Bits a) => a
fIFF_UP = 1
fIFF_BROADCAST :: (Num a, Bits a) => a
fIFF_BROADCAST = 2
fIFF_TAP :: (Num a, Bits a) => a
fIFF_TAP = 2
fIFF_DEBUG :: (Num a, Bits a) => a
fIFF_DEBUG = 4
fIFF_LOOPBACK :: (Num a, Bits a) => a
fIFF_LOOPBACK = 8
fIFF_NAPI :: (Num a, Bits a) => a
fIFF_NAPI = 16
fIFF_POINTOPOINT :: (Num a, Bits a) => a
fIFF_POINTOPOINT = 16
fIFF_NAPI_FRAGS :: (Num a, Bits a) => a
fIFF_NAPI_FRAGS = 32
fIFF_NOTRAILERS :: (Num a, Bits a) => a
fIFF_NOTRAILERS = 32
fIFF_RUNNING :: (Num a, Bits a) => a
fIFF_RUNNING = 64
fIFF_NOARP :: (Num a, Bits a) => a
fIFF_NOARP = 128
fIFF_MULTI_QUEUE :: (Num a, Bits a) => a
fIFF_MULTI_QUEUE = 256
fIFF_PROMISC :: (Num a, Bits a) => a
fIFF_PROMISC = 256
fIFF_ALLMULTI :: (Num a, Bits a) => a
fIFF_ALLMULTI = 512
fIFF_ATTACH_QUEUE :: (Num a, Bits a) => a
fIFF_ATTACH_QUEUE = 512
fIFF_DETACH_QUEUE :: (Num a, Bits a) => a
fIFF_DETACH_QUEUE = 1024
fIFF_MASTER :: (Num a, Bits a) => a
fIFF_MASTER = 1024
fIFF_PERSIST :: (Num a, Bits a) => a
fIFF_PERSIST = 2048
fIFF_SLAVE :: (Num a, Bits a) => a
fIFF_SLAVE = 2048
fIFF_MULTICAST :: (Num a, Bits a) => a
fIFF_MULTICAST = 4096
fIFF_NOFILTER :: (Num a, Bits a) => a
fIFF_NOFILTER = 4096
fIFF_NO_PI :: (Num a, Bits a) => a
fIFF_NO_PI = 4096
fIFF_ONE_QUEUE :: (Num a, Bits a) => a
fIFF_ONE_QUEUE = 8192
fIFF_PORTSEL :: (Num a, Bits a) => a
fIFF_PORTSEL = 8192
fIFF_AUTOMEDIA :: (Num a, Bits a) => a
fIFF_AUTOMEDIA = 16384
fIFF_VNET_HDR :: (Num a, Bits a) => a
fIFF_VNET_HDR = 16384
fIFF_DYNAMIC :: (Num a, Bits a) => a
fIFF_DYNAMIC = 32768
fIFF_TUN_EXCL :: (Num a, Bits a) => a
fIFF_TUN_EXCL = 32768
fIFF_LOWER_UP :: (Num a, Bits a) => a
fIFF_LOWER_UP = 65536
fIFF_DORMANT :: (Num a, Bits a) => a
fIFF_DORMANT = 131072
fIFF_ECHO :: (Num a, Bits a) => a
fIFF_ECHO = 262144
data LinkAttrType = IFLA_UNSPEC
                  | IFLA_ADDRESS
                  | IFLA_BROADCAST
                  | IFLA_IFNAME
                  | IFLA_MTU
                  | IFLA_LINK
                  | IFLA_QDISC
                  | IFLA_STATS
                  | IFLA_COST
                  | IFLA_PRIORITY
                  | IFLA_MASTER
                  | IFLA_WIRELESS
                  | IFLA_PROTINFO
                  | IFLA_TXQLEN
                  | IFLA_MAP
                  | IFLA_WEIGHT
                  | IFLA_OPERSTATE
                  | IFLA_LINKMODE
                  | IFLA_LINKINFO
                  | IFLA_NET_NS_PID
                  | IFLA_IFALIAS
                  | IFLA_NUM_VF
                  | IFLA_VFINFO_LIST
                  | IFLA_STATS64
                  | IFLA_VF_PORTS
                  | IFLA_PORT_SELF
                  | IFLA_AF_SPEC
                  | IFLA_GROUP
                  | IFLA_NET_NS_FD
                  | IFLA_EXT_MASK
                  | IFLA_PROMISCUITY
                  | IFLA_NUM_TX_QUEUES
                  | IFLA_NUM_RX_QUEUES
                  | IFLA_CARRIER
                  | IFLA_PHYS_PORT_ID
                  | IFLA_CARRIER_CHANGES
                  | IFLA_PHYS_SWITCH_ID
                  | IFLA_LINK_NETNSID
                  | IFLA_PHYS_PORT_NAME
                  | IFLA_PROTO_DOWN
                  | IFLA_GSO_MAX_SEGS
                  | IFLA_GSO_MAX_SIZE
                  | IFLA_PAD
                  | IFLA_XDP
                  | IFLA_EVENT
                  | IFLA_NEW_NETNSID
                  | IFLA_IF_NETNSID
                  | IFLA_CARRIER_UP_COUNT
                  | IFLA_CARRIER_DOWN_COUNT
                  | IFLA_NEW_IFINDEX
                  deriving (Eq, Show)

instance Enum LinkAttrType where
  toEnum 0 = IFLA_UNSPEC
  toEnum 1 = IFLA_ADDRESS
  toEnum 2 = IFLA_BROADCAST
  toEnum 3 = IFLA_IFNAME
  toEnum 4 = IFLA_MTU
  toEnum 5 = IFLA_LINK
  toEnum 6 = IFLA_QDISC
  toEnum 7 = IFLA_STATS
  toEnum 8 = IFLA_COST
  toEnum 9 = IFLA_PRIORITY
  toEnum 10 = IFLA_MASTER
  toEnum 11 = IFLA_WIRELESS
  toEnum 12 = IFLA_PROTINFO
  toEnum 13 = IFLA_TXQLEN
  toEnum 14 = IFLA_MAP
  toEnum 15 = IFLA_WEIGHT
  toEnum 16 = IFLA_OPERSTATE
  toEnum 17 = IFLA_LINKMODE
  toEnum 18 = IFLA_LINKINFO
  toEnum 19 = IFLA_NET_NS_PID
  toEnum 20 = IFLA_IFALIAS
  toEnum 21 = IFLA_NUM_VF
  toEnum 22 = IFLA_VFINFO_LIST
  toEnum 23 = IFLA_STATS64
  toEnum 24 = IFLA_VF_PORTS
  toEnum 25 = IFLA_PORT_SELF
  toEnum 26 = IFLA_AF_SPEC
  toEnum 27 = IFLA_GROUP
  toEnum 28 = IFLA_NET_NS_FD
  toEnum 29 = IFLA_EXT_MASK
  toEnum 30 = IFLA_PROMISCUITY
  toEnum 31 = IFLA_NUM_TX_QUEUES
  toEnum 32 = IFLA_NUM_RX_QUEUES
  toEnum 33 = IFLA_CARRIER
  toEnum 34 = IFLA_PHYS_PORT_ID
  toEnum 35 = IFLA_CARRIER_CHANGES
  toEnum 36 = IFLA_PHYS_SWITCH_ID
  toEnum 37 = IFLA_LINK_NETNSID
  toEnum 38 = IFLA_PHYS_PORT_NAME
  toEnum 39 = IFLA_PROTO_DOWN
  toEnum 40 = IFLA_GSO_MAX_SEGS
  toEnum 41 = IFLA_GSO_MAX_SIZE
  toEnum 42 = IFLA_PAD
  toEnum 43 = IFLA_XDP
  toEnum 44 = IFLA_EVENT
  toEnum 45 = IFLA_NEW_NETNSID
  toEnum 46 = IFLA_IF_NETNSID
  toEnum 47 = IFLA_CARRIER_UP_COUNT
  toEnum 48 = IFLA_CARRIER_DOWN_COUNT
  toEnum 49 = IFLA_NEW_IFINDEX
  fromEnum IFLA_UNSPEC = 0
  fromEnum IFLA_ADDRESS = 1
  fromEnum IFLA_BROADCAST = 2
  fromEnum IFLA_IFNAME = 3
  fromEnum IFLA_MTU = 4
  fromEnum IFLA_LINK = 5
  fromEnum IFLA_QDISC = 6
  fromEnum IFLA_STATS = 7
  fromEnum IFLA_COST = 8
  fromEnum IFLA_PRIORITY = 9
  fromEnum IFLA_MASTER = 10
  fromEnum IFLA_WIRELESS = 11
  fromEnum IFLA_PROTINFO = 12
  fromEnum IFLA_TXQLEN = 13
  fromEnum IFLA_MAP = 14
  fromEnum IFLA_WEIGHT = 15
  fromEnum IFLA_OPERSTATE = 16
  fromEnum IFLA_LINKMODE = 17
  fromEnum IFLA_LINKINFO = 18
  fromEnum IFLA_NET_NS_PID = 19
  fromEnum IFLA_IFALIAS = 20
  fromEnum IFLA_NUM_VF = 21
  fromEnum IFLA_VFINFO_LIST = 22
  fromEnum IFLA_STATS64 = 23
  fromEnum IFLA_VF_PORTS = 24
  fromEnum IFLA_PORT_SELF = 25
  fromEnum IFLA_AF_SPEC = 26
  fromEnum IFLA_GROUP = 27
  fromEnum IFLA_NET_NS_FD = 28
  fromEnum IFLA_EXT_MASK = 29
  fromEnum IFLA_PROMISCUITY = 30
  fromEnum IFLA_NUM_TX_QUEUES = 31
  fromEnum IFLA_NUM_RX_QUEUES = 32
  fromEnum IFLA_CARRIER = 33
  fromEnum IFLA_PHYS_PORT_ID = 34
  fromEnum IFLA_CARRIER_CHANGES = 35
  fromEnum IFLA_PHYS_SWITCH_ID = 36
  fromEnum IFLA_LINK_NETNSID = 37
  fromEnum IFLA_PHYS_PORT_NAME = 38
  fromEnum IFLA_PROTO_DOWN = 39
  fromEnum IFLA_GSO_MAX_SEGS = 40
  fromEnum IFLA_GSO_MAX_SIZE = 41
  fromEnum IFLA_PAD = 42
  fromEnum IFLA_XDP = 43
  fromEnum IFLA_EVENT = 44
  fromEnum IFLA_NEW_NETNSID = 45
  fromEnum IFLA_IF_NETNSID = 46
  fromEnum IFLA_CARRIER_UP_COUNT = 47
  fromEnum IFLA_CARRIER_DOWN_COUNT = 48
  fromEnum IFLA_NEW_IFINDEX = 49

data LinkAttrInfoType = IFLA_INFO_UNSPEC
                      | IFLA_INFO_KIND
                      | IFLA_INFO_DATA
                      | IFLA_INFO_XSTATS
                      | IFLA_INFO_SLAVE_KIND
                      | IFLA_INFO_SLAVE_DATA
                      deriving (Eq, Show)

instance Enum LinkAttrInfoType where
  toEnum 0 = IFLA_INFO_UNSPEC
  toEnum 1 = IFLA_INFO_KIND
  toEnum 2 = IFLA_INFO_DATA
  toEnum 3 = IFLA_INFO_XSTATS
  toEnum 4 = IFLA_INFO_SLAVE_KIND
  toEnum 5 = IFLA_INFO_SLAVE_DATA
  fromEnum IFLA_INFO_UNSPEC = 0
  fromEnum IFLA_INFO_KIND = 1
  fromEnum IFLA_INFO_DATA = 2
  fromEnum IFLA_INFO_XSTATS = 3
  fromEnum IFLA_INFO_SLAVE_KIND = 4
  fromEnum IFLA_INFO_SLAVE_DATA = 5

newtype AddrFlags = AddrFlags Int deriving (Bits, Eq, Enum, Integral, Num, Ord, Real, Show)

fIFA_F_SECONDARY :: (Num a, Bits a) => a
fIFA_F_SECONDARY = 1
fIFA_F_TEMPORARY :: (Num a, Bits a) => a
fIFA_F_TEMPORARY = 1
fIFA_F_NODAD :: (Num a, Bits a) => a
fIFA_F_NODAD = 2
fIFA_F_OPTIMISTIC :: (Num a, Bits a) => a
fIFA_F_OPTIMISTIC = 4
fIFA_F_DADFAILED :: (Num a, Bits a) => a
fIFA_F_DADFAILED = 8
fIFA_F_HOMEADDRESS :: (Num a, Bits a) => a
fIFA_F_HOMEADDRESS = 16
fIFA_F_DEPRECATED :: (Num a, Bits a) => a
fIFA_F_DEPRECATED = 32
fIFA_F_TENTATIVE :: (Num a, Bits a) => a
fIFA_F_TENTATIVE = 64
fIFA_F_PERMANENT :: (Num a, Bits a) => a
fIFA_F_PERMANENT = 128
fIFA_F_MANAGETEMPADDR :: (Num a, Bits a) => a
fIFA_F_MANAGETEMPADDR = 256
fIFA_F_NOPREFIXROUTE :: (Num a, Bits a) => a
fIFA_F_NOPREFIXROUTE = 512
fIFA_F_MCAUTOJOIN :: (Num a, Bits a) => a
fIFA_F_MCAUTOJOIN = 1024
fIFA_F_STABLE_PRIVACY :: (Num a, Bits a) => a
fIFA_F_STABLE_PRIVACY = 2048
data Scope = RT_SCOPE_UNIVERSE
           | RT_SCOPE_SITE
           | RT_SCOPE_LINK
           | RT_SCOPE_HOST
           | RT_SCOPE_NOWHERE
           deriving (Eq, Show)

instance Enum Scope where
  toEnum 0 = RT_SCOPE_UNIVERSE
  toEnum 200 = RT_SCOPE_SITE
  toEnum 253 = RT_SCOPE_LINK
  toEnum 254 = RT_SCOPE_HOST
  toEnum 255 = RT_SCOPE_NOWHERE
  fromEnum RT_SCOPE_UNIVERSE = 0
  fromEnum RT_SCOPE_SITE = 200
  fromEnum RT_SCOPE_LINK = 253
  fromEnum RT_SCOPE_HOST = 254
  fromEnum RT_SCOPE_NOWHERE = 255

data AddrAttrType = IFA_UNSPEC
                  | IFA_ADDRESS
                  | IFA_LOCAL
                  | IFA_LABEL
                  | IFA_BROADCAST
                  | IFA_ANYCAST
                  | IFA_CACHEINFO
                  | IFA_MULTICAST
                  | IFA_FLAGS
                  | IFA_RT_PRIORITY
                  deriving (Eq, Show)

instance Enum AddrAttrType where
  toEnum 0 = IFA_UNSPEC
  toEnum 1 = IFA_ADDRESS
  toEnum 2 = IFA_LOCAL
  toEnum 3 = IFA_LABEL
  toEnum 4 = IFA_BROADCAST
  toEnum 5 = IFA_ANYCAST
  toEnum 6 = IFA_CACHEINFO
  toEnum 7 = IFA_MULTICAST
  toEnum 8 = IFA_FLAGS
  toEnum 9 = IFA_RT_PRIORITY
  fromEnum IFA_UNSPEC = 0
  fromEnum IFA_ADDRESS = 1
  fromEnum IFA_LOCAL = 2
  fromEnum IFA_LABEL = 3
  fromEnum IFA_BROADCAST = 4
  fromEnum IFA_ANYCAST = 5
  fromEnum IFA_CACHEINFO = 6
  fromEnum IFA_MULTICAST = 7
  fromEnum IFA_FLAGS = 8
  fromEnum IFA_RT_PRIORITY = 9

data RouteTableId = RT_TABLE_UNSPEC
                  | RT_TABLE_COMPAT
                  | RT_TABLE_DEFAULT
                  | RT_TABLE_MAIN
                  | RT_TABLE_LOCAL
                  | RT_TABLE_MAX
                  deriving (Eq, Show)

instance Enum RouteTableId where
  toEnum 0 = RT_TABLE_UNSPEC
  toEnum 252 = RT_TABLE_COMPAT
  toEnum 253 = RT_TABLE_DEFAULT
  toEnum 254 = RT_TABLE_MAIN
  toEnum 255 = RT_TABLE_LOCAL
  toEnum 4294967295 = RT_TABLE_MAX
  fromEnum RT_TABLE_UNSPEC = 0
  fromEnum RT_TABLE_COMPAT = 252
  fromEnum RT_TABLE_DEFAULT = 253
  fromEnum RT_TABLE_MAIN = 254
  fromEnum RT_TABLE_LOCAL = 255
  fromEnum RT_TABLE_MAX = 4294967295

data RouteProto = RTPROT_UNSPEC
                | RTPROT_REDIRECT
                | RTPROT_KERNEL
                | RTPROT_BOOT
                | RTPROT_STATIC
                | RTPROT_GATED
                | RTPROT_RA
                | RTPROT_MRT
                | RTPROT_ZEBRA
                | RTPROT_BIRD
                | RTPROT_DNROUTED
                | RTPROT_XORP
                | RTPROT_NTK
                | RTPROT_DHCP
                | RTPROT_MROUTED
                | RTPROT_BABEL
                | RTPROT_BGP
                | RTPROT_ISIS
                | RTPROT_OSPF
                | RTPROT_RIP
                | RTPROT_EIGRP
                deriving (Eq, Show)

instance Enum RouteProto where
  toEnum 0 = RTPROT_UNSPEC
  toEnum 1 = RTPROT_REDIRECT
  toEnum 2 = RTPROT_KERNEL
  toEnum 3 = RTPROT_BOOT
  toEnum 4 = RTPROT_STATIC
  toEnum 8 = RTPROT_GATED
  toEnum 9 = RTPROT_RA
  toEnum 10 = RTPROT_MRT
  toEnum 11 = RTPROT_ZEBRA
  toEnum 12 = RTPROT_BIRD
  toEnum 13 = RTPROT_DNROUTED
  toEnum 14 = RTPROT_XORP
  toEnum 15 = RTPROT_NTK
  toEnum 16 = RTPROT_DHCP
  toEnum 17 = RTPROT_MROUTED
  toEnum 42 = RTPROT_BABEL
  toEnum 186 = RTPROT_BGP
  toEnum 187 = RTPROT_ISIS
  toEnum 188 = RTPROT_OSPF
  toEnum 189 = RTPROT_RIP
  toEnum 192 = RTPROT_EIGRP
  fromEnum RTPROT_UNSPEC = 0
  fromEnum RTPROT_REDIRECT = 1
  fromEnum RTPROT_KERNEL = 2
  fromEnum RTPROT_BOOT = 3
  fromEnum RTPROT_STATIC = 4
  fromEnum RTPROT_GATED = 8
  fromEnum RTPROT_RA = 9
  fromEnum RTPROT_MRT = 10
  fromEnum RTPROT_ZEBRA = 11
  fromEnum RTPROT_BIRD = 12
  fromEnum RTPROT_DNROUTED = 13
  fromEnum RTPROT_XORP = 14
  fromEnum RTPROT_NTK = 15
  fromEnum RTPROT_DHCP = 16
  fromEnum RTPROT_MROUTED = 17
  fromEnum RTPROT_BABEL = 42
  fromEnum RTPROT_BGP = 186
  fromEnum RTPROT_ISIS = 187
  fromEnum RTPROT_OSPF = 188
  fromEnum RTPROT_RIP = 189
  fromEnum RTPROT_EIGRP = 192

data RouteType = RTN_UNSPEC
               | RTN_UNICAST
               | RTN_LOCAL
               | RTN_BROADCAST
               | RTN_ANYCAST
               | RTN_MULTICAST
               | RTN_BLACKHOLE
               | RTN_UNREACHABLE
               | RTN_PROHIBIT
               | RTN_THROW
               | RTN_NAT
               | RTN_XRESOLVE
               deriving (Eq, Show)

instance Enum RouteType where
  toEnum 0 = RTN_UNSPEC
  toEnum 1 = RTN_UNICAST
  toEnum 2 = RTN_LOCAL
  toEnum 3 = RTN_BROADCAST
  toEnum 4 = RTN_ANYCAST
  toEnum 5 = RTN_MULTICAST
  toEnum 6 = RTN_BLACKHOLE
  toEnum 7 = RTN_UNREACHABLE
  toEnum 8 = RTN_PROHIBIT
  toEnum 9 = RTN_THROW
  toEnum 10 = RTN_NAT
  toEnum 11 = RTN_XRESOLVE
  fromEnum RTN_UNSPEC = 0
  fromEnum RTN_UNICAST = 1
  fromEnum RTN_LOCAL = 2
  fromEnum RTN_BROADCAST = 3
  fromEnum RTN_ANYCAST = 4
  fromEnum RTN_MULTICAST = 5
  fromEnum RTN_BLACKHOLE = 6
  fromEnum RTN_UNREACHABLE = 7
  fromEnum RTN_PROHIBIT = 8
  fromEnum RTN_THROW = 9
  fromEnum RTN_NAT = 10
  fromEnum RTN_XRESOLVE = 11

newtype RouteFlags = RouteFlags Int deriving (Bits, Eq, Enum, Integral, Num, Ord, Real, Show)

fRTM_F_NOTIFY :: (Num a, Bits a) => a
fRTM_F_NOTIFY = 256
fRTM_F_CLONED :: (Num a, Bits a) => a
fRTM_F_CLONED = 512
fRTM_F_EQUALIZE :: (Num a, Bits a) => a
fRTM_F_EQUALIZE = 1024
fRTM_F_PREFIX :: (Num a, Bits a) => a
fRTM_F_PREFIX = 2048
fRTM_F_LOOKUP_TABLE :: (Num a, Bits a) => a
fRTM_F_LOOKUP_TABLE = 4096
fRTM_F_FIB_MATCH :: (Num a, Bits a) => a
fRTM_F_FIB_MATCH = 8192
data RouteAttrType = RTA_UNSPEC
                   | RTA_DST
                   | RTA_SRC
                   | RTA_IIF
                   | RTA_OIF
                   | RTA_GATEWAY
                   | RTA_PRIORITY
                   | RTA_PREFSRC
                   | RTA_METRICS
                   | RTA_MULTIPATH
                   | RTA_PROTOINFO
                   | RTA_FLOW
                   | RTA_CACHEINFO
                   | RTA_SESSION
                   | RTA_MP_ALGO
                   | RTA_TABLE
                   | RTA_MARK
                   | RTA_MFC_STATS
                   | RTA_VIA
                   | RTA_NEWDST
                   | RTA_PREF
                   | RTA_ENCAP_TYPE
                   | RTA_ENCAP
                   | RTA_EXPIRES
                   | RTA_PAD
                   | RTA_UID
                   | RTA_TTL_PROPAGATE
                   | RTA_IP_PROTO
                   | RTA_SPORT
                   | RTA_DPORT
                   deriving (Eq, Show)

instance Enum RouteAttrType where
  toEnum 0 = RTA_UNSPEC
  toEnum 1 = RTA_DST
  toEnum 2 = RTA_SRC
  toEnum 3 = RTA_IIF
  toEnum 4 = RTA_OIF
  toEnum 5 = RTA_GATEWAY
  toEnum 6 = RTA_PRIORITY
  toEnum 7 = RTA_PREFSRC
  toEnum 8 = RTA_METRICS
  toEnum 9 = RTA_MULTIPATH
  toEnum 10 = RTA_PROTOINFO
  toEnum 11 = RTA_FLOW
  toEnum 12 = RTA_CACHEINFO
  toEnum 13 = RTA_SESSION
  toEnum 14 = RTA_MP_ALGO
  toEnum 15 = RTA_TABLE
  toEnum 16 = RTA_MARK
  toEnum 17 = RTA_MFC_STATS
  toEnum 18 = RTA_VIA
  toEnum 19 = RTA_NEWDST
  toEnum 20 = RTA_PREF
  toEnum 21 = RTA_ENCAP_TYPE
  toEnum 22 = RTA_ENCAP
  toEnum 23 = RTA_EXPIRES
  toEnum 24 = RTA_PAD
  toEnum 25 = RTA_UID
  toEnum 26 = RTA_TTL_PROPAGATE
  toEnum 27 = RTA_IP_PROTO
  toEnum 28 = RTA_SPORT
  toEnum 29 = RTA_DPORT
  fromEnum RTA_UNSPEC = 0
  fromEnum RTA_DST = 1
  fromEnum RTA_SRC = 2
  fromEnum RTA_IIF = 3
  fromEnum RTA_OIF = 4
  fromEnum RTA_GATEWAY = 5
  fromEnum RTA_PRIORITY = 6
  fromEnum RTA_PREFSRC = 7
  fromEnum RTA_METRICS = 8
  fromEnum RTA_MULTIPATH = 9
  fromEnum RTA_PROTOINFO = 10
  fromEnum RTA_FLOW = 11
  fromEnum RTA_CACHEINFO = 12
  fromEnum RTA_SESSION = 13
  fromEnum RTA_MP_ALGO = 14
  fromEnum RTA_TABLE = 15
  fromEnum RTA_MARK = 16
  fromEnum RTA_MFC_STATS = 17
  fromEnum RTA_VIA = 18
  fromEnum RTA_NEWDST = 19
  fromEnum RTA_PREF = 20
  fromEnum RTA_ENCAP_TYPE = 21
  fromEnum RTA_ENCAP = 22
  fromEnum RTA_EXPIRES = 23
  fromEnum RTA_PAD = 24
  fromEnum RTA_UID = 25
  fromEnum RTA_TTL_PROPAGATE = 26
  fromEnum RTA_IP_PROTO = 27
  fromEnum RTA_SPORT = 28
  fromEnum RTA_DPORT = 29

data NeighAttrType = NDA_UNSPEC
                   | NDA_DST
                   | NDA_LLADDR
                   | NDA_CACHEINFO
                   | NDA_PROBES
                   | NDA_VLAN
                   | NDA_PORT
                   | NDA_VNI
                   | NDA_IFINDEX
                   | NDA_MASTER
                   | NDA_LINK_NETNSID
                   | NDA_SRC_VNI
                   deriving (Eq, Show)

instance Enum NeighAttrType where
  toEnum 0 = NDA_UNSPEC
  toEnum 1 = NDA_DST
  toEnum 2 = NDA_LLADDR
  toEnum 3 = NDA_CACHEINFO
  toEnum 4 = NDA_PROBES
  toEnum 5 = NDA_VLAN
  toEnum 6 = NDA_PORT
  toEnum 7 = NDA_VNI
  toEnum 8 = NDA_IFINDEX
  toEnum 9 = NDA_MASTER
  toEnum 10 = NDA_LINK_NETNSID
  toEnum 11 = NDA_SRC_VNI
  fromEnum NDA_UNSPEC = 0
  fromEnum NDA_DST = 1
  fromEnum NDA_LLADDR = 2
  fromEnum NDA_CACHEINFO = 3
  fromEnum NDA_PROBES = 4
  fromEnum NDA_VLAN = 5
  fromEnum NDA_PORT = 6
  fromEnum NDA_VNI = 7
  fromEnum NDA_IFINDEX = 8
  fromEnum NDA_MASTER = 9
  fromEnum NDA_LINK_NETNSID = 10
  fromEnum NDA_SRC_VNI = 11

newtype NeighStateFlags = NeighStateFlags Int deriving (Bits, Eq, Enum, Integral, Num, Ord, Real, Show)

fNUD_NONE :: (Num a, Bits a) => a
fNUD_NONE = 0
fNUD_INCOMPLETE :: (Num a, Bits a) => a
fNUD_INCOMPLETE = 1
fNUD_REACHABLE :: (Num a, Bits a) => a
fNUD_REACHABLE = 2
fNUD_STALE :: (Num a, Bits a) => a
fNUD_STALE = 4
fNUD_DELAY :: (Num a, Bits a) => a
fNUD_DELAY = 8
fNUD_PROBE :: (Num a, Bits a) => a
fNUD_PROBE = 16
fNUD_FAILED :: (Num a, Bits a) => a
fNUD_FAILED = 32
fNUD_NOARP :: (Num a, Bits a) => a
fNUD_NOARP = 64
fNUD_PERMANENT :: (Num a, Bits a) => a
fNUD_PERMANENT = 128
data VethAttrInfoType = VETH_INFO_UNSPEC
                      | VETH_INFO_PEER
                      deriving (Eq, Show)

instance Enum VethAttrInfoType where
  toEnum 0 = VETH_INFO_UNSPEC
  toEnum 1 = VETH_INFO_PEER
  fromEnum VETH_INFO_UNSPEC = 0
  fromEnum VETH_INFO_PEER = 1

data NetlinkFamily = NETLINK_ROUTE
                   | NETLINK_ADD_MEMBERSHIP
                   | NETLINK_UNUSED
                   | NETLINK_DROP_MEMBERSHIP
                   | NETLINK_USERSOCK
                   | NETLINK_FIREWALL
                   | NETLINK_PKTINFO
                   | NETLINK_BROADCAST_ERROR
                   | NETLINK_INET_DIAG
                   | NETLINK_SOCK_DIAG
                   | NETLINK_NFLOG
                   | NETLINK_NO_ENOBUFS
                   | NETLINK_RX_RING
                   | NETLINK_XFRM
                   | NETLINK_SELINUX
                   | NETLINK_TX_RING
                   | NETLINK_ISCSI
                   | NETLINK_LISTEN_ALL_NSID
                   | NETLINK_AUDIT
                   | NETLINK_LIST_MEMBERSHIPS
                   | NETLINK_CAP_ACK
                   | NETLINK_FIB_LOOKUP
                   | NETLINK_CONNECTOR
                   | NETLINK_EXT_ACK
                   | NETLINK_NETFILTER
                   | NETLINK_IP6_FW
                   | NETLINK_DNRTMSG
                   | NETLINK_KOBJECT_UEVENT
                   | NETLINK_GENERIC
                   | NETLINK_SCSITRANSPORT
                   | NETLINK_ECRYPTFS
                   | NETLINK_RDMA
                   | NETLINK_CRYPTO
                   | NETLINK_SMC
                   deriving (Eq, Show)

instance Enum NetlinkFamily where
  toEnum 0 = NETLINK_ROUTE
  toEnum 1 = NETLINK_ADD_MEMBERSHIP
  toEnum 2 = NETLINK_DROP_MEMBERSHIP
  toEnum 3 = NETLINK_FIREWALL
  toEnum 4 = NETLINK_BROADCAST_ERROR
  toEnum 5 = NETLINK_NFLOG
  toEnum 6 = NETLINK_RX_RING
  toEnum 7 = NETLINK_SELINUX
  toEnum 8 = NETLINK_ISCSI
  toEnum 9 = NETLINK_AUDIT
  toEnum 10 = NETLINK_CAP_ACK
  toEnum 11 = NETLINK_CONNECTOR
  toEnum 12 = NETLINK_NETFILTER
  toEnum 13 = NETLINK_IP6_FW
  toEnum 14 = NETLINK_DNRTMSG
  toEnum 15 = NETLINK_KOBJECT_UEVENT
  toEnum 16 = NETLINK_GENERIC
  toEnum 18 = NETLINK_SCSITRANSPORT
  toEnum 19 = NETLINK_ECRYPTFS
  toEnum 20 = NETLINK_RDMA
  toEnum 21 = NETLINK_CRYPTO
  toEnum 22 = NETLINK_SMC
  fromEnum NETLINK_ROUTE = 0
  fromEnum NETLINK_ADD_MEMBERSHIP = 1
  fromEnum NETLINK_UNUSED = 1
  fromEnum NETLINK_DROP_MEMBERSHIP = 2
  fromEnum NETLINK_USERSOCK = 2
  fromEnum NETLINK_FIREWALL = 3
  fromEnum NETLINK_PKTINFO = 3
  fromEnum NETLINK_BROADCAST_ERROR = 4
  fromEnum NETLINK_INET_DIAG = 4
  fromEnum NETLINK_SOCK_DIAG = 4
  fromEnum NETLINK_NFLOG = 5
  fromEnum NETLINK_NO_ENOBUFS = 5
  fromEnum NETLINK_RX_RING = 6
  fromEnum NETLINK_XFRM = 6
  fromEnum NETLINK_SELINUX = 7
  fromEnum NETLINK_TX_RING = 7
  fromEnum NETLINK_ISCSI = 8
  fromEnum NETLINK_LISTEN_ALL_NSID = 8
  fromEnum NETLINK_AUDIT = 9
  fromEnum NETLINK_LIST_MEMBERSHIPS = 9
  fromEnum NETLINK_CAP_ACK = 10
  fromEnum NETLINK_FIB_LOOKUP = 10
  fromEnum NETLINK_CONNECTOR = 11
  fromEnum NETLINK_EXT_ACK = 11
  fromEnum NETLINK_NETFILTER = 12
  fromEnum NETLINK_IP6_FW = 13
  fromEnum NETLINK_DNRTMSG = 14
  fromEnum NETLINK_KOBJECT_UEVENT = 15
  fromEnum NETLINK_GENERIC = 16
  fromEnum NETLINK_SCSITRANSPORT = 18
  fromEnum NETLINK_ECRYPTFS = 19
  fromEnum NETLINK_RDMA = 20
  fromEnum NETLINK_CRYPTO = 21
  fromEnum NETLINK_SMC = 22

data RtNetlinkGroups = RTNLGRP_NONE
                     | RTNLGRP_LINK
                     | RTNLGRP_NOTIFY
                     | RTNLGRP_NEIGH
                     | RTNLGRP_TC
                     | RTNLGRP_IPV4_IFADDR
                     | RTNLGRP_IPV4_MROUTE
                     | RTNLGRP_IPV4_ROUTE
                     | RTNLGRP_IPV4_RULE
                     | RTNLGRP_IPV6_IFADDR
                     | RTNLGRP_IPV6_MROUTE
                     | RTNLGRP_IPV6_ROUTE
                     | RTNLGRP_IPV6_IFINFO
                     | RTNLGRP_DECnet_IFADDR
                     | RTNLGRP_NOP2
                     | RTNLGRP_DECnet_ROUTE
                     | RTNLGRP_DECnet_RULE
                     | RTNLGRP_NOP4
                     | RTNLGRP_IPV6_PREFIX
                     | RTNLGRP_IPV6_RULE
                     | RTNLGRP_ND_USEROPT
                     | RTNLGRP_PHONET_IFADDR
                     | RTNLGRP_PHONET_ROUTE
                     | RTNLGRP_DCB
                     | RTNLGRP_IPV4_NETCONF
                     | RTNLGRP_IPV6_NETCONF
                     | RTNLGRP_MDB
                     | RTNLGRP_MPLS_ROUTE
                     | RTNLGRP_NSID
                     | RTNLGRP_MPLS_NETCONF
                     | RTNLGRP_IPV4_MROUTE_R
                     | RTNLGRP_IPV6_MROUTE_R
                     deriving (Eq, Show)

instance Enum RtNetlinkGroups where
  toEnum 0 = RTNLGRP_NONE
  toEnum 1 = RTNLGRP_LINK
  toEnum 2 = RTNLGRP_NOTIFY
  toEnum 3 = RTNLGRP_NEIGH
  toEnum 4 = RTNLGRP_TC
  toEnum 5 = RTNLGRP_IPV4_IFADDR
  toEnum 6 = RTNLGRP_IPV4_MROUTE
  toEnum 7 = RTNLGRP_IPV4_ROUTE
  toEnum 8 = RTNLGRP_IPV4_RULE
  toEnum 9 = RTNLGRP_IPV6_IFADDR
  toEnum 10 = RTNLGRP_IPV6_MROUTE
  toEnum 11 = RTNLGRP_IPV6_ROUTE
  toEnum 12 = RTNLGRP_IPV6_IFINFO
  toEnum 13 = RTNLGRP_DECnet_IFADDR
  toEnum 14 = RTNLGRP_NOP2
  toEnum 15 = RTNLGRP_DECnet_ROUTE
  toEnum 16 = RTNLGRP_DECnet_RULE
  toEnum 17 = RTNLGRP_NOP4
  toEnum 18 = RTNLGRP_IPV6_PREFIX
  toEnum 19 = RTNLGRP_IPV6_RULE
  toEnum 20 = RTNLGRP_ND_USEROPT
  toEnum 21 = RTNLGRP_PHONET_IFADDR
  toEnum 22 = RTNLGRP_PHONET_ROUTE
  toEnum 23 = RTNLGRP_DCB
  toEnum 24 = RTNLGRP_IPV4_NETCONF
  toEnum 25 = RTNLGRP_IPV6_NETCONF
  toEnum 26 = RTNLGRP_MDB
  toEnum 27 = RTNLGRP_MPLS_ROUTE
  toEnum 28 = RTNLGRP_NSID
  toEnum 29 = RTNLGRP_MPLS_NETCONF
  toEnum 30 = RTNLGRP_IPV4_MROUTE_R
  toEnum 31 = RTNLGRP_IPV6_MROUTE_R
  fromEnum RTNLGRP_NONE = 0
  fromEnum RTNLGRP_LINK = 1
  fromEnum RTNLGRP_NOTIFY = 2
  fromEnum RTNLGRP_NEIGH = 3
  fromEnum RTNLGRP_TC = 4
  fromEnum RTNLGRP_IPV4_IFADDR = 5
  fromEnum RTNLGRP_IPV4_MROUTE = 6
  fromEnum RTNLGRP_IPV4_ROUTE = 7
  fromEnum RTNLGRP_IPV4_RULE = 8
  fromEnum RTNLGRP_IPV6_IFADDR = 9
  fromEnum RTNLGRP_IPV6_MROUTE = 10
  fromEnum RTNLGRP_IPV6_ROUTE = 11
  fromEnum RTNLGRP_IPV6_IFINFO = 12
  fromEnum RTNLGRP_DECnet_IFADDR = 13
  fromEnum RTNLGRP_NOP2 = 14
  fromEnum RTNLGRP_DECnet_ROUTE = 15
  fromEnum RTNLGRP_DECnet_RULE = 16
  fromEnum RTNLGRP_NOP4 = 17
  fromEnum RTNLGRP_IPV6_PREFIX = 18
  fromEnum RTNLGRP_IPV6_RULE = 19
  fromEnum RTNLGRP_ND_USEROPT = 20
  fromEnum RTNLGRP_PHONET_IFADDR = 21
  fromEnum RTNLGRP_PHONET_ROUTE = 22
  fromEnum RTNLGRP_DCB = 23
  fromEnum RTNLGRP_IPV4_NETCONF = 24
  fromEnum RTNLGRP_IPV6_NETCONF = 25
  fromEnum RTNLGRP_MDB = 26
  fromEnum RTNLGRP_MPLS_ROUTE = 27
  fromEnum RTNLGRP_NSID = 28
  fromEnum RTNLGRP_MPLS_NETCONF = 29
  fromEnum RTNLGRP_IPV4_MROUTE_R = 30
  fromEnum RTNLGRP_IPV6_MROUTE_R = 31

