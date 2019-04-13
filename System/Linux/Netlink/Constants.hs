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
                   deriving (Eq, Enum, Show)

data MessageType = NLMSG_NOOP
                 | NLMSG_ERROR
                 | NLMSG_DONE
                 | NLMSG_OVERRUN
                 | NLMSG_MIN_TYPE
                 deriving (Eq, Enum, Show)

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
                      deriving (Eq, Enum, Show)

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
              deriving (Eq, Enum, Show)

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
                  deriving (Eq, Enum, Show)

data LinkAttrInfoType = IFLA_INFO_UNSPEC
                      | IFLA_INFO_KIND
                      | IFLA_INFO_DATA
                      | IFLA_INFO_XSTATS
                      | IFLA_INFO_SLAVE_KIND
                      | IFLA_INFO_SLAVE_DATA
                      deriving (Eq, Enum, Show)

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
           deriving (Eq, Enum, Show)

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
                  deriving (Eq, Enum, Show)

data RouteTableId = RT_TABLE_UNSPEC
                  | RT_TABLE_COMPAT
                  | RT_TABLE_DEFAULT
                  | RT_TABLE_MAIN
                  | RT_TABLE_LOCAL
                  | RT_TABLE_MAX
                  deriving (Eq, Enum, Show)

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
                deriving (Eq, Enum, Show)

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
               deriving (Eq, Enum, Show)

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
                   deriving (Eq, Enum, Show)

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
                   deriving (Eq, Enum, Show)

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
                      deriving (Eq, Enum, Show)

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
                   deriving (Eq, Enum, Show)

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
                     deriving (Eq, Enum, Show)

