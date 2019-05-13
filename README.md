This code relies on an older version of language-c.

The 0.6.1 version has a different API from the 0.8.2. version of language-c.

So we have to use an older Nixpkgs commit hash to utilise this.

We have to use:

```
cabal2nix -f Generators . >./cabal.nix
```

Then when we are in the shell.

```
cabal configure -f Generators
```

Hopefully that works.

To run the Generate command, we need:

```
pushd Scripts
runhaskell ./GenerateConstants.hs ../System/Linux/Netlink/Constants.hs
popd
```

---

The base netlink only supports 32 families.

Here are the official families of netlink:

1. `NETLINK_ROUTE` - rtnetlink stuff
2. `NETLINK_W1`
3. `NETLINK_USERSOCK`
4. `NETLINK_FIREWALL` - removed
5. `NETLINK_INET_DIAG` - query info about sockets
6. `NETLINK_SOCK_DIAG`
7. `NETLINK_NFLOG` - some log for netlink?
8. `NETLINK_XFRM` - ipsec
9. `NETLINK_SELINUX` - selinix events
10. `NETLINK_ISCSI` - ISCSI
11. `NETLINK_AUDIT` - auditing
12. `NETLINK_FIB_LOOKUP` - FIB lookup
13. `NETLINK_CONNECTOR`
14. `NETLINK_NETFILTER` - is this important?
15. `NETLINK_GENERIC`
16. `NETLINK_CRYPTO`

With regards to `NETLINK_GENERIC`, we have:

* GenlHeader (this is the family header)
* GenlData (this also a family header, as it wraps around GenlHeader)

```
GenlPacket a = Packet (GenlData a)
```

The constants it wants appears to be:

`CTRL_CMD_`... control command

`CTRL_ATTR_`... control attribute

`CTRL_ATTR_MCAST` - control attribute multicast?

Generic netlink controller is implemented as a standard generic netlink user, however it listens on a special preallocated generic netlink channel. So the controller is like a special thing.

Generic netlink comms are essentially a series of different communication channels which are multiplexed on a single netlink family. Comm channels are uniquely identified by channel numbers which are dynamically allocated by the generic netlink controller. The controller is a special Gen netlink user which listens on a fixed communication channel number 0x10. Kernel or userspace users which provide services over the generic netlink bus, establish new channels by registering their services with the gen netlink controller. Users who want to use a service, query the controller to see if the service exists and to determine the correct channel number.

A gen netlink message has:

1. netlink message header
2. gen netlink message header 32 bits? 
3. optional user specific message header
4. optional gen netlink message payload
