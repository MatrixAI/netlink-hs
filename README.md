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
