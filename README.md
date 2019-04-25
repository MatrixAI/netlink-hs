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
runhaskell ./Generate.hs ../System/Linux/Constants.hs
popd
```

---

Previously we were working on the `Scripts/Generate.hs`. We will need to get 
This added the `MessageType`... oh yea, we were changing to using ADTs instead of using constants, so we could properly pattern matching on them, and we could use Enums. We are going to generate Enum typeclass derivations for them as well.

Right now I have:

```
data LinkAttrType = IFLA_UNSPEC
                  | IFLA_ADDRESS
                  | ...
                  deriving (Eq, Enum, Show)
```

So we need to change to using:

```
instance Enum LinkAttrType where
  
```

We have family type as well. That's important.

Wait enums are definitely deriving ENUM.

Flags are more complciated. We don't have their numbers. Instead we have to derive it exactly.

Also strangely, could we then derive not... wait how do we know these are in order though!?

Weirdly we have:

```
AF_FILE = 1
AF_LOCAL = 1
AF_UNIX = 1
```

But if we get 1, we show `AF_FILE`.

Right now with my ADT derivation, I'm getting `AF_FILE = 1`, `AF_LOCAL= 2` and `AF_NIX = 3`!! Oh no, so we have to do separate type class derivations for these as well!

Ok so there are synonyms. It is state that:

```
AF_LOCAL - locl address format
AF_UNIX - synonym for AF_LOCAL
AF_FILE - synonym for AF_LOCAL
```

So if we were to use `toEnum :: Int -> a`.

```
instance Enum fName where
  toEnum 1 = AF_LOCAL
  fromEnum AF_FILE = 1
  fromEnum AF_LOCAL = 1
  fromEnum AF_UNIX = 1
```

But we don't know which one is the primary synonym!

```
[
  ("AF_FILE", 1),
  ("AF_LOCAL", 1),
  ("AF_UNIX", 1)
]
```


That is good for creating fromEnum.

But for `toEnum`?

We have to go the other way.

Well we still iterate, but this time we put the value ahead... then the name.

But if there are multiple values that already exist, then we have to filter!

We will need to filter out on unique values

We need to use `nubBy`


```
let derivingEnum name vals = "instance Enum " ++ name ++ " where\n" ++ toEnums ++ fromEnums
      where
        values = M.toList vals
        toEnums = concatMap
          (\(k, v) -> "  toEnum " ++ show v ++ " = " ++ k ++ "\n") $
          nubBy ((==) `on` snd) values
        fromEnums vals = concatMap
          (\(k, v) -> "  fromEnum " ++ k ++ " = " ++ show v ++ "\n") $
          values

```
