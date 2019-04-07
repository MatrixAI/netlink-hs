{ mkDerivation, base, bytestring, cereal, containers, language-c
, monad-loops, pretty-hex, process, regex-pcre, stdenv, unix
}:
mkDerivation {
  pname = "netlink";
  version = "1.1.1.0";
  src = ./.;
  configureFlags = [ "-fgenerators" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cereal containers monad-loops pretty-hex unix
  ];
  executableHaskellDepends = [
    base containers language-c process regex-pcre
  ];
  homepage = "https://github.com/Ongy/netlink-hs";
  description = "Netlink communication for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
