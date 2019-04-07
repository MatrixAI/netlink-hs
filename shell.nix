{
  pkgs ? import ./pkgs.nix,
  haskellPath ? "ghc863"
}:
  with pkgs;
  let
    haskellPackages = lib.getAttrFromPath (lib.splitString "." haskellPath) haskell.packages;
    drv = (import ./default.nix { inherit pkgs haskellPath; }).env;
  in
    drv.overrideAttrs (attrs: {
      src = null;
      buildInputs = (with haskellPackages; [
        cabal2nix
        cabal-install
      ]);
      shellHook = attrs.shellHook + ''
        echo 'Entering ${attrs.name}'
        set -v

        set +v
      '';
    })
