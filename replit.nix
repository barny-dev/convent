{ pkgs }: {
  deps = [
    pkgs.haskellPackages.temporary
    pkgs.haskellPackages.ghc
    pkgs.haskell-language-server
    pkgs.cabal-install
  ];
}