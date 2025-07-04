{ pkgs }: {
  deps = [
    pkgs.haskellPackages.text_2_1_1
    pkgs.haskellPackages.bytestring_0_12_1_0
    pkgs.haskellPackages.hspec
    pkgs.haskellPackages.temporary
    pkgs.haskellPackages.ghc
    pkgs.haskell-language-server
    pkgs.cabal-install
  ];
}