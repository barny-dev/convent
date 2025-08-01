{ pkgs }: {
  deps = [
    pkgs.haskellPackages.transformers_0_6_1_1
    pkgs.haskellPackages.directory_1_3_8_4
    pkgs.haskellPackages.containers_0_7
    pkgs.haskellPackages.uuid
    pkgs.haskellPackages.text_2_1_1
    pkgs.haskellPackages.bytestring_0_12_1_0
    pkgs.haskellPackages.hspec
    pkgs.haskellPackages.temporary
    pkgs.haskellPackages.ghc
    pkgs.haskell-language-server
    pkgs.cabal-install
  ];
}