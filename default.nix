{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865"}:
let
  haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
    };
  };
  developPackage = haskellPackages.developPackage { root = ./.; };
  hoogle         = haskellPackages.ghcWithHoogle (hs: with hs;
                     [ ]);
in
  developPackage.overrideAttrs (oldAttrs: with nixpkgs; {
    buildInputs = oldAttrs.buildInputs
      ++ [ hoogle haskellPackages.hlint ];
  })
