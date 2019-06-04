{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864"}:
let
  haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
    };
  };
  developPackage = haskellPackages.developPackage { root = ./.; };
  hoogle         = haskellPackages.ghcWithHoogle (hs: with hs;
                     # developPackage.buildInputs ++
                     [
                     ]);
in
  developPackage.overrideAttrs (oldAttrs: with nixpkgs; {
    buildInputs = oldAttrs.buildInputs
                  ++ [ git cabal-install zsh vim less hoogle
                       # haskell.packages.ghc862.threadscope
                     ];
    shellHook = ''
      zsh -c "PATH=$PATH; TZ="Asia/Tokyo"; zsh"
      exit
    '';
  })
