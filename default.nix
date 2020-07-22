{ nixpkgs ? import <nixpkgs> {}, compiler ? "default"}:
let
  haskellPackages = (if   compiler == "default"
                     then nixpkgs.pkgs.haskellPackages
                     else nixpkgs.pkgs.haskell.packages.${compiler}).override {
    overrides = self: super: {
      # ansi-terminal     = self.callPackage ./nix/ansi-terminal-0.9.1.nix {};
      # concurrent-output = self.callPackage ./nix/concurrent-output-1.10.10.nix {};
    };
  };
  developPackage = haskellPackages.developPackage { root = ./.; };
  hoogle         = haskellPackages.ghcWithHoogle (hs: with hs;
                     [ ]);
in
  developPackage.overrideAttrs (oldAttrs: with nixpkgs; {
    buildInputs = oldAttrs.buildInputs ++ [
      # hoogle
      # haskellPackages.hlint
    ];
  })
