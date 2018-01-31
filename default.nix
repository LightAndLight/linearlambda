{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./linearlambda.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  unification =
    haskellPackages.callPackage (import ./nix/unification.nix) {};

  drv = haskellPackages.callPackage f { inherit unification; };

in

  drv
