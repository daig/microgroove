{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc821" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, primitive, stdenv, vector }:
      mkDerivation {
        pname = "microgroove";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base primitive vector ];
        homepage = "https://github.com/daig/microgroove";
        description = "Fast access extensible records";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
