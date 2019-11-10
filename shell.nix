{ pkgs ? import <nixpkgs> {} }:

let
  hsPkgs = import ./default.nix;
in
  hsPkgs.default.shellFor {
    buildInputs = with pkgs.haskellPackages;
      [ hpack ghcid brittany ];
  }
