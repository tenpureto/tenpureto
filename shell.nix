let hsPkgs = import ./default.nix { };
in hsPkgs.default.shellFor {
  buildInputs = with hsPkgs.pkgs.haskellPackages; [ hpack ghcid brittany ];
}
