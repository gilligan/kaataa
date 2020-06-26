{ pkgs ? import ((import ./nix/sources.nix).nixpkgs) {}
, compiler ? "ghc883"
}:

let
  hsPkgs = pkgs.haskell.packages.${compiler};
  drv = hsPkgs.callCabal2nix "rover" ./. {};
  watch-tests = pkgs.writeScriptBin "watch-tests" ''
    ${pkgs.ghcid}/bin/ghcid --clear --command "cabal repl rover:test:tests" --test "hspec spec"
  '';
in
  {
    rover = drv;
    shell = hsPkgs.shellFor {
      packages = _: [drv];
      buildInputs = with pkgs; [ cabal-install hlint hsPkgs.ormolu watch-tests ghcid hsPkgs.ghcide hsPkgs.hspec-discover ];
    };
  }
