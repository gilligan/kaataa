let
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  watch-tests = pkgs.writeShellScriptBin "watch-tests" ''
    ls -1 *.go | ${pkgs.entr}/bin/entr -c ${pkgs.go_1_14}/bin/go test ./...
  '';

in {

  build = pkgs.buildGoModule {
    name = "mars-rover";
    src = ./.;
    vendorSha256 = "0000000000000000000000000000000000000000000000000000000000000000";
  };


  shell = pkgs.mkShell {
    buildInputs = [ pkgs.go_1_14 watch-tests ];
  };
}
