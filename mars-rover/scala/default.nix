let
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  watch-tests = pkgs.writeShellScriptBin "watch-tests" ''
    exit 1
  '';

in rec {

  shell = pkgs.mkShell {
    buildInputs = with pkgs; [ metals scala sbt watch-tests ];
  };
}
