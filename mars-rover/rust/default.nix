let
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  gradle2nix = import sources.gradle2nix {};
  buildGradle = pkgs.callPackage ./gradle-env.nix {};

  watch-tests = pkgs.writeShellScriptBin "watch-tests" ''
    ${pkgs.gradle}/bin/gradle -t test
  '';

in rec {

  build = buildGradle {
    envSpec = ./gradle-env.json;
    src = pkgs.lib.cleanSource ./.;
    installPhase = ''
      mkdir -p $out
      cp build/libs/mars-rover.jar $out
    '';
  };

  shell = pkgs.mkShell {
    buildInputs = with pkgs; [ zulu gradle gradle2nix watch-tests ];
  };
}
