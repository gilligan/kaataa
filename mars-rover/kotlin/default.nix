let
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  watch-tests = pkgs.writeShellScriptBin "watch-tests" ''
    ${pkgs.gradle}/bin/gradle -t test
  '';

  kotlin-lsp = pkgs.stdenv.mkDerivation rec {
    pname = "kotlin-language-server";
    version = "0.5.2";
    src = builtins.fetchurl {
      url = "https://github.com/fwcd/kotlin-language-server/releases/download/${version}/server.zip";
      sha256 = "0fsflihkh1b7zhg9bjhb791i9ih1qp3972nma0z9h31j2kpb6xbj";
    };
    buildInputs = with pkgs; [ unzip zulu ];
    installPhase = ''
      mkdir -p $out
      cp -R . $out/
    '';
  };

in rec {
  shell = pkgs.mkShell {
    buildInputs = with pkgs; [ kotlin kotlin-lsp ktlint gradle watch-tests ];
  };
}
