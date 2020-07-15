let
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [ pscid nodejs spago purescript ];
    shellHook = ''
      export PATH=$PATH:./node_modules/.bin
    '';
  }
