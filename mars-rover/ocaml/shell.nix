let
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  ocamlPackages = pkgs.recurseIntoAttrs pkgs.ocamlPackages_latest;
  ocamlVersion = (builtins.parseDrvName ocamlPackages.ocaml.name).version;
  findlibSiteLib = "${ocamlPackages.findlib}/lib/ocaml/${ocamlVersion}/site-lib";
  ocamlInit = pkgs.writeText "ocamlinit" ''
    let () =
      try Topdirs.dir_directory "${findlibSiteLib}"
      with Not_found -> ()
    ;;

    #use "topfind";;
    #thread;;
    #camlp4o;;
    #require "core";;
    #require "core.syntax";;
  '';
in
pkgs.stdenv.mkDerivation rec {
  name = "rwo-shell";
  src = null;
  buildInputs = with ocamlPackages;
    [ ocaml
      core
      core_extended
      findlib
      utop
      dune
      ounit2
      base
      stdio
      ppx_expect
      merlin
    ] ++ [pkgs.opam pkgs.entr];
  OCAMLINIT = "${ocamlInit}";
  shellHook = ''
    alias utop="utop -init ${ocamlInit}"
    alias ocaml="ocaml -init ${ocamlInit}"

    # In order to use `merlin` in vim, we need to source it:
    #
    # ```
    # execute "set rtp+=" . $MERLIN_PATH
    # ```
    export MERLIN_PATH=${ocamlPackages.merlin}/share/merlin/vim
  '';
}
