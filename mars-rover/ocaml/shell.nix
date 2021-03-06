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
  watch-tests = pkgs.writeScriptBin "watch-tests" ''
    ls -1 rover.ml | entr -c dune runtest
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
      angstrom
      merlin
    ] ++ [pkgs.opam pkgs.entr watch-tests pkgs.ocamlformat];
  OCAMLINIT = "${ocamlInit}";
  shellHook = ''
    alias utop="utop -init ${ocamlInit}"
    alias ocaml="ocaml -init ${ocamlInit}"

    # In order to use `merlin` in vim, we need to source it:
    #
    # ```
    # execute "set rtp+=" . $MERLIN_PATH
    # let b:merlin_path="${ocamlPackages.merlin}/bin/ocamlmerlin"
    # ```
    export MERLIN_PATH=${ocamlPackages.merlin}/share/merlin/vim
  '';
}
