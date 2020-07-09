let
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  repl = pkgs.writeScriptBin "repl" ''
    clojure -Sdeps '{:deps {nrepl {:mvn/version "0.7.0"} cider/cider-nrepl {:mvn/version "0.25.0"}}}' -m nrepl.cmdline --middleware '["cider.nrepl/cider-middleware"]'
    '';
in
pkgs.mkShell {
  buildInputs = with pkgs; [ repl clojure leiningen entr clj-kondo];
}
