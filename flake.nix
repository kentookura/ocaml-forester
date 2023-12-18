{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
    ocaml-lsp.url = "github:ocaml/ocaml-lsp";
    asai.url = "github:RedPRL/asai";
    asai.flake = false;
    asai-lsp.url = "/home/kento/repos/asai/";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
  };
  outputs = { self, asai, flake-utils, opam-nix, nixpkgs, ocaml-lsp, asai-lsp
    , opam-repository }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        # caveat: the opam files get generated by dune,
        # so make sure the project builds with dune, then with nix
        legacyPackages = let
          inherit (opam-nix.lib.${system}) buildOpamProject;
          scope = buildOpamProject {
            repos = [ "${opam-repository}" ocaml-lsp asai asai-lsp ];
          } "forester" ./. {
            ocaml-system = "*";
            lsp = "*";
            asai = "*";
          };
        in scope;
        packages.default = self.legacyPackages.${system}."forester";
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ocaml-ng.ocamlPackages_5_0.ocaml
            ocaml-ng.ocamlPackages_5_0.menhir
            ocaml-ng.ocamlPackages_5_0.dune_3
            opam
            ocamlformat
            vscodium
            watchexec
            #ocaml-lsp.packages.${system}.default
          ];
          shellHook = ''
            eval $(opam env)
            export PATH=$PATH:$PWD/result/bin
          '';
        };
      });
}
