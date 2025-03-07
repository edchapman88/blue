{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs }@inputs:
    # Don't forget to put the package name instead of `throw':
    let package = "blue";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        scope =
          on.buildDuneProject { } package ./. { ocaml-base-compiler = "*"; };
      in {
        legacyPackages = scope;

        packages.default = self.legacyPackages.${system}.${package};
      });
}
