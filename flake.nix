{
  description = "A flake for ovm";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, nixpkgs-unstable }:
    let
      systems = [ "x86_64-linux" ];
    in
      flake-utils.lib.eachSystem systems (system:
        let
          inherit (nixpkgs) lib;
          pkgs = import nixpkgs { inherit system; };
          unstable = import nixpkgs-unstable { inherit system; };
          defaultStdenv = pkgs.multiStdenv;
        in
          {
            packages = {
              ovm = defaultStdenv.mkDerivation
              rec {
                pname = "ovm";
                version = "0.1";

                src = self;
                enableParallelBuilding = true;

                buildInputs = with unstable; [
                  ocaml
                  dune_3
                  ocamlPackages.menhir
                  ocamlPackages.dune-build-info
                  ocamlPackages.findlib
                ];

                shellHook = ''
                  dune build
                  dune install --prefix=$out
                  export PATH=$out/bin/:$PATH

                  dune clean
                '';
              };

              devShell = pkgs.mkShell.override { stdenv = defaultStdenv; }
              { inputsFrom = [ self.packages.${system}.ovm ]; };
            };
            defaultPackage = self.packages.${system}.ovm;
          }
      );
}
