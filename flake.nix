{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }@attrs:
    flake-utils.lib.eachSystem flake-utils.lib.defaultSystems (
      system:
      let
        pkgs = import nixpkgs{ inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            ocaml
            ocamlPackages.menhir
            ocamlPackages.utop
            ocamlPackages.ocamlformat
            ocamlPackages.js_of_ocaml
            ocamlPackages.js_of_ocaml-ppx
            nixfmt-rfc-style
            dune_3
            python3 # for python3 -m http.server
          ];
        };
      }
    );
}
