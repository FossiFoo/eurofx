{
  description = "OES development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = {self, nixpkgs}:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
        forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
          pkgs = import nixpkgs {
              inherit system;
              inherit (nixpkgs) glibcLocales;
              inherit (nixpkgs.stdenv) isLinux;
              inherit (nixpkgs.lib) optionalString;
            };
        });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: import ./dependencies.nix { pkgs = pkgs; });
    };
}
