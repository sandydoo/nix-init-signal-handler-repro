{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = f: builtins.listToAttrs (map (name: { inherit name; value = f name; }) systems);
    in rec {
      packages = forAllSystems (system: 
        let 
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.haskellPackages.callCabal2nix "nix-init-signal-handler-repro" ./.
            { nix = pkgs.nix; };
        }
      );

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        pkgs.mkShell {
          inputsFrom = [packages.${system}.default];
          nativeBuildInputs = [pkgs.pkgs-config];
        }
      );
    };
}
