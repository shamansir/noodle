{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        config = { };
        overlays = builtins.attrValues self.overlays;
      });
    in {
      overlays = {
        purescript = inputs.purescript-overlay.overlays.default;
      };

      packages = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in {
          # FIXME: on Mac ARM works only with `nix build --option system x86_64-darwin`

          default = pkgs.stdenv.mkDerivation {
            pname = "noodle";
            version = "1.0.0";
            src = ./.;

            # This ensures the right versions of the tools are in the PATH.
            buildInputs = [
              pkgs.nodejs
              pkgs.purs-tidy-bin.purs-tidy-0_10_0
              pkgs.git
              pkgs.dhall
              pkgs.purs-backend-es
              pkgs.purs-bin.purs-0_15_9
              #pkgs.spago-bin.spago-0_21_0
              pkgs.spago-unstable
              # pkgs.spago-unstable
            ];

            # Optionally, set an environment variable so that spago can find the right purs
            # (if needed; depends on your spago configuration)
            # configureFlags = "--with-purs=${pkgs.purs-bin.purs-0_15_9}/bin/purs";

            buildPhase = ''
              # Create a temporary cache directory for spago
              export XDG_CACHE_HOME=$(mktemp -d)
              export HOME=$(mktemp -d)
              spago build
            '';

            # Here we assume that spago creates a directory (say, `output/`) with the build
            # artifacts. We then copy that to $out. Adjust as necessary for your project.
            installPhase = ''
              mkdir -p $out
              cp -r output $out/
            '';
          };
        });

      devShells = forAllSystems (system:
        # pkgs now has access to the standard PureScript toolchain
        # FIXME: on Mac ARM works only with `nix develop --option system x86_64-darwin`
        let pkgs = nixpkgsFor.${system}; in {
          default = pkgs.mkShell {
            name = "noodle";
            inputsFrom = builtins.attrValues self.packages.${system};
            buildInputs = with pkgs; [
              nodejs
              purs-bin.purs-0_15_9
              spago-bin.spago-0_21_0
              purs-tidy-bin.purs-tidy-0_10_0
              purs-backend-es
            ];
          };
        });
  };
}