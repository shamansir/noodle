{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
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
        let
          pkgs = nixpkgsFor.${system};
          nodeDependencies = (pkgs.callPackage ./default.nix {}).nodeDependencies;
        in {
          # FIXME: on Mac ARM works only with `nix build --option system x86_64-darwin`

          default = pkgs.stdenv.mkDerivation {
            pname = "noodle";
            version = "1.0.0";
            src = ./.;

            buildInputs = with pkgs; [
              cacert
              nodejs_20
              purs-tidy-bin.purs-tidy-0_10_0
              git
              dhall
              purs-backend-es
              purs-bin.purs-0_15_9
              #spago-bin.spago-0_21_0
              spago-unstable
            ];

            buildPhase = ''
              # Create a temporary cache directory for spago
              export XDG_CACHE_HOME=$(mktemp -d)
              export HOME=$(mktemp -d)
              export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
              ln -s ${nodeDependencies}/lib/node_modules ./node_modules
              export PATH="${nodeDependencies}/bin:$PATH"
              spago build --output output.nix
            '';

            installPhase = ''
              mkdir -p $out
              cp -r output.nix $out/
              cp -r ndf $out/
            '';
          };
        });

      apps = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          nodeDependencies = (pkgs.callPackage ./default.nix {}).nodeDependencies;

        in {

          default = let
            runCli = pkgs.writeShellApplication {
              name = "run-cli";
              runtimeInputs =
                with pkgs; [
                  # cacert
                  nodejs_20
                  # purs-tidy-bin.purs-tidy-0_10_0
                  # git
                  # dhall
                  # purs-backend-es
                  # purs-bin.purs-0_15_9
                  #spago-bin.spago-0_21_0
                  spago-unstable
                ];
              text = ''
                # node ./test-blessed.js
                # spago run --demo
                spago run --output output.nix
              '';
            };
          in {
            type = "app";
            program = "${runCli}/bin/run-cli";
          };

        });

      devShells = forAllSystems (system:
        # pkgs now has access to the standard PureScript toolchain
        # FIXME: on Mac ARM works only with `nix develop --option system x86_64-darwin`

        let
          pkgs = nixpkgsFor.${system};
          nodeDependencies = (pkgs.callPackage ./default.nix {}).nodeDependencies;
        in {
          default = pkgs.mkShell {
            name = "noodle";
            inputsFrom = builtins.attrValues self.packages.${system};
            shellHook = ''
              ln -s ${nodeDependencies}/lib/node_modules ./node_modules
              export PATH="${nodeDependencies}/bin:$PATH"
            '';
            buildInputs = with pkgs; [
              nodejs_23
              purs-bin.purs-0_15_9
              spago-unstable
              purs-tidy-bin.purs-tidy-0_10_0
              purs-backend-es
            ];
          };
        });
  };
}