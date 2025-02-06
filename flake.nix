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

            buildInputs = with pkgs; [
              cacert
              nodejs
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
              spago build
            '';

            installPhase = ''
              mkdir -p $out
              cp -r output $out/
              cp -r ndf $out/
            '';
          };
        });

      apps = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in {

          default = let
            runCli = pkgs.writeShellApplication {
              name = "run-cli";
              runtimeInputs =
                with pkgs; [
                  # cacert
                  nodejs
                  # purs-tidy-bin.purs-tidy-0_10_0
                  # git
                  # dhall
                  # purs-backend-es
                  # purs-bin.purs-0_15_9
                  #spago-bin.spago-0_21_0
                  spago-unstable
                ];
              text = ''
                spago run
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
        let pkgs = nixpkgsFor.${system}; in {
          default = pkgs.mkShell {
            name = "noodle";
            inputsFrom = builtins.attrValues self.packages.${system};
            buildInputs = with pkgs; [
              nodejs
              purs-bin.purs-0_15_9
              #spago-bin.spago-0_21_0
              spago-unstable
              purs-tidy-bin.purs-tidy-0_10_0
              purs-backend-es
            ];
          };
        });
  };
}