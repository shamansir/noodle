{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ps-overlay.url = "github:thomashoneyman/purescript-overlay";
    mkSpagoDerivation = {
      url = "github:jeslie0/mkSpagoDerivation";
      inputs = {
        registry.url = "github:purescript/registry/fe3f499706755bb8a501bf989ed0f592295bb4e3";
        registry-index.url = "github:purescript/registry-index/a349ca528812c89915ccc98cfbd97c9731aa5d0b";
      };
    };
  };

  outputs = { self, nixpkgs, flake-utils, ps-overlay, mkSpagoDerivation }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ mkSpagoDerivation.overlays.default
                       ps-overlay.overlays.default
                     ];
        };

        noodleCliPackage =
            pkgs.mkSpagoDerivation {
              name = "noodle";
              spagoYaml = ./spago.yaml;
              spagoLock = ./spago.lock;
              src = ./.;
              version = "0.1.0";
              nativeBuildInputs = [ pkgs.esbuild pkgs.purs-backend-es pkgs.purs-unstable pkgs.spago-unstable ];
              #buildPhase = "spago bundle-app --output ./output-es -m Cli.Main"
              buildPhase = "spago build --output ./output-es && purs-backend-es bundle-app -m Cli.Main --no-build --minify --to=main.min.js --platform=node";
              #buildPhase = ''
              #  find ./node_modules/reblessed/dist -name "*.d.ts.map" -exec rm {} \;
              # nvim ./node_modules/reblessed/dist/lib/unicode.js
              # spago build --output ./output-es && purs-backend-es bundle-app -m Cli.Main --no-build --minify --to=main.min.js --platform=node";
              # ''
              installPhase = "mkdir $out; cp -r main.min.js $out";
              buildNodeModulesArgs = {
                npmRoot = ./.;
                nodejs = pkgs.nodejs;
              };
            };

        runCompiledScriptWithNode = pkgs.runCommand "run-compiled-with-node" {} ''
            mkdir -p $out
            cat > $out/run-compiled-with-node.sh <<EOF
            #!/bin/sh
            exec ${pkgs.nodejs}/bin/node ${noodleCliPackage}/main.min.js "\$@"
            EOF
            chmod +x $out/run-compiled-with-node.sh
          '';


        noodleApp = {
            type = "app";
            program = "${runCompiledScriptWithNode}/run-compiled-with-node.sh";
        };

      in
        {
          packages.default = noodleCliPackage;

          apps.output1 = noodleApp;

          apps.default = noodleApp;
        }

    );
}
