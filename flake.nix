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
    
      
        nixTestPackage =
            pkgs.mkSpagoDerivation {
              spagoYaml = ./spago.yaml;
              spagoLock = ./spago.lock;
              src = ./.;
              version = "0.1.0";
              nativeBuildInputs = [ pkgs.esbuild pkgs.purs-backend-es pkgs.purs-unstable pkgs.spago-unstable ];
              buildPhase = "spago build --output ./output-es && purs-backend-es bundle-app --no-build --minify --to=main.min.js";
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
            exec ${pkgs.nodejs}/bin/node ${nixTestPackage}/main.min.js "\$@"
            EOF
            chmod +x $out/run-compiled-with-node.sh 
          '';


        nixTestApp = { 
            type = "app";
            program = "${runCompiledScriptWithNode}/run-compiled-with-node.sh";
        };

      in     
        {
          packages.default = nixTestPackage;
          
          apps.output1 = nixTestApp;

          apps.default = nixTestApp;
        }

    );
}
