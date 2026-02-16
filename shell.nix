{ pkgs ? import <nixpkgs> {} }:

let
    inherit (pkgs) stdenv;
in
stdenv.mkDerivation {
    name = "node";
    packages = with pkgs; [
        nodejs-22
        nodePackages.firebase-tools
        nodePackages.node-gyp
    ];
    shellHook = ''
        export PATH="$PWD/node_modules/.bin/:$PATH"
        export NPM_PACKAGES="$HOME/.npm-packages"
    '';
}
