{ pkgs ? import <nixpkgs> { } }:
let
  frontend = pkgs.mkYarnPackage {
    name = "ltl-visualizer";
    src = ./.;
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
  };
in pkgs.mkShell {
  inputsFrom = [ frontend ];
  buildInputs = [ pkgs.ghp-import ];
}

