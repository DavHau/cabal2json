{
  description = "Convert cabal files to json";

  nixConfig = {
    extra-trusted-public-keys = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=";
    extra-substituters = "https://hydra.iohk.io";
  };

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    all-cabal-files.url = "github:commercialhaskell/all-cabal-files/hackage";
    all-cabal-files.flake = false;
  };

  outputs = {
    self,
    all-cabal-files,
    haskellNix,
    nixpkgs,
  } @ inp: let
    l = nixpkgs.lib // builtins;
    supportedSystems = ["x86_64-linux"];

    forAllSystems = f: l.genAttrs supportedSystems
      (system: f system nixpkgs.legacyPackages.x86_64-linux
        (import haskellNix.inputs.nixpkgs-unstable {
          inherit system;
          overlays = [haskellNix.overlay];
        }));

  in {
    devShell = forAllSystems (system: pkgs: pkgsHaskell: pkgs.mkShell {
      buildInputs = with pkgs; [
        ghcid
        stack
        haskell-language-server
      ];
    });

    packages = forAllSystems (system: pkgs: pkgsHaskell: {

      cabal2json-nixpkgs = pkgs.haskell.packages.ghc922.callPackage ./nix/cabal2json.nix {};

      cabal2json =
        let
          flake = (pkgsHaskell.haskell-nix.project' {
            src = builtins.path {
              path = ./.;
              name = "src";
              filter = path: _: ! l.elem (l.baseNameOf path) ["flake.nix" "flake.lock"];
            };
          }).flake {};
        in
          flake.packages."cabal2json:exe:cabal2json";

    });

    checks = forAllSystems (system: pkgs: pkgsHaskell: {
      all-cabal-json-files =
        let
          script = pkgs.writeScript "call-cabal2json" ''
            #!${pkgs.bash}/bin/bash

            cabalFile=$1
            targetFile=$out/''${cabalFile%.*}.json
            echo "creating: $targetFile"
            mkdir -p $(dirname $targetFile)
            ${self.packages.${system}.cabal2json}/bin/cabal2json $cabalFile > $targetFile
          '';
        in
          pkgs.runCommand "all-cabal-json-files" {} ''
            cd ${all-cabal-files}
            ${pkgs.parallel}/bin/parallel \
              --halt now,fail,1 \
              -a <(${pkgs.findutils}/bin/find . -type f -name '*.cabal') \
              bash ${script}
        '';
    });
  };
}
