{ system ? builtins.currentSystem, pkgs ? (import ./nixpkgs.nix { inherit system; }) }:

with pkgs.lib;
with pkgs.haskell.lib;

let
  jemalloc450 = import ./jemalloc450.nix { inherit pkgs; };

  ale_stack2nix = (import ./build/nix/ale.nix { inherit pkgs; }).override {
    overrides = self: super: {
      rocksdb-haskell = super.rocksdb-haskell.override {
        rocksdb = pkgs.rocksdb.override { jemalloc = jemalloc450; };
      };

      ale-core-node = dontCheck super.ale-core-node;  # ale-ping test
    };
  };

  docs = (import ./release/docs.nix { inherit pkgs; });

  ###

  ale = {
    core       = ale_stack2nix.ale-core-core;
    core-tools = justStaticExecutables ale_stack2nix.ale-core-tools-common;
    node       = justStaticExecutables ale_stack2nix.ale-core-node;
    wallet     = justStaticExecutables ale_stack2nix.ale-core-wallet;
    hub        = justStaticExecutables ale_stack2nix.ale-core-hub;
  };

  # HACK: Better to use `writeShellScriptBin` but it has a bug
  #       that is not fixed in our version of `nixpkgs` yet.
  docs-serve = pkgs.writeScriptBin "ale-docs-serve" ''
    #!${pkgs.stdenv.shell}
    cd "${docs.all}" && ${pkgs.python3}/bin/python3 -m http.server --bind localhost 8080
  '';

  ###

  misc = {
    stack2nix = import (pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "stack2nix";
      rev = "486a88f161a08df8af42cb4c84f44e99fa9a98d8";
      sha256 = "0nskf1s51np320ijlf38sxmksk68xmg14cnarg1p9rph03y81m7w";
    }) { inherit pkgs; };
  };

in {
  asn = ale_stack2nix;
  inherit ale;
  docs = docs // { serve = docs-serve; };
} // misc
