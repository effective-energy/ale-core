{ prId ? "master", nixpkgs ? <nixpkgs> }:

let

  pkgs = import nixpkgs {};

  cacheStackWorkDirs = builtins.readFile ./release/cache_stack_work_dirs; 
  jobs = {
    stack-test = pkgs.releaseTools.nixBuild {
      name = "ale-stack-test";
      src = ./.;
      nativeBuildInputs = with pkgs; [
        git  # stack uses it to fetch extra-deps
        nix
        stack
      ];

      NIX_PATH = "nixpkgs=${nixpkgs}";
      NIX_REMOTE = "daemon";
      CACHE_STACK_WORK_DIRS = cacheStackWorkDirs;
      PR_ID=prId;
      buildCommand = builtins.readFile ./release/stack_build_with_caching;
      failureHook = cacheStackWorkDirs;
    };
  };
in jobs
