let
  nixpkgs_pinned = {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "c64639b54caa6595f9ef62ed2548593b5fe5db66";
    sha256 = "02j2nkykafvcbv0aclvv864232wxzf8viafrcp80cwif0gacp2f6";
  };
in import ((import <nixpkgs> {}).fetchFromGitHub nixpkgs_pinned)
