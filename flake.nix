{
  description = "sdp management";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    advisory-db = {
      url = "github:rustsec/advisory-db";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    rust-overlay,
    ...
  } @ attrs: (flake-utils.lib.eachDefaultSystem (
    system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [rust-overlay.overlays.default];
      };
      inherit (pkgs) callPackage;

      common = callPackage ./common.nix (attrs
        // {
          inherit pkgs;
        });
    in {
      formatter = pkgs.alejandra;
      checks = common.checks;
      packages = common.packages;
      devShells = common.devShells;

      apps = {
        provisioner = {
          type = "app";
          program = "${common.packages.provisioner}/bin/provisioner";
          meta = {
            description = "Provisioning";
          };
        };
      };
    }
  ));
}
