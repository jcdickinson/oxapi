{
  pkgs,
  crane,
  ...
}: let
  inherit (pkgs) lib;

  rustToolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;

  craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.toml
      ./Cargo.lock
      ./crates
    ];
  };

  commonArgs = {
    inherit src;
    strictDeps = true;

    nativeBuildInputs = with pkgs; [
      pkg-config
    ];

    buildInputs = with pkgs; [
      openssl.dev
    ];
  };

  cargoArtifacts = craneLib.buildDepsOnly (commonArgs
    // {
      # Can't get deps caching to work.
      # crane expects target/release to exist.
      buildPhaseCargoCommand = ''
        mkdir -p target/release
        touch target/release/.keep
      '';
      checkPhaseCargoCommand = ''
        mkdir -p target/release
        touch target/release/.keep
      '';
    });

  individualCrateArgs =
    commonArgs
    // {
      inherit cargoArtifacts src;
      inherit (craneLib.crateNameFromCargoToml {inherit src;}) version;
      doCheck = false;
    };
in rec {
  inherit craneLib;

  checks = {
    inherit (packages) provisioner;

    clippy = craneLib.cargoClippy (
      commonArgs
      // {
        inherit cargoArtifacts;
        cargoClippyExtraArgs = "--all-targets -- --deny warnings";
      }
    );

    fmt = craneLib.cargoFmt {
      inherit src;
    };

    test = craneLib.cargoNextest (
      commonArgs
      // {
        inherit cargoArtifacts;
        partitions = 1;
        partitionType = "count";
        cargoNextestPartitionsExtraArgs = "--no-tests=pass";
      }
    );
  };

  packages = {
    provisioner = craneLib.buildPackage (
      individualCrateArgs
      // {
        pname = "provisioner";
        cargoExtraArgs = "-p a8a-provisioner";
        postInstall = ''
          mv $out/bin/a8a-provisioner $out/bin/provisioner
        '';
      }
    );

    backend = craneLib.buildPackage (
      individualCrateArgs
      // {
        pname = "mgmt-backend";
        cargoExtraArgs = "-p a8a-backend";
        postInstall = ''
          mv $out/bin/a8a-backend $out/bin/mgmt_backend
        '';
      }
    );

    provisionerContainer = pkgs.dockerTools.buildLayeredImage {
      name = "smp-provisioner";
      tag = "latest";
      contents = [
        packages.provisioner
        pkgs.dockerTools.caCertificates
      ];
      config = {
        Entrypoint = ["/bin/provisioner"];
        WorkingDir = "/";
      };
    };

    backendContainer = pkgs.dockerTools.buildLayeredImage {
      name = "smp-backend";
      tag = "latest";
      contents = [
        packages.backend
        pkgs.dockerTools.caCertificates
      ];
      config = {
        Entrypoint = ["/bin/mgmt_backend"];
        WorkingDir = "/";
        Env = [
          "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
          "SSL_CERT_DIR=${pkgs.cacert}/etc/ssl/certs"
        ];
      };
    };
  };

  devShells.default =
    pkgs.mkShell.override {
      stdenv = pkgs.stdenvAdapters.useMoldLinker pkgs.clangStdenv;
    } {
      inputsFrom = builtins.attrValues checks;

      shellHook = with pkgs; ''
        export LD_LIBRARY_PATH="${lib.makeLibraryPath [openssl]}:$LD_LIBRARY_PATH"
      '';

      packages = with pkgs; [
        craneLib.rustc
        craneLib.cargo
        craneLib.clippy
        craneLib.rustfmt
        just
        sqlx-cli
        nodejs
        jq
        podman-compose
        bacon
        tmux
        llvmPackages.clangUseLLVM
        lld
        buildah
        opentofu
      ];
    };
}
