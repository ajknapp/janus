{
  description = "An extensible EDSL for high-performance computing.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "janus";

        derivation =
          { mkDerivation
          , stdenv
          , lib
          , base
          , async
          , base16-bytestring
          , containers
          , cryptohash-sha256
          , dependent-sum-template
          , gcc
          , hashable
          , lens
          , language-c-quote
          , libffi
          , mmap
          , profunctors
          , rock
          , semigroupoids
          , semilattices
          , transformers
          , vector
          , zstd
          }:
          mkDerivation {
            pname = packageName;
            version = "0.1.0.0";
            src = ./.;
            libraryHaskellDepends =
              [
                base
                async
                base16-bytestring
                containers
                cryptohash-sha256
                dependent-sum-template
                hashable
                lens
                language-c-quote
                libffi
                mmap
                profunctors
                rock
                semigroupoids
                semilattices
                transformers
                vector
                zstd
              ];
            librarySystemDepends = [ gcc ];
            description = "An extensible EDSL for high-performance computing.";
            license = "unknown";
            hydraPlatforms = lib.platforms.none;
          };

        pkg = (haskellPackages.override {
          overrides = self: super: rec {
            dependent-hashmap = pkgs.haskell.lib.dontCheck (super.dependent-hashmap.overrideAttrs (_ : { meta = { broken = false; }; }));
            rock = jailbreakUnbreak super.rock;
            semilattices = jailbreakUnbreak super.semilattices;
          };
        }).callPackage derivation {};

      in {
        packages.${packageName} = pkg;

        defaultPackage = self.packages.${system}.${packageName};

        devShell = haskellPackages.shellFor {
          packages = p: [ pkg ];
          buildInputs = with haskellPackages; pkg.env.buildInputs ++ [
            cabal-install
            haskell-language-server
          ];
          # shellHook = "export LIBRARY_PATH=${pkgs.lib.getLib pkgs.stdenv.cc.libc}/lib";
          withHoogle = true;
        };

      });
}

