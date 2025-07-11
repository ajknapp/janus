{
  description = "An extensible EDSL for high-performance computing.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { system = "x86_64-linux"; config.allowUnfree = true; };
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
          , cudaPackages
          , dependent-sum-template
          , gcc
          , hashable
          , lens
          , language-c-quote
          , libffi
          , linuxPackages
          , profunctors
          , rock
          , semigroupoids
          , semilattices
          , tasty-discover
          , tasty-hedgehog
          , tasty-hunit
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
                profunctors
                rock
                semigroupoids
                semilattices
                transformers
                vector
                zstd
              ];
            librarySystemDepends = [ cudaPackages.cudatoolkit cudaPackages.libnvjitlink gcc linuxPackages.nvidia_x11 ];
            testHaskellDepends = [tasty-discover tasty-hedgehog tasty-hunit];
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
          withHoogle = true;
          shellHook = ''
            export PATH=${pkgs.gcc}/bin:$PATH
            export CUDA_PATH=${pkgs.cudaPackages.cudatoolkit}
            export LD_LIBRARY_PATH=${pkgs.linuxPackages.nvidia_x11}/lib:${pkgs.cudaPackages.libnvjitlink.lib}/lib
            export EXTRA_LDFLAGS="-L${pkgs.linuxPackages.nvidia_x11}/lib"
            export EXTRA_CCFLAGS="-I/usr/include"
         '';
        };

      });
}
