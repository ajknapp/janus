{
  description = "An extensible EDSL for high-performance computing.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { system = system; config.allowUnfree = true; };
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
          , concurrency
          , containers
          , cryptohash-sha256
          , cudaPackages
          , dejafu
          , dependent-sum-template
          , exceptions
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
          , tasty-dejafu
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
                concurrency
                containers
                cryptohash-sha256
                dejafu
                dependent-sum-template
                exceptions
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
            libraryPkgconfigDepends = [ (pkgs.cudaPackages.libnvjitlink.overrideAttrs (oldAttrs: {
              postFixup = (oldAttrs.postFixup or "") + ''
substituteInPlace $dev/share/pkgconfig/nvjitlink.pc --replace "-lnvjitlink" "-lnvJitLink"
substituteInPlace $dev/share/pkgconfig/nvjitlink-12.9.pc --replace "-lnvjitlink" "-lnvJitLink"'';
            })) ];
            librarySystemDepends = [ cudaPackages.cudatoolkit gcc linuxPackages.nvidia_x11 ];
            testHaskellDepends = [ concurrency dejafu tasty-dejafu tasty-discover tasty-hedgehog tasty-hunit ];
            description = "An extensible EDSL for high-performance computing.";
            license = "unknown";
            hydraPlatforms = lib.platforms.none;
          };

        pkg = (haskellPackages.override {
          overrides = self: super: rec {
            dependent-hashmap = pkgs.haskell.lib.dontCheck (super.dependent-hashmap.overrideAttrs (a : { meta = { broken = false; }; patches = (a.patches or []) ++ [./foldl.patch]; }));
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
            export PATH=/run/current-system/sw/bin:${pkgs.gcc}/bin:$PATH
            export CUDA_PATH=${pkgs.cudaPackages.cudatoolkit}
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath pkg.env.buildInputs}:$LD_LIBRARY_PATH
            export EXTRA_LDFLAGS="-L${pkgs.linuxPackages.nvidia_x11}/lib"
         '';
        };

      });
}
