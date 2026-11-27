{
  description = "An extensible EDSL for high-performance computing.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    sleef-src = {
      url = "git+https://github.com/shibatch/sleef?ref=refs/tags/3.9.0&submodules=1";
      flake = false;
    };
    argobots-src = {
      url = "github:pmodels/argobots/v1.2";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, sleef-src, argobots-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { system = system; config.allowUnfree = true; };
        haskellPackages = pkgs.haskellPackages;

        argobots = pkgs.callPackage ./nix/argobots.nix { inherit argobots-src; };

        sleef = pkgs.stdenv.mkDerivation {
          pname = "sleef";
          version = "3.9.0";
          src = sleef-src;
          nativeBuildInputs = [ pkgs.cmake pkgs.ninja ];
          cmakeFlags = [
            "-DSLEEF_BUILD_SHARED_LIBS=ON"
            "-DSLEEF_BUILD_INLINE_HEADERS=ON"
            "-DSLEEF_BUILD_TESTS=OFF"
            "-DSLEEF_ENABLE_TLFLOAT=OFF"
            "-DSLEEF_DISABLE_MPFR=ON"
            "-DSLEEF_DISABLE_SSL=ON"
            "-DSLEEF_DISABLE_FFTW=ON"
            "-DSLEEF_BUILD_DFT=OFF"
            "-DSLEEF_BUILD_QUAD=OFF"
            "-DSLEEF_BUILD_GNUABI_LIBS=OFF"
          ];
        };

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "janus";

        derivation =
          { mkDerivation
          , stdenv
          , lib
          , base
          , adjunctions
          , argobots
          , async
          , base16-bytestring
          , concurrency
          , constraints
          , constraints-extras
          , containers
          , cryptohash-sha256
          , cudaPackages
          , dejafu
          , dependent-hashmap
          , dependent-sum
          , dependent-sum-template
          , distributive
          , exceptions
          , gcc
          , hashable
          , hkd
          , lens
          , language-c-quote
          , libffi
          , linear
          , linuxPackages
          , profunctors
          , rock
          , semigroupoids
          , semilattices
          , sleef
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
                adjunctions
                async
                base16-bytestring
                concurrency
                constraints
                constraints-extras
                containers
                cryptohash-sha256
                dejafu
                dependent-hashmap
                dependent-sum
                dependent-sum-template
                distributive
                exceptions
                hashable
                hkd
                lens
                language-c-quote
                libffi
                linear
                profunctors
                rock
                semigroupoids
                semilattices
                transformers
                vector
                zstd
              ];
            libraryPkgconfigDepends = [ argobots sleef (pkgs.cudaPackages.libnvjitlink.overrideAttrs (oldAttrs: {
              postFixup = (oldAttrs.postFixup or "") + ''
substituteInPlace $dev/share/pkgconfig/nvjitlink.pc --replace "-lnvjitlink" "-lnvJitLink"
substituteInPlace $dev/share/pkgconfig/nvjitlink-12.9.pc --replace "-lnvjitlink" "-lnvJitLink"'';
            })) ];
            librarySystemDepends = [ argobots cudaPackages.cudatoolkit gcc linuxPackages.nvidia_x11 sleef ];
            testHaskellDepends = [ concurrency dejafu hkd linear semilattices tasty-dejafu tasty-discover tasty-hedgehog tasty-hunit ];
            description = "An extensible EDSL for high-performance computing.";
            license = "unknown";
            hydraPlatforms = lib.platforms.none;
          };

        pkg = pkgs.haskell.lib.overrideCabal ((haskellPackages.override {
          overrides = self: super: rec {
            dependent-hashmap = pkgs.haskell.lib.dontCheck (super.dependent-hashmap.overrideAttrs (a : { meta = { broken = false; }; patches = (a.patches or []) ++ [./foldl.patch]; }));
            hkd = jailbreakUnbreak super.hkd;
            rock = jailbreakUnbreak super.rock;
            semilattices = jailbreakUnbreak super.semilattices;
          };
        }).callPackage derivation { inherit argobots sleef; }) (old: {
          preBuild = (old.preBuild or "") + ''
            export ARGOBOTS_PATH=${argobots}
            export SLEEF_PATH=${sleef}
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ argobots pkgs.linuxPackages.nvidia_x11 pkgs.cudaPackages.cudatoolkit pkgs.cudaPackages.libnvjitlink.lib sleef ]}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
          '';
          testToolDepends = (old.testToolDepends or []) ++ [ pkgs.gcc pkgs.cudaPackages.cudatoolkit ];
        });

      in {
        packages = {
          ${packageName} = pkg;
          argobots = argobots;
          sleef = sleef;
        };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = haskellPackages.shellFor {
          packages = p: [ pkg ];
          buildInputs = with haskellPackages; pkg.env.buildInputs ++ [
            argobots
            cabal-install
            haskell-language-server
            sleef
          ];
          withHoogle = true;
          shellHook = ''
            export PATH=/run/current-system/sw/bin:${pkgs.gcc}/bin:$PATH
            export CUDA_PATH=${pkgs.cudaPackages.cudatoolkit}
            export ARGOBOTS_PATH=${argobots}
            export SLEEF_PATH=${sleef}
            export C_INCLUDE_PATH=${argobots}/include:${sleef}/include''${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}
            export LIBRARY_PATH=${argobots}/lib:${sleef}/lib''${LIBRARY_PATH:+:$LIBRARY_PATH}
            export PKG_CONFIG_PATH=${argobots}/lib/pkgconfig:${sleef}/lib/pkgconfig''${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}
            export LD_LIBRARY_PATH=${argobots}/lib:${sleef}/lib:${pkgs.cudaPackages.libnvjitlink.lib}/lib:${pkgs.lib.makeLibraryPath pkg.env.buildInputs}:$LD_LIBRARY_PATH
            export EXTRA_LDFLAGS="-L${pkgs.linuxPackages.nvidia_x11}/lib"
         '';
        };

      });
}
