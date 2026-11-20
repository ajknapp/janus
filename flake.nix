{
  description = "An extensible EDSL for high-performance computing.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    sleef-src = {
      url = "git+https://github.com/shibatch/sleef?ref=refs/tags/3.9.0&submodules=1";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, sleef-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { system = system; config.allowUnfree = true; };
        haskellPackages = pkgs.haskellPackages;

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
            libraryPkgconfigDepends = [ sleef (pkgs.cudaPackages.libnvjitlink.overrideAttrs (oldAttrs: {
              postFixup = (oldAttrs.postFixup or "") + ''
substituteInPlace $dev/share/pkgconfig/nvjitlink.pc --replace "-lnvjitlink" "-lnvJitLink"
substituteInPlace $dev/share/pkgconfig/nvjitlink-12.9.pc --replace "-lnvjitlink" "-lnvJitLink"'';
            })) ];
            librarySystemDepends = [ cudaPackages.cudatoolkit gcc linuxPackages.nvidia_x11 sleef ];
            testHaskellDepends = [ concurrency dejafu semilattices tasty-dejafu tasty-discover tasty-hedgehog tasty-hunit ];
            description = "An extensible EDSL for high-performance computing.";
            license = "unknown";
            hydraPlatforms = lib.platforms.none;
          };

        pkg = pkgs.haskell.lib.overrideCabal ((haskellPackages.override {
          overrides = self: super: rec {
            dependent-hashmap = pkgs.haskell.lib.dontCheck (super.dependent-hashmap.overrideAttrs (a : { meta = { broken = false; }; patches = (a.patches or []) ++ [./foldl.patch]; }));
            rock = jailbreakUnbreak super.rock;
            semilattices = jailbreakUnbreak super.semilattices;
          };
        }).callPackage derivation { inherit sleef; }) (old: {
          preBuild = (old.preBuild or "") + ''
            export SLEEF_PATH=${sleef}
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.linuxPackages.nvidia_x11 pkgs.cudaPackages.cudatoolkit pkgs.cudaPackages.libnvjitlink.lib sleef ]}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
          '';
          testToolDepends = (old.testToolDepends or []) ++ [ pkgs.gcc pkgs.cudaPackages.cudatoolkit ];
        });

      in {
        packages = {
          ${packageName} = pkg;
          sleef = sleef;
        };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = haskellPackages.shellFor {
          packages = p: [ pkg ];
          buildInputs = with haskellPackages; pkg.env.buildInputs ++ [
            cabal-install
            haskell-language-server
            sleef
          ];
          withHoogle = true;
          shellHook = ''
            export PATH=/run/current-system/sw/bin:${pkgs.gcc}/bin:$PATH
            export CUDA_PATH=${pkgs.cudaPackages.cudatoolkit}
            export SLEEF_PATH=${sleef}
            export C_INCLUDE_PATH=${sleef}/include''${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}
            export LIBRARY_PATH=${sleef}/lib''${LIBRARY_PATH:+:$LIBRARY_PATH}
            export PKG_CONFIG_PATH=${sleef}/lib/pkgconfig''${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}
            export LD_LIBRARY_PATH=${sleef}/lib:${pkgs.cudaPackages.libnvjitlink.lib}/lib:${pkgs.lib.makeLibraryPath pkg.env.buildInputs}:$LD_LIBRARY_PATH
            export EXTRA_LDFLAGS="-L${pkgs.linuxPackages.nvidia_x11}/lib"
         '';
        };

      });
}
