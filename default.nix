{ stdenv, haskellPackages, nodejs }:

let env = haskellPackages.ghcWithPackages (p: with p; [
      lens reflex-dom
    ]);
in
  stdenv.mkDerivation {
    name = "card-game";
    buildInputs = [ env nodejs ];
    shellHook   = ''
      export NIX_GHC="${env}/bin/ghcjs"
      export NIX_GHCPKG="${env}/bin/ghcjs-pkg"
      export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
    '';
  }
