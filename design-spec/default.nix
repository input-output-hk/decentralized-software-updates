# TODO: this has to be unified with the rest of the repo. Setting up the nix
# build in this way now so that I can get access to a compiling document.
{ sources ? import ./nix/sources.nix # See: https://github.com/nmattia/niv and https://nix.dev/tutorials/towards-reproducibility-pinning-nixpkgs.html#dependency-management-with-niv
, pkgs ? import sources.nixpkgs {}   # Use the pinned sources.
}:

with pkgs;

stdenv.mkDerivation {
  name = "decentralized-updates-design-spec";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      scheme-full # TODO: you might want to choose a smaller scheme.

                      # Add other LaTeX libraries (packages) here as needed, e.g:
                      # stmaryrd pgf

                      # build tools
                      latexmk
                      ;
                  })
                  glibcLocales
                ];
  src = ./.;
  buildPhase = "make";

  meta = with lib; {
    description = "Decentralized updates design specification";
    license = licenses.bsd3;
    platforms = platforms.linux;
  };
}
