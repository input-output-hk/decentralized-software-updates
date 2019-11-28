{ pkgs ? (import  ../default.nix {}).pkgs
}:

with pkgs;

stdenv.mkDerivation {
  name = "decentralized-updates";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      scheme-small

                      # libraries
                      stmaryrd lm-math amsmath extarrows cleveref semantic tikz-cd xcolor

                      # bclogo and dependencies
                      bclogo mdframed xkeyval etoolbox needspace pgf

                      # font libraries `mathpazo` seems to depend on palatino
                      # , but it isn't pulled.
                      mathpazo palatino microtype

                      # libraries for marginal notes
                      xargs todonotes

                      # build tools
                      latexmk
                      ;
                  })

                ];
  src = ./.;
  buildPhase = "make";

  meta = with lib; {
    description = "Specification of a Decentralized Update Mechanism for Cardano";
    license = licenses.bsd3;
    platforms = platforms.linux;
  };
}
