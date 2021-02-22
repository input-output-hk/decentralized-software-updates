{ pkgs ? (import  ../default.nix {}).pkgs
}:

with pkgs;

latex.buildLatex {
  name = "decentralized-updates";
  texFiles = [ "decentralized-updates" ];
  meta = with lib; {
    description = "Specification of a Decentralized Update Mechanism for Cardano";
    license = licenses.asl20;
    platforms = platforms.linux;
  };
  src = latex.filterLatex ./.;

  texInputs = {
    inherit (texlive)

    scheme-small

    # libraries
    stmaryrd lm-math amsmath extarrows cleveref semantic tikz-cd xcolor

    # bclogo and dependencies
    bclogo mdframed xkeyval etoolbox needspace pgf

    # font libraries `mathpazo` seems to depend on palatino , but it isn't
    # pulled.
    mathpazo palatino microtype

    # libraries for marginal notes
    xargs todonotes

    pgfplots

    # build tools
    latexmk
    ;

  };
  buildInputs = [ gitMinimal ];
}
