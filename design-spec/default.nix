{ pkgs ? import ../nix/default.nix {} }:

with pkgs;

latex.buildLatex {
  name = "decentralized-updates-design-spec";
  texFiles = [ "abstract  macros  priviledge-decentralized-swupdates  title" ];
  meta = with lib; {
    description = "Decentralized updates design specification";
    license = licenses.asl20;
    platforms = platforms.linux;
  };
  src = latex.filterLatex ./.;

  texInputs = {
    inherit (texlive)
    scheme-full # TODO: you might want to choose a smaller scheme.

    # Add other LaTeX libraries (packages) here as needed, e.g:
    # stmaryrd pgf

    # build tools
    latexmk
    ;

  };
  buildInputs = [ gitMinimal ];
}
