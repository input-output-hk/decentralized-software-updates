# our packages overlay
pkgs: _:
with pkgs; {
  cbor-diag = pkgs.callPackage ./pkgs/cbor-diag { };
  cddl = pkgs.callPackage ./pkgs/cddl { };
}
