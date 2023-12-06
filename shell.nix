{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.ghc.withPackages (h: with h; [hspec megaparsec]))
    pkgs.haskellPackages.ghcid

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
