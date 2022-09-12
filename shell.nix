{ pkgs ? import <nixpkgs> { } }:
let
  # Do not touch this part.
  def = import ../dotfiles/dev-shell/haskell;
in
pkgs.mkShell {
  buildInputs = def.buildInputs ++
    [
      pkgs.zlib.dev
      pkgs.libGL.dev
      pkgs.libGLU.dev
      pkgs.pkg-config
      pkgs.SDL2.dev
    ];
  #LD_LIBRARY_PATH = def.LD_LIBRARY_PATH;
}
