# Helper to add Atlas spec indexing commands to flake apps.
# Import in flake.nix outputs with:
#
#   let
#     specApps = import ./nix/spec-index-apps.nix {
#       inherit pkgs;
#       root = ./.;
#     };
#   in {
#     apps.${system} = (apps.${system} or {}) // specApps.apps;
#   }
#
# Provides:
#   nix run .#spec-index        # generate under spec/v1 (auto-fallback to s/v)
#   nix run .#spec-index-auto   # auto-detect root (nil), same as above
{ pkgs
, root     # project root path, e.g. ./. 
}:

let
  emacs = pkgs.emacs-nox;

  mkIndex = pkgs.writeShellApplication {
    name = "atlas-spec-index";
    runtimeInputs = [ emacs ];
    text = ''
      set -euo pipefail
      cd "${root}"
      # Prefer working copy script; Nix will copy it into the store on build, preserving current content.
      emacs -Q --batch \
        -l "${root}/spec/scripts/mk-index.el" \
        --eval '(atlas-spec-generate nil)'
    '';
  };

  mkIndexAuto = pkgs.writeShellApplication {
    name = "atlas-spec-index-auto";
    runtimeInputs = [ emacs ];
    text = ''
      set -euo pipefail
      cd "${root}"
      ARG="${1:-}"
      if [ -z "$ARG" ]; then
        emacs -Q --batch \
          -l "${root}/spec/scripts/mk-index.el" \
          --eval '(atlas-spec-generate nil)'
      else
        # Ensure trailing slash
        case "$ARG" in
          */) ROOT="$ARG" ;;
          *)  ROOT="$ARG/";;
        esac
        emacs -Q --batch \
          -l "${root}/spec/scripts/mk-index.el" \
          --eval "(atlas-spec-generate (expand-file-name \"$ROOT\" default-directory))"
      fi
    '';
  };
in {
  apps = {
    spec-index = {
      type = "app";
      program = "${mkIndex}/bin/atlas-spec-index";
    };
    spec-index-auto = {
      type = "app";
      program = "${mkIndexAuto}/bin/atlas-spec-index-auto";
    };
  };
}
