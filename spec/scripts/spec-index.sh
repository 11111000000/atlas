#!/usr/bin/env bash
set -euo pipefail

# Atlas spec index generator wrapper.
# Usage:
#   ./spec-index.sh                 # auto-detects root: ./spec/v1 or ./s/v
#   ./spec-index.sh spec/v1/        # explicit root
#   ./spec-index.sh s/v/            # explicit root

ROOT_ARG="${1:-}"

# Resolve script dir to locate mk-index.el
SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd -P)"
ELISP="$SCRIPT_DIR/mk-index.el"

if [[ ! -f "$ELISP" ]]; then
  echo "Error: mk-index.el not found at $ELISP" >&2
  exit 1
fi

if [[ -z "${ROOT_ARG}" ]]; then
  emacs -Q --batch -l "$ELISP" --eval "(atlas-spec-generate nil)"
else
  # Normalize to directory
  case "$ROOT_ARG" in
    */) ROOT_DIR="$ROOT_ARG" ;;
    *)  ROOT_DIR="$ROOT_ARG/";;
  esac
  emacs -Q --batch -l "$ELISP" --eval "(atlas-spec-generate (expand-file-name \"$ROOT_DIR\" default-directory))"
fi
