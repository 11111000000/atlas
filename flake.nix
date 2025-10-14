{
  description = "Atlas.el - run ERT tests in batch";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs }:
  let
    systems = [ "x86_64-linux" "aarch64-linux" ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
  in
  {
    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        packages = with pkgs; [ emacs-nox ripgrep ];
        shellHook = ''
          echo "Run tests: emacs -Q --batch -L lisp -l test/ert-runner.el"
          echo "Or via flake app: nix run .#tests"
          echo "Generate spec index: nix run .#spec-index"
        '';
      };
    });

    # nix run .#tests, nix run .#spec-index
    apps = forAllSystems (pkgs:
      let
        emacs = pkgs.emacs-nox;
        testsDrv = pkgs.writeShellApplication {
          name = "atlas-tests";
          # rg may be used by tests/getters
          runtimeInputs = [ pkgs.ripgrep ];
          text = ''
            set -euo pipefail
            exec ${emacs}/bin/emacs -Q --batch -L ${./lisp} -l ${./test}/ert-runner.el
          '';
        };
        specIndexDrv = pkgs.writeShellApplication {
          name = "atlas-spec-index";
          runtimeInputs = [ pkgs.emacs-nox ];
          text = ''
            set -euo pipefail
            # Generate spec-index.sexp and link-map.sexp (auto-detects spec root: ./spec/v1 or ./s/v)
            exec ${emacs}/bin/emacs -Q --batch \
              --eval "(progn (load-file (expand-file-name \"spec/scripts/mk-index.el\" default-directory)) (atlas-spec-generate nil))"
          '';
        };
      in rec {
        tests = { type = "app"; program = "${testsDrv}/bin/atlas-tests"; };
        spec-index = { type = "app"; program = "${specIndexDrv}/bin/atlas-spec-index"; };
        default = tests;
      });

    checks = forAllSystems (pkgs: {
      ert = pkgs.runCommand "atlas-ert" { buildInputs = [ pkgs.emacs-nox pkgs.ripgrep ]; } ''
        cp -r ${./lisp} ./lisp
        cp -r ${./test} ./test
        ${pkgs.emacs-nox}/bin/emacs -Q --batch -L lisp -l test/ert-runner.el
        touch $out
      '';
    });
  };
}
