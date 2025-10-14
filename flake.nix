{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        deps = with pkgs.sbcl.pkgs; [
          clingon
          ironclad
          fiveam
          local-time
        ];

        name = "cltpt";
        cltpt-lisp = pkgs.sbcl.buildASDFSystem {
          pname = name;
          version = "dev";
          src = ./.;
          systems = [ "cltpt" ];
          lispLibs = deps;
        };

        sbcl' = pkgs.sbcl.withOverrides (
          self: super: {
            inherit cltpt-lisp;
          }
        );
        sbcl'' = sbcl'.withPackages (ps: [ cltpt-lisp ]);

        build-script = pkgs.writeText "build.lisp" ''
          (require :asdf)
          (asdf:load-system :${name})
          (sb-ext:save-lisp-and-die "${name}"
           :executable t
           :purify t
           #+sb-core-compression :compression
           #+sb-core-compression t
          :toplevel (lambda () (cltpt/commandline:commandline-main (uiop:command-line-arguments)))
          )
        '';

        pkg = pkgs.stdenv.mkDerivation {
          pname = name;
          version = "0.0.1";
          src = ./.;
          nativeBuildInputs = [ pkgs.sbcl sbcl'' pkgs.makeWrapper ];
          dontStrip = true;
          buildPhase = ''
            ${sbcl''}/bin/sbcl --script ${build-script}
          '';
          installPhase = ''
            install -D ${name} $out/bin/${name}
          '';
        };

      in {
        packages = {
          default = pkg;
          cltpt = pkg;
          cltpt-lib = cltpt-lisp;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            sbcl''
            pkgs.rlwrap
          ];
          shellHook = ''
            echo "run with 'sbcl --load main.lisp [args..]' or './main.sh [args..]'"
          '';
        };

        apps.default = { type = "app"; program = "${pkg}/bin/${name}"; };
      });
}