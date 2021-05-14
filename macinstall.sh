#!/bin/zsh
opam switch create bp 4.07.1
opam install graphics
opam update
opam upgrade
eval $(opam env)
opam install -y utop ounit qcheck ocaml-lsp-server ocamlformat yojson ansiterminal csv bisect_ppx-ocamlbuild menhir user-setup
opam user-setup install
opam install camlimages
