
MODULES= map types command state main authors genmap gui text
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind \
	-plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=unix,ounit2,str,qcheck

OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS) -pkg camlimages

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

clean:
	ocamlbuild -clean

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

docs: docs-public 

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.public $(MLIS)

# docs-private: build
# 	mkdir -p _doc.private
# 	ocamlfind ocamldoc -I _build -package $(PKGS) \
# 		-html -stars -d _doc.private \
# 		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)


zip:
	zip MS3.zip *images/*.png* *.ml* *.sh *.txt *.md _tags .merlin .ocamlformat .ocamlinit Makefile	
	

