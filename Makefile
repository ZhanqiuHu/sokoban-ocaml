<<<<<<< HEAD
MODULES=types author main command state
=======

MODULES= types command state main author
>>>>>>> b048ece7eaeb8a0285d5dd4c00a8ff8c2b4551ee
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte

OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

clean:
	ocamlbuild -clean

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

	
# clean:
# 	ocamlbuild -clean
# 	rm -rf _doc.public _doc.private adventure.zip

