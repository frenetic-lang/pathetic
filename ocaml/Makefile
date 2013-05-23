.PHONY: all doc
OCAMLBUILD = ocamlbuild -use-ocamlfind 

all:
	$(OCAMLBUILD) main.otarget

debug:
	$(OCAMLBUILD) debug.otarget

native:
	$(OCAMLBUILD) native.otarget

doc:
	$(OCAMLBUILD) doc.docdir/index.html

test: 
	$(OCAMLBUILD) test.otarget		

clean:
	rm -rf _build