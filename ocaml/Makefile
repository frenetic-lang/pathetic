.PHONY: all doc
OCAMLBUILD = ocamlbuild -use-ocamlfind 

all:
	$(OCAMLBUILD) main.otarget

native:
	$(OCAMLBUILD) native.otarget

doc:
	$(OCAMLBUILD) doc.docdir/index.html

test: _build/Test.d.byte
	$<

clean:
	rm -rf _build