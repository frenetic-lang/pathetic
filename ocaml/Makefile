LIBRARIES = OpenFlowLib

SUFFIXES = .a .cmxa .cma .d.cma

.PHONY: all doc clean $(LIBRARIES)

OCAMLBUILD = ocamlbuild -use-ocamlfind $(addprefix -I ,$(LIBRARIES))

OCAMLFIND = ocamlfind

%.d.byte:
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug $@

%.native %.byte:
	$(OCAMLBUILD) $@

%.d.cma:
	$(OCAMLBUILD) -no-links -cflag -ppopt -cflag -lwt-debug $@

%.a %.cmxa %.cma:
	$(OCAMLBUILD) -no-links $@

$(LIBRARIES):
	$(foreach SUFFIX, $(SUFFIXES), \
	    $(OCAMLBUILD) $(addsuffix $(SUFFIX),$@/$@);)

all: $(LIBRARIES)
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