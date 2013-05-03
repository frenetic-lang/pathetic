all:
	cd desmoines && $(MAKE)
	cd ocaml && $(MAKE)

test:
	cd ocaml && $(MAKE) test

clean:
	cd desmoines && $(MAKE) clean
	cd ocaml && $(MAKE) clean
