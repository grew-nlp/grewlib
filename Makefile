OCB_FLAGS = -use-ocamlfind -use-menhir -I src
OCB = ocamlbuild $(OCB_FLAGS)

LIB_FILES = libgrew.cma libgrew.cmxa libgrew.a libgrew.cmi libgrew_types.cmi libgrew.cmx
INSTALL_FILES = $(LIB_FILES:%=_build/src/%)

VERSION = `cat VERSION`

build:
	$(OCB) $(LIB_FILES)

install: build uninstall
	cp META_STD META
	ocamlfind install -patch-version $(VERSION) libgrew META $(INSTALL_FILES)
	rm -f META

install_dev: build uninstall_dev
	cp META_DEV META
	ocamlfind install -patch-version $(VERSION) libgrew_dev META $(INSTALL_FILES)
	rm -f META

uninstall:
	ocamlfind remove libgrew

uninstall_dev:
	ocamlfind remove libgrew_dev

doc:
	$(OCB) libgrew.docdir/index.html

.PHONY:	all clean build sanity

clean:
	$(OCB) -clean
