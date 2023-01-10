PREFIX?=/usr/local
BINDIR=$(PREFIX)/bin
DATADIR=$(PREFIX)/share/libgrew/

OCB_FLAGS = -use-ocamlfind -use-menhir -I src
OCB = ocamlbuild $(OCB_FLAGS)

LIB_FILES = libgrew.cma libgrew.cmxa libgrew.a libgrew.cmi libgrew.cmx libgrew.cmxs grew_types.cmi grew_types.cmx
INSTALL_FILES = $(LIB_FILES:%=_build/src/%)

VERSION = `cat VERSION`

build: datadir
	$(OCB) $(LIB_FILES)

datadir:
	echo $(DATADIR) > DATADIR

install: build uninstall
	ocamlfind install -patch-version $(VERSION) libgrew META $(INSTALL_FILES)
	mkdir -p $(DATADIR)
	cp -rf grs/* $(DATADIR)

uninstall:
	ocamlfind remove libgrew
	rm -rf $(DATADIR)

doc:
	$(OCB) libgrew.docdir/index.html

.PHONY:	all clean build sanity

clean:
	$(OCB) -clean
