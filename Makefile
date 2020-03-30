PREFIX?=/usr/local
BINDIR=$(PREFIX)/bin
DATADIR=$(PREFIX)/share/libgrew/

OCB_FLAGS = -use-ocamlfind -use-menhir -I src
OCB = ocamlbuild $(OCB_FLAGS)

LIB_FILES = libgrew.cma libgrew.cmxa libgrew.a libgrew.cmi libgrew_types.cmi libgrew.cmx libgrew.cmxs
INSTALL_FILES = $(LIB_FILES:%=_build/src/%)

VERSION = `cat VERSION`

build: datadir
	$(OCB) $(LIB_FILES)

datadir:
	echo $(DATADIR) > DATADIR

install: build uninstall
	cp META_STD META
	ocamlfind install -patch-version $(VERSION) libgrew META $(INSTALL_FILES)
	rm -f META
	mkdir -p $(DATADIR)
	cp -rf grs/* $(DATADIR)

install_dev: build uninstall_dev
	cp META_DEV META
	ocamlfind install -patch-version $(VERSION) libgrew_dev META $(INSTALL_FILES)
	rm -f META
	mkdir -p $(DATADIR)
	cp -rf grs/* $(DATADIR)

uninstall:
	ocamlfind remove libgrew
	rm -rf $(DATADIR)

uninstall_dev:
	ocamlfind remove libgrew_dev
	rm -rf $(DATADIR)

doc:
	$(OCB) libgrew.docdir/index.html

.PHONY:	all clean build sanity

clean:
	$(OCB) -clean
