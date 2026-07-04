.PHONY: default
default: build

.PHONY: build
build: 
	ocamlbuild build

.PHONY: test
test:
	ocamlbuild test

.PHONY: bench
bench:
	ocamlbuild tests/bench.byte && _build/tests/bench.byte
	ocamlbuild tests/bench.native && _build/tests/bench.native

.PHONY: clean
clean:
	ocamlbuild -clean

.PHONY: doc
doc:
	ocamlbuild src/lmdb.docdir/index.html

NAME=lmdb
DOCDIR=.gh-pages

$(DOCDIR)/.git:
	mkdir -p $(DOCDIR)
	cd $(DOCDIR) && (\
		git clone -b gh-pages git@github.com:Drup/$(NAME).git . \
	)

gh-pages: $(DOCDIR)/.git doc
	git -C $(DOCDIR) pull
	cp -r _build/src/lmdb.docdir/* $(DOCDIR)/dev/
	git -C $(DOCDIR) add --all 
	git -C $(DOCDIR) commit -a -m "gh-page updates"
	git -C $(DOCDIR) push origin gh-pages

