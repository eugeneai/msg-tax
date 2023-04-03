.PHONY: run build check prep repl

PYTARGET=app/tokenizer.py
PKG=msg-tax

run: build check
	python $(PYTARGET) | cabal run

build:
	cabal build

check: $(PYTARGET)
	python -m py_compile $^

prep: check
	python $(PYTARGET)

repl: build
	cabal v2-repl exe:$(PKG)
