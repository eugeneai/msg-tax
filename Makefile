.PHONY: run build check

PYTARGET=app/tokenizer.py

run: build check
	python $(PYTARGET) | cabal run

build:
	cabal build

check: $(PYTARGET)
	python -m py_compile $^
