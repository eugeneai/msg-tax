.PHONY: run build check prep repl sent

PYTARGET=app/tokenizer.py
PKG=msg-tax

run: build check
	python $(PYTARGET) -d -i data/posts.json | cabal v2-run msg-tax -- -t

build:
	cabal build

check: $(PYTARGET)
	python -m py_compile $^

prep: check
	python $(PYTARGET)

repl: build
	cabal v2-repl exe:$(PKG)


sent: build
	# cabal v2-run msg-tax -- -s "Стали"
	cabal v2-run msg-tax -- -s "Мама мыла раму."
	#cabal v2-run msg-tax -- -s "Мама мыла"
