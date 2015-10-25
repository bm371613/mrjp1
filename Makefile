all:
	cabal configure
	cabal build

clean:
	rm -rf dist

.PHONY: all clean

