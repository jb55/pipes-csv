
all:
	cabal build

install:
	cabal install --haddock-html

clean:
	rm -rf dist
	cabal sandbox delete || exit 0
	cabal sandbox init
	cabal configure
