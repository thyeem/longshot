bin := longshot
ghc := --with-compiler=ghc-8.10.7
opts := "--ghc-options=-Wall -threaded -rtsopts -with-rtsopts=-N -feager-blackholing"
release := $(ghc) $(opts) "--ghc-options=-O2 -fexpose-all-unfoldings -dynamic"
test-opts := $(release) --test-show-details=direct
path := ${HOME}/.local/bin

.PHONY: build
build:
	cabal build $(release)
	cp -f $(shell cabal list-bin $(bin)) app

.PHONY: install
install:
	make build
	/usr/bin/strip app/$(bin)
	mkdir -p $(path)
	cp -f app/$(bin) $(path)

clean:
	git clean -xdf
	cabal clean

.PHONY: test
test:
	cabal test test $(test-opts) --test-option=--match --test-option="$(match)"
