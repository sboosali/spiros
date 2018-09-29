##################################################
# Makefile Variables
##################################################

BaseDirectory=$(CURDIR)

DefaultTarget=spiros

##################################################
# the `default` target
##################################################

default: build

.PHONY: default

##################################################
# `cabal` wrapper targets
##################################################

check:
	cabal new-build -fno-code -O0 all

.PHONY: check

##################################################

build: build-default

.PHONY: build

##################################################

build-default:
	cabal new-build $(DefaultTarget)

.PHONY: build-default

##################################################

repl:
	cabal new-repl $(DefaultTarget)

.PHONY: repl

##################################################

test:
	cabal new-test $(DefaultTarget)

.PHONY: test

##################################################

clean:
	rm -rf "dist/" dist-*/ ".stack-work"
	rm -f *.project.local .ghc.environment.*

.PHONY: clean

##################################################

cabal-compile:
	cabal new-build all

.PHONY: cabal-compile

##################################################

stack-compile:
	stack --nix build

.PHONY: stack-compile

##################################################

sdist: build
	cabal sdist

.PHONY: sdist

##################################################

configure: configure-8-4

.PHONY: configure

##################################################

configure-8-4:
	cabal new-configure --enable-nix -w ghc-8.4.3

.PHONY: configure-8-4

##################################################

configure-8-6:
	cabal new-configure --enable-nix -w ghc-8.6.1

.PHONY: configure-8-6

##################################################