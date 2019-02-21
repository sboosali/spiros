##################################################
# Makefile Settings
##################################################

SHELL=bash

.EXPORT_ALL_VARIABLES:

##################################################
# Makefile Variables: overrideable (via EnvironmentVariables)
##################################################

BaseDirectory ?=$(CURDIR)

NixDirectory       ?=./nix
Cabal2nixDirectory ?=$(NixDirectory)/packages/cabal2nix

#------------------------------------------------#

CabalTargets ?=all
CabalTarget  ?=lib:spiros

ProjectFile ?=./cabal.project

#------------------------------------------------#

NixTargets ?=*
NixTarget  ?=packages.spiros

#------------------------------------------------#

Cabal      ?=cabal
CabalBuild ?=cabal new-build

#------------------------------------------------#

Nix      ?=nix
NixBuild ?=nix-build --show-trace

Cabal2nix ?=cabal2nix

#------------------------------------------------#

Open		?=xdg-open
Pandoc		?=pandoc
Markdown	?=multimarkdown

#------------------------------------------------#

CheckCabal	?=$(Cabal) check
CheckTarball	?=tar -C /tmp -zxvf
CheckMarkdown	?=$(Markdown)
CheckJson	?=jsonlint
CheckBash	?=shellcheck
CheckNix	?=nix-instantiate

#------------------------------------------------#

##################################################
# Makefile Variables: not overrideable
##################################################

CabalOptions=--project-file $(ProjectFile)

##################################################
# the `default` target
##################################################

default: build

.PHONY: default

##################################################
# `nix` wrapper targets
##################################################

nix: # cabal2nix

	$(NixBuild)  -A "$(NixTargets)"  "$(NixDirectory)"

.PHONY: nix

#------------------------------------------------#

nix-spiros: cabal2nix-spiros

	$(NixBuild)  -A "packages.spiros"  "$(NixDirectory)"

.PHONY: nix-spiros

#------------------------------------------------#

cabal2nix: cabal2nix-spiros

.PHONY: cabal2nix

#------------------------------------------------#

cabal2nix-spiros:

	@echo "========================================"

	mkdir -p "$(Cabal2nixDirectory)"

	@echo "#  -*- mode: nix; buffer-read-only: t; -*-  " > "$(Cabal2nixDirectory)/spiros.nix"

	(cd "$(Cabal2nixDirectory)"  &&  $(Cabal2nix) "file://$(BaseDirectory)/spiros" >> "./spiros.nix")

        # ^ Nix PathLiterals are relative to their source file (not the directory of an invoked command).

	@echo "========================================"

	@cat "$(Cabal2nixDirectory)/spiros.nix"

.PHONY: cabal2nix-spiros

##################################################
# `cabal` wrapper targets
##################################################

build:

	$(CabalBuild) $(CabalOptions) $(CabalTargets)

.PHONY: build

#------------------------------------------------#

check:

	$(CabalBuild) $(CabalOptions) -fno-code -O0 $(CabalTargets)

.PHONY: check

#------------------------------------------------#

build-default:
	cabal new-build $(DefaultTarget)

.PHONY: build-default

#------------------------------------------------#

repl:
	cabal new-repl $(DefaultTarget)

.PHONY: repl

#------------------------------------------------#

test:
	cabal new-test $(DefaultTarget)

.PHONY: test

#------------------------------------------------#

clean:
	rm -rf "dist/" dist-*/ ".stack-work"
	rm -f *.project.local .ghc.environment.*
	find .  -type f  -name 'TAGS'  -exec rm -f \{} \+

.PHONY: clean

#------------------------------------------------#

cabal-compile:
	cabal new-build all

.PHONY: cabal-compile

#------------------------------------------------#

stack-compile:
	stack --nix build

.PHONY: stack-compile

#------------------------------------------------#

sdist: build
	cabal sdist

.PHONY: sdist

#------------------------------------------------#

configure: configure-8-4

.PHONY: configure

#------------------------------------------------#

configure-8-4:
	cabal new-configure --enable-nix -w ghc-8.4.3

.PHONY: configure-8-4

#------------------------------------------------#

configure-8-6:
	cabal new-configure --enable-nix -w ghc-8.6.1

.PHONY: configure-8-6

#------------------------------------------------#

tags:
	cabal new-repl $(DefaultTarget) < <(echo -e $(TagsScript))

        # ^ NOTE:
        # * the « <(...) » is a Process Substitution, while
        # * the « ... < ... » is a Redirection.

.PHONY: tags

##################################################