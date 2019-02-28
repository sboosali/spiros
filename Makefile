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

LibraryTarget    ?=lib:spiros

ExecutableTarget ?=exe:example-sprios

#------------------------------------------------#

#NixTargets ?=
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

RootDirectory ?=$(CURDIR)
DefaultPackageDirectory ?=$(DefaultPackageName)

#------------------------------------------------#

ReleaseDirectory ?=./ignore/release
#                          ^ [Customize]

# ReleaseDirectory ?=./release
# ^ change `ReleaseDirectory` to `./release` during a release
# to actually commit it.

BuildDirectory ?=./dist-newstyle
BuildTarballDirectory ?=$(BuildDirectory)/sdist/

NixDirectory ?=./nix
ScriptDirectory ?=./scripts
DocumentDirectory ?=./docs

HaddockDirectory ?=$(ReleaseDirectory)/documentation
TarballDirectory ?=$(ReleaseDirectory)/tarballs
BinaryDirectory ?=$(ReleaseDirectory)/bin
InstallDirectory ?=$(ReleaseDirectory)/dist-newstyle/ #TODO

#------------------------------------------------#



#------------------------------------------------#

##################################################
# Makefile Variables: not overrideable
##################################################

CabalOptions=--project-file $(ProjectFile) --builddir $(BuildDirectory)

##################################################
# the `default` target
##################################################

default: build

.PHONY: default

##################################################
# `nix` wrapper targets
##################################################

nix: cabal2nix

	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link ./result

.PHONY: nix

#------------------------------------------------#

nix-spiros: cabal2nix-spiros

	$(NixBuild)  -A "packages.spiros"  "$(NixDirectory)"  --out-link ./result-spiros

.PHONY: nix-spiros

#------------------------------------------------#

nix-cabal-static:

	$(NixBuild)  -A "cabal.static"  "$(NixDirectory)"  --out-link "./result-cabal"

	@echo -e "\n========================================\n"

	tree "./result-cabal"

	@echo -e "\n========================================\n"

	cat "./result-cabal/*.project"

	@echo -e "\n========================================\n"

.PHONY: nix-cabal-static

#------------------------------------------------#

nix-static: cabal2nix-static

	@echo -e "\n========================================\n"

	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link "./result-static"  --arg static true

	@echo -e "\n========================================\n"

	ldd "./result-static/bin/example-sprios"

	@echo -e "\n========================================\n"

.PHONY: nix-static

#------------------------------------------------#

nix-dynamic: cabal2nix

	@echo -e "\n========================================\n"

	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link "./result-dynamic"  --arg static false

	@echo -e "\n========================================\n"

	ldd "./result-dynamic/bin/example-sprios"

	@echo -e "\n========================================\n"

.PHONY: nix-dynamic

#------------------------------------------------#

nix-musl: cabal2nix

	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link ./result-musl  --arg musl true

.PHONY: nix-musl

#------------------------------------------------#

nix-ghcjs: cabal2nix

	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link ./result-ghcjs  --argstr compiler ghcjs

.PHONY: nix-ghcjs

#------------------------------------------------#

nix-ghc-integer-simple: cabal2nix

	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link ./result-integer-simple  --arg integer-simple true

.PHONY: nix-ghc-integer-simple

#------------------------------------------------#

nix-ghc-8.6: cabal2nix

	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link ./result-ghc-8-6  --argstr compiler ghc863

.PHONY: nix-ghc-8.6

#------------------------------------------------#

nix-ghc-8.4: cabal2nix

	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link ./result-ghc-8-4  --argstr compiler ghc844

.PHONY: nix-ghc-8.4

#------------------------------------------------#

nix-ghc-8.2: cabal2nix

	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link ./result-ghc-8-2  --argstr compiler ghc822

.PHONY: nix-ghc-8.2

#------------------------------------------------#

# nix-ghc-8.0: cabal2nix
# 	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link ./result-ghc-8-0  --argstr compiler ghc802
# .PHONY: nix-ghc-8.0

#------------------------------------------------#

# nix-ghc-7.10: cabal2nix
# 	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link ./result-ghc-7-10  --argstr compiler ghc7103
# .PHONY: nix-ghc-7.10

#------------------------------------------------#

nix-static-integer-simple: cabal2nix

	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link ./result-static-integer-simple  --arg static true  --arg integer-simple true

	ldd "./result-static-integer-simple/bin/example-sprios"

.PHONY: nix-static-integer-simple

#------------------------------------------------#

nix-local:

	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link ./result-local  --arg nixpkgs "~/nixpkgs"

.PHONY: nix-local

##################################################
# « cabal2nix »...
##################################################

#------------------------------------------------#

cabal2nix: cabal2nix-spiros

.PHONY: cabal2nix

#------------------------------------------------#

cabal2nix-static:

	@echo "========================================"

	mkdir -p "$(Cabal2nixDirectory)"

	@echo "#  -*- mode: nix; buffer-read-only: t; -*-  " > "$(Cabal2nixDirectory)/spiros.nix"

	(cd "$(Cabal2nixDirectory)"  &&  $(Cabal2nix) "-fstatic" "file://$(BaseDirectory)/spiros" >> "./spiros.nix")

        # ^ Nix PathLiterals are relative to their source file (not the directory of an invoked command).

	@echo "========================================"

	@cat "$(Cabal2nixDirectory)/spiros.nix"

.PHONY: cabal2nix-static

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

#------------------------------------------------#

nix-static-example:

.PHONY: nix-static-example

#------------------------------------------------#

##################################################
# Executables ####################################
##################################################

#------------------------------------------------#

example:

	$(CabalBuild) $(CabalOptions) "exe:example-sprios"

	$(Cabal) new-exec $(CabalOptions) -- example-sprios --version

	$(Cabal) new-install $(CabalOptions) --overwrite-policy=always "exe:example-sprios"

	ldd `which example-sprios`

#	cabal new-exec --project-file ./cabal.project -- ldd `which example-sprios`

.PHONY: example

#------------------------------------------------#

static-example:

	$(Cabal) new-run $(CabalOptions) -fstatic "exe:example-sprios" -- "--help"

	ldd `which example-sprios`

.PHONY: static-example

##################################################
# `cabal` wrapper targets
##################################################

#------------------------------------------------#

build-static:

	@echo -e "\n========================================\n"

#	$(CabalBuild)  --disable-shared  --enable-static  $(CabalTargets)

	cabal  -v2  new-build  --enable-static  --disable-shared  --disable-executable-dynamic  --project-file "./cabal/static.project"  "exe:example-sprios"   #   "lib:spiros"

	@echo -e "\n========================================\n"

	tree "./dist-newstyle/build/x86_64-linux/ghc-8.6.3/spiros-0.3.1/"

	@echo -e "\n========================================\n"

	ldd "./dist-newstyle/build/x86_64-linux/ghc-8.6.3/spiros-0.3.1/x/example-sprios/build/example-sprios/example-sprios"

	@echo -e "\n========================================\n"

.PHONY: build-static

#------------------------------------------------#

check:

	$(CabalBuild) $(CabalOptions) -fno-code -O0 $(CabalTargets)

.PHONY: check

##################################################

#------------------------------------------------#

##################################################

#------------------------------------------------#

repl:
	cabal new-repl $(CabalTarget)

.PHONY: repl

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

# configure: configure-sboo

# .PHONY: configure

#------------------------------------------------#

configure-ghcjs:

	cabal new-configure --project-file="./cabal-ghcjs.project"

.PHONY: configure-ghcjs

#------------------------------------------------#

unconfigure:

	cabal new-configure

.PHONY: unconfigure

#------------------------------------------------#

configure:

	cabal --enable-nix new-configure --project-file="./cabal.project"

.PHONY: configure

#------------------------------------------------#

configure-ghc-8.4:

	cabal --enable-nix new-configure -w ghc-8.4.4

.PHONY: configure-ghc-8.4

#------------------------------------------------#

configure-ghc-8.6:

	cabal --enable-nix new-configure -w ghc-8.6.3

.PHONY: configure-ghc-8.6

#------------------------------------------------#

tags:
	cabal new-repl $(DefaultTarget) < <(echo -e $(TagsScript))

        # ^ NOTE:
        # * the « <(...) » is a Process Substitution, while
        # * the « ... < ... » is a Redirection.

.PHONY: tags

##################################################
# Installation ###################################
##################################################

# installing/configuring dependencies.

#------------------------------------------------#

apt-install:

	sudo apt install -y "libgmp-dev"
	sudo apt install -y "libffi-dev"
	sudo apt install -y "ghc"                # ghc-7.10.3

.PHONY: apt-install

#------------------------------------------------#

# nix-install:

# 	nix-env --install ""
# 	nix-env --install ""
# 	nix-env --install ""

# .PHONY: nix-install

#------------------------------------------------#

##################################################
# Building #######################################
##################################################

#------------------------------------------------#

build:

	@echo "=================================================="
	@echo ""

	$(Cabal) new-build $(CabalOptions) $(CabalTargets)

	@echo ""
	@echo "=================================================="


.PHONY: build

#------------------------------------------------#

build-ghcjs:

	cabal new-build --project-file="./cabal-ghcjs.project" all

.PHONY: build-ghcjs

#------------------------------------------------#


#------------------------------------------------#

##################################################
# Testing ########################################
##################################################

#------------------------------------------------#

test:

	@echo "=================================================="
	@echo ""

	$(Cabal) new-test $(CabalOptions) --enable-tests $(CabalTargets)

	@echo ""
	@echo "=================================================="

.PHONY: test

#------------------------------------------------#

bench:

	@echo "=================================================="
	@echo ""

	$(Cabal) new-bench $(CabalOptions) --enable-benchmarks $(CabalTargets)

	@echo ""
	@echo "=================================================="

.PHONY: bench

#------------------------------------------------#



#------------------------------------------------#

##################################################
# Documentation ##################################
##################################################

#------------------------------------------------#

docs:

	@echo "=================================================="
	@echo ""

	$(Cabal) new-haddock $(CabalOptions) --enable-documentation $(CabalTargets)

	@echo ""
	@echo "=================================================="
	@echo ""

	find $(BuildDirectory) -name "index.html" -print

	@echo ""
	@echo "=================================================="

.PHONY: docs

#------------------------------------------------#

##################################################
# Release ########################################
##################################################

#------------------------------------------------#

sdist:

	$(Cabal) new-build $(CabalTargets)
	$(Cabal) new-sdist $(CabalTargets)

.PHONY: sdist

#------------------------------------------------#

static:

	$(Cabal) new-build -fstatic --enable-executable-static --project-file="./cabal-static.project" exe:example-sprios

# --enable-executable-static
# -fstatic
# --project-file="./cabal-static.project"
# --extra-lib-dirs="/usr/lib/x86_64-linux-gnu"
# --extra-lib-dirs="/nix/store/blfgah5rv7h3qzl2gv6p6d8i2sxh0vgl-musl-1.1.21/lib /nix/store/pdyjwbhb77k17n6gl78a87a70gywr8dk-gmp-6.1.2/lib /nix/store/vz8iz7ws35aww6i8521z4964xp5akalh-libffi-3.2.1/lib"

.PHONY: static

#------------------------------------------------#

##################################################