##################################################
# Makefile Settings
##################################################

SHELL=bash

.EXPORT_ALL_VARIABLES:

##################################################
# Makefile Variables: overrideable (via EnvironmentVariables)
##################################################

Develop?=1
Release?=0

#------------------------------------------------#

CurrentGitCommit:=$(shell git rev-parse --verify HEAD)
Timestamp:=$(shell date +%Y%m%d%H%M)

#------------------------------------------------#

Version=0.4.0
LongVersion=$(Version)-$(CurrentGitCommit)-$(Timestamp)

#------------------------------------------------#

CabalTargets ?=all
CabalTarget  ?=lib:spiros

ProjectFile ?=./cabal.project

#------------------------------------------------#

BaseDirectory ?=$(CURDIR)

NixDirectory       ?=./nix
Cabal2nixDirectory ?=$(NixDirectory)/packages/cabal2nix

#------------------------------------------------#

LibraryTarget    ?=lib:spiros

ExecutableTarget ?=exe:example-spiros

#------------------------------------------------#

NixFile    ?=./nix/default.nix
NixTarget  ?=cabalProjects

#NixTargets ?=
#NixTarget  ?=packages.spiros

#------------------------------------------------#

Cabal      ?=cabal
CabalBuild ?=cabal new-build

CabalStatic ?=--enable-executable-static

#------------------------------------------------#

Nix      ?=nix
NixBuild ?=nix-build --show-trace

Cabal2nix ?=cabal2nix

#------------------------------------------------#

MakeETags ?=hasktags --etags  --tags-absolute --follow-symlinks
MakeCTags ?=hasktags --ctags  --tags-absolute --follow-symlinks
#MakeGTags ?=hasktags --gtags  --tags-absolute --follow-symlinks

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

################################################## Miscellaneous

ETagsFile      ?=TAGS
CTagsFile      ?=tags

TagsDirectory ?=$(PackageDirectory)

#------------------------------------------------#

##################################################
# Makefile Variables: not overrideable
##################################################

CabalOptions=--project-file $(ProjectFile) --builddir $(BuildDirectory)

#------------------------------------------------#
# Makefile Targets: Standard --------------------#
#------------------------------------------------#

build:

	@echo "=================================================="
	@echo ""

	$(Cabal) new-build $(CabalOptions) $(CabalTargets)

	@echo ""
	@echo "=================================================="


.PHONY: build

#------------------------------------------------#

check:

	@echo "=================================================="
	@echo ""

	$(Cabal) new-test $(CabalOptions) $(CabalTargets)

	@echo ""
	@echo "=================================================="


.PHONY: check

#------------------------------------------------#

#TODO js 7.10 static 8.0 8.2 8.4 8.6

all: js 7.10 static 8.6

	@printf "\n%s\n" "========================================"

	@printf "\nSuccess! GHCs: \n\n"

	@printf "• « %s » (« %s »)\n" "ghc-7.10" 7.10.3
#	@printf "• « %s » (« %s »)\n" "ghc-8.0" 8.0.2
#	@printf "• « %s » (« %s »)\n" "ghc-8.2" 8.2.2
	@printf "• « %s » (« %s »)\n" "ghc-8.4" 8.4.4
	@printf "• « %s » (« %s »)\n" "ghc-8.6" 8.6.4

	@printf "\nSuccess! non-GHCs: \n\n"

	@printf "• « %s » (« %s », « %s »)\n" "ghcjs" ghcjs-8.6.0.1 ghc-8.6.2 # JavaScript

	@printf "\nSuccess! Musl: \n\n"

	@printf "• « %s » (« %s »)\n" "libmusl" ghc-8.4.3

	@printf "\n%s\n" "========================================"

.PHONY: all

#------------------------------------------------#

clean:
	rm -rf "dist/" dist-*/ ".stack-work"
	rm -f *.project.local .ghc.environment.*
	rm -rf "result/" result-*/
	find .  -type f  -name 'TAGS'  -exec rm -f \{} \+

.PHONY: clean

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

	ldd "./result-static/bin/example-spiros"

	@echo -e "\n========================================\n"

.PHONY: nix-static

#------------------------------------------------#

nix-dynamic: cabal2nix

	@echo -e "\n========================================\n"

	$(NixBuild)  -A "$(NixTarget)"  "$(NixDirectory)"  --out-link "./result-dynamic"  --arg static false

	@echo -e "\n========================================\n"

	ldd "./result-dynamic/bin/example-spiros"

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

	ldd "./result-static-integer-simple/bin/example-spiros"

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

LC_ALL=C.UTF-8

#------------------------------------------------#

example-spiros:

	@printf "\n%s\n" ========================================

	$(Cabal) new-build $(CabalOptions) "-fexamples" "exe:example-spiros"

	@printf "\n%s\n" ========================================

	$(Cabal) new-install $(CabalOptions) "-fexamples" "exe:example-spiros" "--overwrite-policy=always"

	@printf "\n%s\n" ========================================

	@example-spiros --help

	@printf "\n"

	@example-spiros --information -v

	@printf "\n"

	@example-spiros --version -v

	@printf "\n"

	@example-spiros --version

	@printf "\n%s\n" ========================================

.PHONY: example-spiros

#------------------------------------------------#

static-example:

	$(Cabal) new-run $(CabalOptions) -fstatic "exe:example-spiros" -- "--help"

	ldd `which example-spiros`

.PHONY: static-example

##################################################
# `cabal` wrapper targets
##################################################

#------------------------------------------------#

build-static:

	@echo -e "\n========================================\n"

#	$(CabalBuild)  --disable-shared  --enable-static  $(CabalTargets)

	cabal  -v2  new-build  --enable-static  --disable-shared  --disable-executable-dynamic  --project-file "./cabal/static.project"  "exe:example-spiros"   #   $(CabalTarget)

	@echo -e "\n========================================\n"

	tree "./dist-newstyle/build/x86_64-linux/ghc-8.6.3/spiros-0.3.1/"

	@echo -e "\n========================================\n"

	ldd "./dist-newstyle/build/x86_64-linux/ghc-8.6.3/spiros-0.3.1/x/example-spiros/build/example-spiros/example-spiros"

	@echo -e "\n========================================\n"

.PHONY: build-static

##################################################

#------------------------------------------------#

##################################################

#------------------------------------------------#

repl:
	cabal new-repl $(CabalTarget)

.PHONY: repl

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

##################################################
# Development ####################################
##################################################

# developing this package...

#------------------------------------------------#

tags: etags ctags

.PHONY: tags

#------------------------------------------------#

etags:

	@echo '=================================================='
	@echo

	$(MakeETags)  "$(TagsDirectory)"  --output "$(ETagsFile)"

	@echo '=================================================='
	@echo

	@cat $(ETagsFile)

	@echo
	@echo '=================================================='

.PHONY: etags

#------------------------------------------------#

ctags:

	@echo '=================================================='
	@echo

	$(MakeCTags)  "$(TagsDirectory)"  --output "$(CTagsFile)"

	@echo '=================================================='
	@echo

	@cat $(CTagsFile)

	@echo
	@echo '=================================================='

.PHONY: ctags

#------------------------------------------------#

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

build-ghcjs:

	cabal new-build --project-file="./cabal-ghcjs.project" all

.PHONY: build-ghcjs

#------------------------------------------------#

static-cabal: cabal-static.project

	$(Cabal) new-build -fstatic $(CabalStatic) --project-file="./cabal-static.project" exe:example-spiros

# --enable-executable-static
# -fstatic
# --project-file="./cabal-static.project"
# --extra-lib-dirs="/usr/lib/x86_64-linux-gnu"
# --extra-lib-dirs="/nix/store/blfgah5rv7h3qzl2gv6p6d8i2sxh0vgl-musl-1.1.21/lib /nix/store/pdyjwbhb77k17n6gl78a87a70gywr8dk-gmp-6.1.2/lib /nix/store/vz8iz7ws35aww6i8521z4964xp5akalh-libffi-3.2.1/lib"

.PHONY: static-cabal

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
# Nix ############################################
##################################################

#------------------------------------------------#

nix-repl:

	$(Nix) repl $(NixFile)

.PHONY: nix-repl

#------------------------------------------------#

nix-build:

	$(NixBuild) $(NixFile) -A $(NixTarget)

.PHONY: nix-build

#------------------------------------------------#

cabal-static.project:

	$(NixBuild) $(NixFile) -o "result-cabal-project-static" -A "cabalProjects.static"

	find -L "./result-cabal-project-static"

	cat "./result-cabal-project-static"

	ln --symbolic --force "./result-cabal-project-static" ./cabal-static.project

.PHONY: cabal-static.project

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
# Haskell Compilers -----------------------------#
#------------------------------------------------#

js:

#	$(Cabal) new-update  ghcjs-overlay --project-file "./cabal-ghcjs.project"

	$(Cabal) new-build   $(CabalTarget)  --project-file "./cabal-ghcjs.project"

.PHONY: js

#------------------------------------------------#

7.10:

	$(Cabal) new-build   $(CabalTarget)  --project-file "./cabal-ghc-7-10.project"

.PHONY: 7.10

#------------------------------------------------#

8.0:

	$(Cabal) new-build   $(CabalTarget)  --project-file "./cabal-ghc-8-00.project"

.PHONY: 8.0

#------------------------------------------------#

8.2:

	$(Cabal) new-build   $(CabalTarget)  --project-file "./cabal-ghc-8-02.project"

.PHONY: 8.2

#------------------------------------------------#

8.4:

	$(Cabal) new-build   $(CabalTarget)  --project-file "./cabal-ghc-8-04.project"

.PHONY: 8.4

#------------------------------------------------#

8.6:

	$(Cabal) new-build   $(CabalTarget)  --project-file "./cabal-ghc-8-06.project"

.PHONY: 8.6

#------------------------------------------------#

8.8:

	$(Cabal) new-build   $(CabalTarget)  --project-file "./cabal-ghc-8-08.project"

.PHONY: 8.8

#------------------------------------------------#
# Nix -------------------------------------------#
#------------------------------------------------#

static--example-spiros: static/cabal2nix/spiros.nix

	@printf "%s\n\n" ========================================

	NIX_PATH="nixpkgs=https://github.com/NixOS/nixpkgs/archive/2c07921cff84dfb0b9e0f6c2d10ee2bfee6a85ac.tar.gz" nix-build --out-link "./result-static" -A "example-spiros" "./static/default.nix"

	@printf "\n%s\n\n" ========================================

	@find -L "./result-static/" -type f

	@printf "\n%s\n\n" ========================================

	"./result-static/bin/example-spiros" --help

	@echo

	"./result-static/bin/example-spiros" --version

	@printf "\n%s\n\n" ========================================

	du -h "./result-static/bin/example-spiros"

	@printf "\n%s\n\n" ========================================

	! ldd "./result-static/bin/example-spiros"

	@printf "\n%s\n" ========================================

# « https://github.com/nh2/static-haskell-nix#readme »

.PHONY: static--example-spiros

#------------------------------------------------#

static/cabal2nix/spiros.nix:

	mkdir -p "./static/cabal2nix/"

	(cd "./static/cabal2nix/"  &&  $(Cabal2nix) "-fstatic" "-fexamples" "--no-check" "--compiler=ghc-8.4" "file://../../spiros" > "./spiros.nix")

#------------------------------------------------#
# Uploading -------------------------------------#
#------------------------------------------------#

upload-spiros: sdist

	$(Cabal) upload --username=sboo --password-command="pass hackage.haskell.org/user/sboo" ./dist-newstyle/sdist/spiros-$(Version).tar.gz

.PHONY: upload-spiros

#------------------------------------------------#

sdist: build

	$(Cabal) new-build $(CabalTargets)
	$(Cabal) new-sdist $(CabalTargets)

.PHONY: sdist

#------------------------------------------------#
# Release ---------------------------------------#
#------------------------------------------------#

release: release/spiros-$(Version).tar.gz release/bin/example-spiros

	@find ./release

.PHONY: release

#------------------------------------------------#

release/bin/example-spiros: static--example-spiros

	mkdir -p "./release/bin"
	cp "./result-static/bin/example-spiros" $@
	chmod u+rxw $@
	chmod g-rxw $@
	chmod o-rxw $@

#------------------------------------------------#

release/spiros-$(Version).tar.gz: sdist

	mkdir -p "./release"
	cp "./dist-newstyle/sdist/spiros-$(Version).tar.gz" $@

#------------------------------------------------#

static: static--example-spiros

.PHONY: static

#------------------------------------------------#

upload: release/spiros-$(Version).tar.gz

	$(Cabal) upload $^ --publish

.PHONY: upload

#------------------------------------------------#
# Notes -----------------------------------------#
#------------------------------------------------#

# Makefile Syntax:
#
# • « $@ » — the target        — (a.k.a. output file(s); a.k.a. the rule's left-hand-side).
# • « $^ » — all prerequisites — (a.k.a. inputs files, space-separated; a.k.a. the rule's right-hand-side).

#------------------------------------------------#
# EOF -------------------------------------------#
#------------------------------------------------#