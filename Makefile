# Copyright (c) 2015 Taylor Fausak <taylor@fausak.me>
#
# See COPYING.Haskeleton.md for the licence covering this file.

.PHONY: all bench build clean configure haddock hpc install repl run test

all: install configure build haddock test hpc bench

bench:
	cabal bench --jobs

build:
	cabal build --jobs

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	cabal configure --enable-benchmarks --enable-tests

haddock:
	cabal haddock --hyperlink-source
	# dist/doc/html/phec/index.html

hpc:
	hpc markup --destdir=tmp dist/hpc/tix/tests/tests.tix
	# tmp/hpc_index.html

install:
	cabal sandbox init
	cabal install --enable-benchmarks --enable-tests --jobs --only-dependencies --reorder-goals

repl:
	cabal repl lib:phec

run:
	cabal run --jobs phec

test:
	cabal test --jobs
	cabal check
