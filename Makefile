deps:
	git clone git@github.com:purescript/purescript-quickcheck.git
	git clone git@github.com:purescript/purescript-readline.git
	git clone git@github.com:purescript/purescript-maps.git
	git clone git@github.com:purescript/purescript-parsing.git
	git clone git@github.com:purescript/purescript-transformers.git

all: lib test

lib: js/Language/PureScript/Values.js

js/Prelude.js:
	mkdir -p js/Language/PureScript
	mkdir -p externs
	psc -o js/Prelude.js \
	  -e externs/Prelude.e.purs.hs \
	  --magic-do --tco

js/Language/PureScript/Values.js: js/Prelude.js
	mkdir -p js/Language/PureScript
	mkdir -p externs/Language/PureScript
	psc src/Language/PureScript/Values.purs.hs \
	  externs/Prelude.e.purs.hs \
	  -o js/Language/PureScript/Values.js \
	  -e externs/Language/PureScript/Values.e.purs.hs \
	  --magic-do --tco --module Language.PureScript.Values --no-prelude

test:

