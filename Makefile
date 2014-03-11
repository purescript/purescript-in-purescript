deps:
	git clone git@github.com:purescript/purescript-quickcheck.git
	git clone git@github.com:purescript/purescript-readline.git
	git clone git@github.com:purescript/purescript-generics.git
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

js/Data/Generics.js: js/Prelude.js
	mkdir -p js/Data
	mkdir -p externs/Data
	psc purescript-generics/src/Data/Generics.purs.hs \
	  externs/Prelude.e.purs.hs \
	  -o js/Data/Generics.js \
	  -e externs/Data/Generics.e.purs.hs \
	  --module Data.Generics --tco --magic-do --no-prelude

js/Language/PureScript/Names.js: js/Prelude.js js/Data/Generics.js
	mkdir -p js/Language/PureScript
	mkdir -p externs/Language/PureScript
	psc src/Language/PureScript/Names.purs.hs \
	  externs/Prelude.e.purs.hs \
	  externs/Data/Generics.e.purs.hs \
	  -o js/Language/PureScript/Names.js \
	  -e externs/Language/PureScript/Names.e.purs.hs \
	  --magic-do --tco --module Language.PureScript.Names --no-prelude

js/Language/PureScript/Values.js: js/Prelude.js js/Language/PureScript/Names.js
	mkdir -p js/Language/PureScript
	mkdir -p externs/Language/PureScript
	psc src/Language/PureScript/Values.purs.hs \
	  externs/Prelude.e.purs.hs \
	  externs/Language/PureScript/Names.e.purs.hs \
	  -o js/Language/PureScript/Values.js \
	  -e externs/Language/PureScript/Values.e.purs.hs \
	  --magic-do --tco --module Language.PureScript.Values --no-prelude

test:

