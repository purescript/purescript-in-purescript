deps:
	git clone git@github.com:purescript/purescript-arrows.git
	git clone git@github.com:purescript/purescript-quickcheck.git
	git clone git@github.com:purescript/purescript-readline.git
	git clone git@github.com:purescript/purescript-generics.git
	git clone git@github.com:purescript/purescript-maps.git
	git clone git@github.com:purescript/purescript-parsing.git
	git clone git@github.com:purescript/purescript-transformers.git

all: lib

lib:
	psc --make \
	  purescript-arrows/src/Control/Arrow.purs.hs \
	  purescript-generics/src/Data/Generics.purs.hs \
	  src/Language/PureScript/Names.purs.hs \
	  src/Language/PureScript/Values.purs.hs \
	  src/Language/PureScript/Types.purs.hs \
	  src/Language/PureScript/CodeGen/JS/AST.purs.hs

