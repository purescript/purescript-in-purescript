deps:
	git clone git@github.com:purescript/purescript-arrows.git       src-lib/arrows
	git clone git@github.com:purescript/purescript-quickcheck.git   src-lib/quickcheck
	git clone git@github.com:purescript/purescript-readline.git     src-lib/readline
	git clone git@github.com:purescript/purescript-generics.git     src-lib/generics
	git clone git@github.com:purescript/purescript-maps.git         src-lib/maps
	git clone git@github.com:purescript/purescript-parsing.git      src-lib/parsing
	git clone git@github.com:purescript/purescript-transformers.git src-lib/transformers

all: lib

lib:
	psc --make \
	  src-lib/arrows/src/Control/Arrow.purs.hs \
	  src-lib/generics/src/Data/Generics.purs.hs \
	  src-lib/transformers/src/Control/Monad/Error.purs \
	  src-lib/transformers/src/Control/Monad/Error/Class.purs \
	  src-lib/transformers/src/Control/Monad/Error/Trans.purs \
	  src-lib/transformers/src/Control/Monad/Identity.purs \
	  src-lib/transformers/src/Control/Monad/Maybe/Trans.purs \
	  src-lib/transformers/src/Control/Monad/Reader.purs \
	  src-lib/transformers/src/Control/Monad/Reader/Class.purs \
	  src-lib/transformers/src/Control/Monad/Reader/Trans.purs \
	  src-lib/transformers/src/Control/Monad/State.purs \
	  src-lib/transformers/src/Control/Monad/State/Class.purs \
	  src-lib/transformers/src/Control/Monad/State/Trans.purs \
	  src-lib/transformers/src/Control/Monad/Trans.purs \
	  src-lib/transformers/src/Control/Monad/Writer.purs \
	  src-lib/transformers/src/Control/Monad/Writer/Class.purs \
	  src-lib/transformers/src/Control/Monad/Writer/Trans.purs \
	  src/Language/PureScript/Names.purs.hs \
	  src/Language/PureScript/Values.purs.hs \
	  src/Language/PureScript/Types.purs.hs \
	  src/Language/PureScript/CodeGen/JS/AST.purs.hs

