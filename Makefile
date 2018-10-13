SHELL:=$(shell echo $$SHELL)

build:
	stack build

test:
	stack test

random: build
	stack exec random-bits 1000000000 | pv -rbt >/dev/null

prof: build-profile
	stack exec -- random-bits 1000000 +RTS -p >/dev/null; \

build-profile:
	stack build --profile  

clean:
	stack clean

code:
	stack build hoogle intero stylish-haskell hlint; \
	$(SHELL) -c -i "code ."

haddock:
	stack haddock

.PHONY: build test random prof build-profile clean code haddock
