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
	zsh -c -i "code ."

.PHONY: build test code clean
