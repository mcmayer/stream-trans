build:
	stack build

test:
	stack test

random: build
	stack exec random-bits 10000000 | pv -rbt >/dev/null

clean:
	stack clean

code:
	stack build hoogle intero stylish-haskell hlint; \
	zsh -c -i "code ."

.PHONY: build test code clean