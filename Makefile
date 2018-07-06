build:
	stack build

test:
	stack test

clean:
	stack clean

code:
	stack build hoogle intero stylish-haskell hlint; \
	zsh -c -i "code ."

.PHONY: build test code clean