all: main

main:
	ghc -O --make Main

clean:
	rm *hi *o
