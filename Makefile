all: main

OPTS=-O2 -threaded

main:
	ghc ${OPTS} --make Main
force:
	ghc ${OPTS} --make -fforce-recomp Main

prof:
	ghc ${OPTS} --make -fforce-recomp -prof -auto-all Main
	./Main +RTS -p

clean:
	rm *hi *o
