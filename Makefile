all: main

OPTS=-O2 -threaded -funbox-strict-fields

main:
	ghc ${OPTS} --make Main
force:
	ghc ${OPTS} --make -fforce-recomp Main

prof:
	ghc ${OPTS} --make -fforce-recomp -prof -auto-all -caf-all Main
	./Main +RTS -p

clean:
	rm *hi *o
