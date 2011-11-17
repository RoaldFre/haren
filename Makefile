all: main

OPTS=-O2 -threaded -funbox-strict-fields -rtsopts

main:
	ghc ${OPTS} --make Main
force:
	ghc ${OPTS} --make -fforce-recomp Main

prof:
	ghc ${OPTS} --make -fforce-recomp -prof -auto-all -caf-all Main
	./Main +RTS -p
	less Main.prof

clean:
	rm *hi *o
