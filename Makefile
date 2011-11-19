all: main

OPTS=-O2 -rtsopts -threaded -funbox-strict-fields -fexcess-precision -funfolding-use-threshold=32 -optc-O3 -optc-ffast-math 

main:
	ghc ${OPTS} --make Main
force:
	ghc ${OPTS} --make -fforce-recomp Main

prof:
	ghc ${OPTS} --make -fforce-recomp -prof -auto-all -caf-all Main
	./Main +RTS -p -sstderr
	less Main.prof

clean:
	rm -f *hi *hc *o *prof
