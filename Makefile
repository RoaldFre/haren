all: main

OPTS=-O2 -rtsopts -threaded -funbox-strict-fields -fexcess-precision -funfolding-use-threshold=1000 -optc-O3 -optc-ffast-math -opta-march=native -optc-march=native

main:
	ghc ${OPTS} --make Main
force:
	ghc ${OPTS} --make -fforce-recomp Main

prof:
	ghc ${OPTS} --make -fforce-recomp -prof -auto-all -caf-all Main
	./Main +RTS -p -sstderr
	less Main.prof

test:
	ghc ${OPTS} --make MathTest.hs
	./MathTest

clean:
	rm -f *hi *hc *o *prof
