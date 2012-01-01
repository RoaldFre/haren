all: main

include Makefile.inc

main:
	ghc ${OPTS} ${BACKEND} --make Main

prof:
	ghc ${OPTS} $(BACKEND_C_NO_LTO) --make -prof -auto-all -caf-all Bench
	./Bench +RTS -H300m -p -hc -sstderr 2> Bench.stderr
	less Bench.stderr
	less Bench.prof
	hp2ps -e8in -c Bench.hp
	evince Bench.ps

demos:
	cd Demo; ${MAKE}

clean:
	rm -f *.hi{,-boot} *.o{,-boot} *.hc *.hp *.ps *.prof *.aux
	cd Material; rm -f *.hi{,-boot} *.o{,-boot} *.hc *.hp *.ps *.prof *.aux
	cd Geometry; rm -f *.hi{,-boot} *.o{,-boot} *.hc *.hp *.ps *.prof *.aux
	cd Demo; rm -f *.hi{,-boot} *.o{,-boot} *.hc *.hp *.ps *.prof *.aux
