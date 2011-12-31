all: main

export

BACKEND_C=-optc-O4 -optc-flto -optl-O4 -optl-flto -opta-march=native -optc-march=native -optl-march-native -optc-ffast-math -optc-mfpmath=sse -optc-msse4
BACKEND_C_NO_LTO=-optc-O3 -optl-O3 -opta-march=native -optc-march=native -optl-march-native -optc-ffast-math -optc-mfpmath=sse -optc-msse4

LLVM_FP=-optlo-enable-unsafe-fp-math -optlc-enable-unsafe-fp-math -optlo-enable-no-nans-fp-math -optlo-enable-fp-mad
BACKEND_LLVM_BASIC=-fllvm -optlo-O3 -optlc-O3 ${LLVM_FP}
BACKEND_LLVM_AGRESSIVE=-fllvm -optlo-basicaa -optlo-basiccg -optlo-count-aa -optlo-domfrontier -optlo-domtree -optlo-globalsmodref-aa -optlo-memdep -optlo-no-aa -optlo-postdomtree -optlo-codegenprepare -optlo-adce -optlo-functionattrs -optlo-block-placement -optlo-constmerge -optlo-constprop -optlo-die -optlo-dse -optlo-globaldce -optlo-globalopt -optlo-indvars -optlo-inline -optlo-ipconstprop -optlo-ipsccp -optlo-lcssa -optlo-loop-deletion -optlo-loop-unroll -optlo-loop-unswitch -optlo-loop-simplify -optlo-mem2reg -optlo-memcpyopt -optlo-scalarrepl -optlo-tailcallelim -optlo-prune-eh ${LLVM_FP}
#based on http://donsbot.wordpress.com/2010/03/01/evolving-faster-haskell-programs-now-with-llvm/

BACKEND=${BACKEND_LLVM_BASIC}

RTS_OPTS=-with-rtsopts="-N"
WARNINGS=-Wall -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-type-defaults
COMMON_OPTS=-rtsopts -threaded -funbox-strict-fields -fexcess-precision -fmax-worker-args=40 ${RTS_OPTS} ${WARNINGS}

OPTS=${COMMON_OPTS} -funfolding-use-threshold=1000 -funfolding-creation-threshold=1000 -fspec-constr-count=64 -fspec-constr-threshold=1000

OPTS_QUICK=${COMMON_OPTS} -O2 -funfolding-use-threshold=200 -funfolding-creation-threshold=200

main:
	ghc ${OPTS} ${BACKEND} --make Main
force:
	ghc ${OPTS} ${BACKEND} --make -fforce-recomp Main

gcc:
	make force BACKEND=${BACKEND_C}
agg:
	make force BACKEND=${BACKEND_LLVM_AGRESSIVE}
basic:
	make force BACKEND=${BACKEND_LLVM_BASIC}

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
	rm -f *.hi{,-boot} *.o{,-boot} *.hc *.prof *.aux
