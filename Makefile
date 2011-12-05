all: main

OPTS_C=-optc-O3 -opta-march=native -optc-march=native -optl-march-native -optc-ffast-math -optc-mfpmath=sse -optc-msse4

LLVM_FP=-optlo-enable-unsafe-fp-math -optlc-enable-unsafe-fp-math -optlo-enable-no-nans-fp-math -optlo-enable-fp-mad

OPTS_LLVM_BASIC=-fllvm -optlo-O3 -optlc-O3 ${LLVM_FP}
OPTS_LLVM_AGRESSIVE=-fllvm -optlo-basicaa -optlo-basiccg -optlo-count-aa -optlo-domfrontier -optlo-domtree -optlo-globalsmodref-aa -optlo-memdep -optlo-no-aa -optlo-postdomtree -optlo-codegenprepare -optlo-adce -optlo-functionattrs -optlo-block-placement -optlo-constmerge -optlo-constprop -optlo-die -optlo-dse -optlo-globaldce -optlo-globalopt -optlo-indvars -optlo-inline -optlo-ipconstprop -optlo-ipsccp -optlo-lcssa -optlo-loop-deletion -optlo-loop-unroll -optlo-loop-unswitch -optlo-loop-simplify -optlo-mem2reg -optlo-memcpyopt -optlo-scalarrepl -optlo-tailcallelim -optlo-prune-eh ${LLVM_FP}
#based on http://donsbot.wordpress.com/2010/03/01/evolving-faster-haskell-programs-now-with-llvm/

BACKEND_OPTS=${OPTS_LLVM_BASIC}

COMMON_OPTS=-rtsopts -threaded -funbox-strict-fields -fexcess-precision -fmax-worker-args=40

OPTS=${COMMON_OPTS} -funfolding-use-threshold=1000 -funfolding-creation-threshold=1000 -fspec-constr-count=64 -fspec-constr-threshold=1000 ${BACKEND_OPTS}

OPTS_QUICK=${COMMON_OPTS} -O2 -funfolding-use-threshold=200 -funfolding-creation-threshold=200

main:
	ghc ${OPTS_QUICK} --make Main
force:
	ghc ${OPTS_QUICK} --make -fforce-recomp Main
perf:
	ghc ${OPTS} --make Main
forceperf:
	ghc ${OPTS} --make -fforce-recomp Main

gcc:
	make forceperf BACKEND_OPTS=${OPTS_C}
agg:
	make forceperf BACKEND_OPTS=${OPTS_LLVM_AGRESSIVE}
basic:
	make forceperf BACKEND_OPTS=${OPTS_LLVM_BASIC}

forceprof:
	ghc ${OPTS} --make -fforce-recomp -prof -auto-all -caf-all Main
prof:
	ghc ${OPTS} --make -prof -auto-all -caf-all Main
	./Main +RTS -p -hc -sstderr 2> Main.stderr
	less Main.stderr
	less Main.prof
	hp2ps -e8in -c Main.hp
	evince Main.ps

test:
	ghc ${OPTS_QUICK} --make MathTest.hs
	./MathTest

clean:
	rm -f *hi *hc *o *prof
