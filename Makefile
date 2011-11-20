all: main

OPTS_C=-optc-O3 -opta-march=native -optc-march=native -optl-march-native -optc-ffast-math -optc-mfpmath=sse

LLVM_FP=-optlo-enable-unsafe-fp-math -optlc-enable-unsafe-fp-math -optlo-enable-no-nans-fp-math -optlo-enable-fp-mad

OPTS_LLVM_BASIC=-fllvm -optlo-O3 -optlc-O3 ${LLVM_FP}
OPTS_LLVM_AGRESSIVE=-fllvm -optlo-basicaa -optlo-basiccg -optlo-count-aa -optlo-domfrontier -optlo-domtree -optlo-globalsmodref-aa -optlo-memdep -optlo-no-aa -optlo-postdomtree -optlo-codegenprepare -optlo-adce -optlo-functionattrs -optlo-block-placement -optlo-constmerge -optlo-constprop -optlo-die -optlo-dse -optlo-globaldce -optlo-globalopt -optlo-indvars -optlo-inline -optlo-ipconstprop -optlo-ipsccp -optlo-lcssa -optlo-loop-deletion -optlo-loop-unroll -optlo-loop-unswitch -optlo-loop-simplify -optlo-mem2reg -optlo-memcpyopt -optlo-scalarrepl -optlo-tailcallelim -optlo-prune-eh ${LLVM_FP}
#based on http://donsbot.wordpress.com/2010/03/01/evolving-faster-haskell-programs-now-with-llvm/

BACKEND_OPTS=${OPTS_LLVM_AGRESSIVE}

OPTS=-Odph -O2 -rtsopts -threaded -funbox-strict-fields -fexcess-precision -funfolding-use-threshold=1000 -funfolding-creation-threshold=1000 ${BACKEND_OPTS}

main:
	ghc ${OPTS} --make Main
force:
	ghc ${OPTS} --make -fforce-recomp Main

gcc:
	make force BACKEND_OPTS=${OPTS_C}
agg:
	make force BACKEND_OPTS=${OPTS_LLVM_AGRESSIVE}
basic:
	make force BACKEND_OPTS=${OPTS_LLVM_BASIC}

prof:
	ghc ${OPTS} --make -fforce-recomp -prof -auto-all -caf-all Main
	./Main +RTS -p -sstderr
	less Main.prof

test:
	ghc ${OPTS} --make MathTest.hs
	./MathTest

clean:
	rm -f *hi *hc *o *prof
