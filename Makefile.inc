BACKEND_C=-optc-O4 -optc-flto -optl-O4 -optl-flto -opta-march=native -optc-march=native -optl-march-native -optc-ffast-math -optc-mfpmath=sse -optc-msse4
BACKEND_C_NO_LTO=-optc-O3 -optl-O3 -opta-march=native -optc-march=native -optl-march-native -optc-ffast-math -optc-mfpmath=sse -optc-msse4

#WARNING: THE LLVM BACKEND IS PRONE TO CREATING SEGFAULTS ON MY SYSTEM!
#LLVM_FP=-optlo-enable-unsafe-fp-math -optlc-enable-unsafe-fp-math -optlo-enable-no-nans-fp-math -optlo-enable-fp-mad
LLVM_FP=
BACKEND_LLVM_BASIC=-fllvm -optlo-O2 -optlc-O2 ${LLVM_FP}
BACKEND_LLVM_AGGRESSIVE=-fllvm -optlo-basicaa -optlo-basiccg -optlo-count-aa -optlo-domfrontier -optlo-domtree -optlo-globalsmodref-aa -optlo-memdep -optlo-no-aa -optlo-postdomtree -optlo-codegenprepare -optlo-adce -optlo-functionattrs -optlo-block-placement -optlo-constmerge -optlo-constprop -optlo-die -optlo-dse -optlo-globaldce -optlo-globalopt -optlo-indvars -optlo-inline -optlo-ipconstprop -optlo-ipsccp -optlo-lcssa -optlo-loop-deletion -optlo-loop-unroll -optlo-loop-unswitch -optlo-loop-simplify -optlo-mem2reg -optlo-memcpyopt -optlo-scalarrepl -optlo-tailcallelim -optlo-prune-eh ${LLVM_FP}
#based on http://donsbot.wordpress.com/2010/03/01/evolving-faster-haskell-programs-now-with-llvm/

BACKEND=${BACKEND_C}

RTS_OPTS=-with-rtsopts="-N -K100m"
WARNINGS=-Wall -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-type-defaults
FORCE=
COMMON_OPTS=-rtsopts -threaded -funbox-strict-fields -fexcess-precision -fmax-worker-args=40 ${RTS_OPTS} ${WARNINGS} ${FORCE}

OPTS=${COMMON_OPTS} -O2 -funfolding-use-threshold=1000 -funfolding-creation-threshold=1000 -fspec-constr-count=64 -fspec-constr-threshold=1000

OPTS_QUICK=${COMMON_OPTS} -O1 -funfolding-use-threshold=200 -funfolding-creation-threshold=200


#conditionals (is there a better way/can we get flags?)
#use 'make target GCC= F=' to force a remake of target with the gcc backend
F=no
GCC=no
LLVM=no
Q=no
ifneq ($F, no)
	FORCE='-fforce-recomp'
endif
ifneq ($(GCC), no)
	BACKEND=$(BACKEND_C)
endif
ifneq ($(LLVM), no)
	BACKEND=$(BACKEND_LLVM_BASIC)
endif
ifneq ($Q, no)
	OPTS=$(OPTS_QUICK)
endif



#these work on the default target:

force:
	@${MAKE} FORCE='-fforce-recomp'
gcc:
	@${MAKE} BACKEND='${BACKEND_C}'
agg:
	@${MAKE} BACKEND='${BACKEND_LLVM_AGGRESSIVE}'
basic:
	@${MAKE} BACKEND='${BACKEND_LLVM_BASIC}'

forcegcc:
	@${MAKE} force BACKEND='${BACKEND_C}'
forceagg:
	@${MAKE} force BACKEND='${BACKEND_LLVM_AGGRESSIVE}'
forcebasic:
	@${MAKE} force BACKEND='${BACKEND_LLVM_BASIC}'

# vim: ft=make
