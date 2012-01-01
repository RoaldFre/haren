all: teapot scenegraph materials

include ../Makefile.inc

teapot:
	ghc -i.. ${OPTS} ${BACKEND} --make Teapot
scenegraph:
	ghc -i.. ${OPTS} ${BACKEND} --make SceneGraph
materials:
	ghc -i.. ${OPTS} ${BACKEND} --make Materials

.DEFAULT_GOAL := all
