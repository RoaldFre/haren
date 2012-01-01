all: teapot scenegraph materials

include ../Makefile.inc

#Haray
teapot:
	ghc -i.. ${OPTS} ${BACKEND} --make Teapot
scenegraph:
	ghc -i.. ${OPTS} ${BACKEND} --make SceneGraph
materials:
	ghc -i.. ${OPTS} ${BACKEND} --make Materials


#Haras
haras:
	ghc -i.. ${OPTS} ${BACKEND} --make HarasDemo
