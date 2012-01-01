all: teapot scenegraph materials ifs haras

include ../Makefile.inc

#Haray
teapot:
	ghc -i.. ${OPTS} ${BACKEND} --make Teapot
scenegraph:
	ghc -i.. ${OPTS} ${BACKEND} --make SceneGraph
materials:
	ghc -i.. ${OPTS} ${BACKEND} --make Materials
ifs:
	ghc -i.. ${OPTS} ${BACKEND} --make IFS

#Haras
haras:
	ghc -i.. ${OPTS} ${BACKEND} --make HarasDemo
