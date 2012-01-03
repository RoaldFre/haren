all: teapot venus chess tree forrest scenegraph materials dielectric ifs ifs2 ifs2 haras

include ../Makefile.inc

#Haray
teapot:
	ghc -i.. ${OPTS} ${BACKEND} --make Teapot
venus:
	ghc -i.. ${OPTS} ${BACKEND} --make Venus
chess:
	ghc -i.. ${OPTS} ${BACKEND} --make Chess
tree:
	ghc -i.. ${OPTS} ${BACKEND} --make Tree
forrest:
	ghc -i.. ${OPTS} ${BACKEND} --make Forrest
scenegraph:
	ghc -i.. ${OPTS} ${BACKEND} --make SceneGraph
materials:
	ghc -i.. ${OPTS} ${BACKEND} --make Materials
dielectric:
	ghc -i.. ${OPTS} ${BACKEND} --make Dielectric
ifs:
	ghc -i.. ${OPTS} ${BACKEND} --make IFS
ifs2:
	ghc -i.. ${OPTS} ${BACKEND} --make IFS2
ifs3:
	ghc -i.. ${OPTS} ${BACKEND} --make IFS3

#Haras
haras:
	ghc -i.. ${OPTS} ${BACKEND} --make HarasDemo
