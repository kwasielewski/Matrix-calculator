BUILD_PATH=./_build/install/default/bin/matrix 

all:
	dune build

install: all
	ln -s ${BUILD_PATH} ./matrix
