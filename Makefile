# Commands:

.PHONY: build init test clean doc deploy stage

build: 
	ghc --make -O -o classifier Main.hs

prof:
	ghc --make -prof -o classifier Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f classifier
	rm -f *.hi
	rm -f *.o

setup:
	mkdir -p  ~/.ghc/x86_64-linux-8.2.2
	ln -s ~sfogarty/.ghc/x86_64-linux-8.2.2/package.conf.d ~/.ghc/x86_64-linux-8.2.2/package.conf.d
