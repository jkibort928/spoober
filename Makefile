all: Main

Main: src/Main.hs bin build
	ghc src/Main.hs -o bin/Main -odir build -hidir build

bin:
	mkdir -p bin

build:
	mkdir -p build

clean:
	rm bin/* build/*
