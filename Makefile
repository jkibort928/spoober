all: Main

Main: src/Main.hs bin src/build
	ghc src/Main.hs -o bin/Main -odir src/build -hidir src/build

bin:
	mkdir -p bin

src/build:
	mkdir -p src/build

clean:
	rm bin/* src/build/*
