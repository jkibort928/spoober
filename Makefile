all: Main

Main: src/Main.hs
	ghc src/Main.hs -o src/bin/Main -odir src/build -hidir src/build

clean:
	rm src/bin/* src/build/*
