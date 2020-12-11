all:
	ghc -i./src/parser:./src/ --make ./src/Compiler.hs -o ./latc

clean:
	-rm -f ./src/*.log ./src/*.aux ./src/*.hi ./src/*.o ./src/*.dvi
	-rm -f ./src/parser/*.log ./src/parser/*.aux ./src/parser/*.hi ./src/parser/*.o ./src/parser/*.dvi
