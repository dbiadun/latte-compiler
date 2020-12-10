all:
	ghc -i./parser --make Compiler.hs -o compiler

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f ./parser/*.log ./parser/*.aux ./parser/*.hi ./parser/*.o ./parser/*.dvi
