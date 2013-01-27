scheme: parser.hs
	ghc --make -O2 -package parsec parser.hs -o scheme

clean:
	@rm -f *.hi *.o scheme
