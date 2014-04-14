all:
	./Makefile.gen;

clean:
	rm -f *.lex.sml
	rm -f *.grm.sml
	rm -f *.grm.desc
	rm -f *.grm.sig
	rm -f mlcompimage*
	rm -f *~
	rm -Rf CM
	rm -Rf .cm
	rm -f a.casm
	rm -f a.term
