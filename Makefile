all:
	happy -gca ParRholang.y
	alex -g LexRholang.x
	ghc --make Main.hs -o rholint

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.bak DocRholang.txt rholang.dtd

distclean: clean
	-rm -f DocRholang.* LexRholang.* ParRholang.* LayoutRholang.* SkelRholang.* PrintRholang.* Main.* AbsRholang.* rholint ErrM.* SharedString.* ComposOp.* rholang.dtd Makefile*

