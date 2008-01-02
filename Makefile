all:
		ghc --make main.hs -O -o sds

clear:
		rm *hi *o sds

