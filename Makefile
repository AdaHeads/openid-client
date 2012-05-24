GNATMAKE_ARGS=-gnat05 -gnatyO -gnatE -gnato -gnatv -gnati1 -gnatf -gnatn -fstack-check -gnatyO -m

all:
	gnatmake $(GNATMAKE_ARGS) -P openid

clean:
	rm -f *.o *.ali

distclean: clean

