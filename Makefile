GNATMAKE_ARGS=-s -gnat05 -gnatyO -gnatE -gnato -gnatv -gnati1 -gnatf -gnatn -fstack-check -gnatyO -m

all:
	gnatmake $(GNATMAKE_ARGS) -Popenid
