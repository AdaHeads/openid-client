GNATMAKE_ARGS=-gnat05 -gnatyO -gnatE -gnato -gnatv -gnati1 -gnatf -gnatn -fstack-check -gnatyO -m
GNATMAKE_ARGS=-gnat05 -gnatyO -gnatE -gnato -gnatv -gnati1 -gnatf -gnatn -fstack-check -gnatyO -m -g

all: style-check
	gnatmake $(GNATMAKE_ARGS) -P openid

clean:
	rm -f *.o *.ali b__*.ad?

distclean: clean

style-check:
	@if egrep -l '	| $$' *.ad? | egrep -v '^b([~]|[_][_])'; then echo "Please remove tabs and end-of-line spaces from the source files listed above."; false; fi

fix-style:
	@egrep -l '	| $$' *.ad? | egrep -v '^b([~]|[_][_])' | xargs --no-run-if-empty perl -i -lpe 's/	/        /g; s/ +$$//'

