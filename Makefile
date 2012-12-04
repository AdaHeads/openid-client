include .config

all: style-check
	mkdir -p obj
	gnatmake $(GNATMAKE_ARGS) -P openid

clean:
	rm -f *.o *.ali b__*.ad?

distclean: clean

style-check:
	@if egrep -l '	| $$' *.ad? | egrep -v '^b([~]|[_][_])'; then echo "Please remove tabs and end-of-line spaces from the source files listed above."; false; fi

fix-style:
	@egrep -l '	| $$' *.ad? | egrep -v '^b([~]|[_][_])' | xargs --no-run-if-empty perl -i -lpe 's/	/        /g; s/ +$$//'

