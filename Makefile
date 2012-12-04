include .config

all: style-check generated/configuration.ads
	mkdir -p obj
	gnatmake $(GNATMAKE_ARGS) -P openid

clean:
	rm -f *.o *.ali b__*.ad?

distclean: clean

style-check:
	@if egrep -l '	| $$' */*.ad? | egrep -v '^obj/b([~]|[_][_])'; then echo "Please remove tabs and end-of-line spaces from the source files listed above."; false; fi

fix-style:
	@egrep -l '	| $$' */*.ad? | egrep -v '^obj/b([~]|[_][_])' | xargs --no-run-if-empty perl -i -lpe 's/	/        /g; s/ +$$//'

generated/configuration.ads:
	mkdir -p generated
	@echo 'package Configuration is'                               > generated/configuration.ads
	@echo '   Host_Name : constant String := "'${SERVER_NAME}'";' >> generated/configuration.ads                                                   
	@echo 'end Configuration;'                                    >> generated/configuration.ads
