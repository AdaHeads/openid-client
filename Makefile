ifeq ($(PROCESSORS),)
PROCESSORS=1
endif

all: style-check generated/configuration.ads
	mkdir -p obj
	gnatmake -j${PROCESSORS} -P openid

debug:
	BUILDTYPE=Debug gnatmake -j${PROCESSORS} -P openid

clean:
	gnatclean -P openid
	BUILDTYPE=Debug gnatclean -P openid
	rm -f generated/*

distclean: clean

style-check:
	@if egrep -l '	| $$' */*.ad? | egrep -v '^obj/b([~]|[_][_])'; then echo "Please remove tabs and end-of-line spaces from the source files listed above."; false; fi

fix-style:
	@egrep -l '	| $$' */*.ad? | egrep -v '^obj/b([~]|[_][_])' | xargs --no-run-if-empty perl -i -lpe 's/	/        /g; s/ +$$//'

generated/configuration.ads:
	mkdir -p generated
	@echo 'package Configuration is'                               > generated/configuration.ads
	@echo '   Host_Name : constant String := "${SERVER_NAME}";' >> generated/configuration.ads
	@echo 'end Configuration;'                                    >> generated/configuration.ads
