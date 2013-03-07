include makefile.setup

ifeq ($(PROCESSORS),)
PROCESSORS=1
endif

all:
	gnatmake -j${PROCESSORS} -P openid_client_build

debug:
	BUILDTYPE=Debug gnatmake -j${PROCESSORS} -P openid_client_build

clean:
	gnatclean -P openid_client_build
	BUILDTYPE=Debug gnatclean -P openid_client_build

distclean: clean
	rm -rf $(prefix)/openid_client
	rm -rf $(prefix)/include/openid_client
	rm -f $(prefix)/lib/gnat/openid_client.gpr

install: all
	mkdir -p $(prefix)/lib/gnat
	mkdir -p $(prefix)/openid_client
	mkdir -p $(prefix)/include/openid_client
	cp -pr library/* $(prefix)/openid_client
	cp -pr src/*.ad[sb] $(prefix)/include/openid_client
	cp -pr openid_client.gpr $(prefix)/lib/gnat
