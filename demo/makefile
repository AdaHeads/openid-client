ifeq ($(PROCESSORS),)
PROCESSORS=1
endif

ifeq ($(SERVER_NAME),)
SERVER_NAME=localhost
endif

all: generated/configuration.ads
	gnatmake -j${PROCESSORS} -P openid_demo

debug:
	BUILDTYPE=Debug gnatmake -j${PROCESSORS} -P openid_demo

clean:
	gnatclean -P openid_demo
	BUILDTYPE=Debug gnatclean -P openid_demo
	@rm -rf build_debug/configuration*
	@rm -rf build_production/configuration.*
	@rm -r generated/*

generated/configuration.ads:
	mkdir -p generated
	@echo 'package Configuration is' > generated/configuration.ads
	@echo '   Host_Name : constant String := "${SERVER_NAME}";' >> generated/configuration.ads
	@echo 'end Configuration;' >> generated/configuration.ads
