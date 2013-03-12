###############################################################################
#                                                                             #
#                              OpenID-Client                                  #
#                                                                             #
#                                Make File                                    #
#                                                                             #
#                       Copyright (C) 2013-, AdaHeads K/S                     #
#                                                                             #
#  This is free software;  you can redistribute it  and/or modify it          #
#  under terms of the  GNU General Public License as published  by the        #
#  Free Software  Foundation;  either version 3,  or (at your option) any     #
#  later version.  This software is distributed in the hope  that it will     #
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty    #
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        #
#  General Public License for  more details.                                  #
#  You should have  received  a copy of the GNU General  Public  License      #
#  distributed  with  this  software;   see  file COPYING3.  If not, go       #
#  to http://www.gnu.org/licenses for a complete copy of the license.         #
#                                                                             #
###############################################################################

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
