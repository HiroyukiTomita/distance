#
# Makefile for Ferret External Functions
#
# January 20 1998
# Jonathan Callahan
#
#  15-Nov-1999 Ansley Manke  
#  remove refs to ef_utility/*.o  (now in ferret executable)
#
#  ACM 2/2001  debug macros 
#  ACM 2/2002  change targets to all and extras; somehow standard
#               not working well w/ linux.
#
# include platform specific macro definitions
#
# include ../../site_specific.mk
BUILDTYPE = $(HOSTTYPE)
include ../ef_utility/platform_specific.mk.$(BUILDTYPE)
 
#
# Macros
#

.SUFFIXES: .so

#
# Rules
#

.F.so:
	$(F77) $(FFLAGS) -c  $<
	$(LD) $(LD_DYN_FLAGS) $(SYSLIBS) $*.o -o $*.so
 

#
# Targets
#

all:	ferret_cmn distance.so

debug:
	$(MAKE) "FFLAGS = $(FFLAGS) -g -Ddebug" "CFLAGS = $(CFLAGS) -g -Ddebug"  all


ferret_cmn:
	ln -s ../ef_utility/ferret_cmn ferret_cmn

install:
	cp *.so /home/tomita/EF/MY_EXT_FNC

clean:
	-rm -f *.o *.so core a.out temp.* ferret_cmn

#
# End of Makefile
#
