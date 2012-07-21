# This will not compile with gfortran or g95 since they lack time and ctime
# functions. Compiles with ifort
FC = gfortran
# Makefile for DELAUNDO.
all: delaundo
#
FFLAGS= 	 -g
FFLAGS= 	 -O
# Platform dependencies:
MACH_DEP=
# The IBM doesn't know dtime. Make one out of mclock.
#MACH_DEP=$(SDIR)/dtime.o
#

COPT= $(FFLAGS)
LDFLAGS = $(FFLAGS)
#
.f.o:
	$(FC) -c $(FFLAGS) -o $@ $<
#
#
# directory for executable. 
EDIR=	.
#
#
# std directory for sources. 
SDIR=	.
SOBJS=  $(SDIR)/area.o \
	$(SDIR)/backgrd.o \
	$(SDIR)/boxwalk.o \
	$(SDIR)/bspcchk.o \
	$(SDIR)/bspcint.o \
	$(SDIR)/bstrint.o \
	$(SDIR)/buildnde.o \
	$(SDIR)/calcvvr.o \
	$(SDIR)/chartoint.o \
	$(SDIR)/chartoreal.o \
	$(SDIR)/chkbnd.o \
	$(SDIR)/cnctnde.o \
	$(SDIR)/coarseini.o \
	$(SDIR)/cosangl.o \
	$(SDIR)/debug_funs.o \
	$(SDIR)/defctrl.o \
	$(SDIR)/delaundo.o \
	$(SDIR)/dlnyrstr.o \
	$(SDIR)/dpwntpl1.o \
	$(SDIR)/fixiarea.o \
	$(SDIR)/flgvvrcn.o \
	$(SDIR)/flgout.o \
	$(SDIR)/fndiarea.o \
	$(SDIR)/fndnde.o \
	$(SDIR)/fndvscnd.o \
	$(SDIR)/functn.o \
	$(SDIR)/getctrl.o \
	$(SDIR)/helpme.o \
	$(SDIR)/hsort.o \
	$(SDIR)/huldpl.o \
	$(SDIR)/iffywalk.o \
	$(SDIR)/illicit.o \
	$(SDIR)/infoctrl.o \
	$(SDIR)/intcld.o \
	$(SDIR)/introvis.o \
	$(SDIR)/iwalk.o \
	$(SDIR)/ivvalk.o \
	$(SDIR)/ivvnxt.o \
	$(SDIR)/lplc.o \
	$(SDIR)/ndemrg.o \
	$(SDIR)/ndex.o \
	$(SDIR)/nghbnde.o \
	$(SDIR)/path.o \
	$(SDIR)/plausctrl.o \
	$(SDIR)/pstprc.o \
	$(SDIR)/pstprcs.o \
	$(SDIR)/pullstack.o \
	$(SDIR)/pushstack.o \
	$(SDIR)/pwntpl.o \
	$(SDIR)/quaddpl.o \
	$(SDIR)/readchar.o \
	$(SDIR)/readctrl.o \
	$(SDIR)/readfip.o \
	$(SDIR)/readinteger.o \
	$(SDIR)/readkey.o \
	$(SDIR)/readpts.o \
	$(SDIR)/readreal.o \
	$(SDIR)/readstring.o \
	$(SDIR)/rmbkg.o \
	$(SDIR)/scanbnd.o \
	$(SDIR)/setup.o \
	$(SDIR)/spcolchk.o \
	$(SDIR)/supcalc.o \
	$(SDIR)/supntpl.o \
	$(SDIR)/swapdiag.o \
	$(SDIR)/swaps.o \
	$(SDIR)/tricav.o \
	$(SDIR)/tridpl.o \
	$(SDIR)/visbox.o \
	$(SDIR)/xample.o 
SINC=	$(SDIR)/bkgrd.cbl \
	$(SDIR)/nodes.cbl \
	$(SDIR)/vvrtc.cbl
$(SOBJS): $(SINC)
#
#
delaundo:$(SOBJS) $(MACH_DEP)
	$(FC) $(COPT)  -o $(SDIR)/delaundo   $(SOBJS) $(MACH_DEP)
#
clean:
	/bin/rm -f $(SDIR)/*.bk $(SDIR)/*JNL  $(SDIR)/*~ $(SDIR)/*.log
cleano: 
	/bin/rm -f $(SOBJS) 
small: clean cleano
	gzip *


