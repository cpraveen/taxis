# make ipol
all: ipol
#
#
VERSION = 3.2
 CFLAGS =  -g
#CFLAGS = -O 
MACH_DEP= $(HOSTTYPE)
BIN_DIR= $(HOME)/bin/$(MACH_DEP)
#
#.f.o:
#	$(FC) -c $(CFLAGS) -o $@ $<
#
# files from delaundo/std
CDIR = .
ADIR = ../../delaundo/std
AOBJS=	$(CDIR)/chartoint.o \
	$(CDIR)/helpme.o \
	$(CDIR)/path.o \
	$(CDIR)/readfip.o \
	$(CDIR)/readinteger.o \
	$(CDIR)/readkey.o
$(CDIR)/%.o : $(ADIR)/%.f
	$(FC) -c $(CFLAGS)  -o $@ $<
#	$(FC) -c $(CFLAGS) -o ./$(@F) $(ADIR)/$(@F:.o=.f)
#
# files around ipol
CDIR = .
COBJS=	$(CDIR)/addstation.o \
	$(CDIR)/blend.o \
	$(CDIR)/calcmap.o \
	$(CDIR)/curves.o \
	$(CDIR)/domap.o \
	$(CDIR)/ipol.o \
	$(CDIR)/defaults.o
$(CDIR)/%.o : $(CDIR)/%.f
	$(FC) -c $(CFLAGS) -o ./$(@F) $(CDIR)/$(@F:.o=.f)

#
ipol:	$(AOBJS) $(COBJS)
	$(FC) -o ipol $(CFLAGS) $(AOBJS) $(COBJS)
#
install: ipol
	 strip ipol ;\
	 /bin/mv -f ipol $(BIN_DIR)/ipol-$(VERSION) ;\
	 /bin/rm -f $(BIN_DIR)/ipol ;\
	 ln -s $(BIN_DIR)/ipol-$(VERSION) $(BIN_DIR)/ipol
#
#
clean:
	/bin/rm $(CDIR)/*~
cleano:
	/bin/rm $(AOBJS) $(COBJS)



