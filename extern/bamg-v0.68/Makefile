HOSTTYPE=linux
#  definition of the suffixe for debug object and optimized 
OG =_g.o
OO =_o.o
# -----------------

all: bamg cvmsh2 $(FLAGFILE) drawbdmesh

INSTALLDIR=/usr/local/bin
FLAGFILE=FLAG.$(HOSTTYPE)

include $(FLAGFILE)

OBJ =  Mesh2$(OO)   MeshRead$(OO)   MeshWrite$(OO)   MeshGeom$(OO)    MeshQuad$(OO) \
       QuadTree$(OO)  Meshio$(OO) Metric$(OO) SetOfE4$(OO) 

GOBJ = Mesh2$(OG)     MeshRead$(OG)   MeshWrite$(OG)   MeshDraw$(OG)       MeshGeom$(OG)    MeshQuad$(OG) \
	  QuadTree$(OG)  Xrgraph$(OG) Meshio$(OG) Metric$(OG) SetOfE4$(OG)   \
	 CheckPtr$(OG)

.SUFFIXES: $(OO) .cpp $(OG)



graphic: drawbdmesh  bamg-g
world: all graphic cvmsh2
install: all
	chmod a+x bamg cvmsh2 
	cp bamg  cvmsh2 $(INSTALLDIR)	
install-world: install graphic 
	chmod a+x bamg-g  drawbdmesh
	cp  bamg-g  drawbdmesh  $(INSTALLDIR)	
install-inria:
	for i in rascasse apron  kilka vive squale; do echo sur $$i;remsh $$i "cd bamg;$(MAKE) install-world";done
install-www: 
	./MakeTar ; \
        vvv="`grep '@(#)INRIA' bamg.cpp | awk '{print $$(NF-1)}'`" ; \
	ddd="`grep '@(#)INRIA' bamg.cpp | awk -F, '{print $$2}'`"; \
	tarfilegz=/tmp/bamg-$$vvv.tar.gz; \
	cp README /net/rascasse/ftp_gamma/bamg; \
	cp README /net/tetrodon/www_gamma/gamma/cdrom/ftp/bamg; \
	(cd /net/rascasse/ftp_gamma/bamg;  cp -f $$tarfilegz .; \
         ln -f -s `basename $$tarfilegz`  bamg.tar.gz);  \
	cp -f $$tarfilegz  /net/tetrodon/www_gamma/gamma/cdrom/ftp/bamg/bamg.tar.gz; \
	sed "s/XXXX/$$vvv/" <www/eng.htm  >/net/tetrodon/www_gamma/gamma/cdrom/www/bamg/eng.htm; \
	sed "s/XXXX/$$vvv/" <www/fra.htm  >/net/tetrodon/www_gamma/gamma/cdrom/www/bamg/fra.htm; \
	sed -e "s/XXXX/$$vvv/" -e "s;YYYY;$$ddd;" <www/copyright.htm  >/net/tetrodon/www_gamma/gamma/cdrom/www/bamg/copyright.htm 
bamg: $(OBJ)  bamg$(OO) 
	$(CXX)  $(CXXLINKFLAG) $(O)  -o bamg  $(OBJ) bamg$(OO) $(STDLIB)
cvmsh2: $(OBJ)  cvmsh2$(OO) 
	$(CXX)  $(CXXLINKFLAG) $(O)  -o cvmsh2  $(OBJ) cvmsh2$(OO) $(STDLIB)
drawbdmesh: $(GOBJ)   drawbdmesh$(OG)
	$(CXX)  $(CXXLINKFLAG) $(G)   -o drawbdmesh drawbdmesh$(OG)  $(GOBJ)  $(X11LIB) $(STDLIB)

bamg-g: $(GOBJ)  bamg$(OG) 
	$(CXX)  $(CXXLINKFLAG) $(G)  -o bamg-g bamg$(OG) $(GOBJ)  $(X11LIB) $(STDLIB)
test: all
	cd examples/square;bamg=../../bamg;export bamg;./dotest
	cd examples/square;bamg=../../bamg;export bamg;./dotest -q
	cd examples/square;bamg=../../bamg;export bamg;./doadapt
	cd examples/test;perl ./dotest.pl 
	echo " -- end of test "
test-g: graphic
	cd examples/square;bamg=../../bamg-g;export bamg;. ./dotest 
	cd examples/square;bamg=../../bamg-g;export bamg;. ./dotest -q
	cd examples/square;bamg=../../bamg-g;export bamg;. ./doadapt
	cd examples/test;perl ./dotest1.pl 
	cd examples/quadloop;perl ./dotest.pl 
	echo " -- end of test-g "

#amfmt2mesh:amfmt2mesh.f
#	$(F77) $(F77FLAGS) amfmt2mesh.f -o $@
clean:cleang
	rm -f *~ *$(OO) *$(OG) bamg 
	rm -rf ptrepository
cleang:
	rm -f *$(OG) bamg-g drawbdmesh


.cpp$(OO):
	$(CXX)  $(CXXFLAGS) -c $<
	mv  $*.o $@
.cpp$(OG):
	$(CXX)  $(G) $(CXXGRAPHIC)  -c $<
	mv  $*.o $@
## ajoute F Hecht ---
$(FLAGFILE):
	if [ ! -f $(FLAGFILE) ] ; then                                     \
	echo "you have no HOSTTYPE variable define";                       \
	echo the knows  HOSTTYPE are : `echo FLAG.* | sed 's/FLAG.//g'` ;   \
	 echo Create the file  FLAG.$$HOSTTYPE; exit 1;                \
	fi

###
Mesh2$(OO): Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h SetOfE4.h
MeshDraw$(OO): Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h SetOfE4.h
MeshGeom$(OO): Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h SetOfE4.h
MeshQuad$(OO): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h \
	SetOfE4.h
MeshRead$(OO): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h \
	SetOfE4.h
MeshWrite$(OO): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h \
	SetOfE4.h
Meshio$(OO): Meshio.h
Metric$(OO): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h
QuadTree$(OO): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h
R2$(OO): R2.h
SetOfE4$(OO): meshtype.h SetOfE4.h
Xrgraph$(OO): rgraph.h
bamg$(OO): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h
cvmsh2$(OO): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h
drawbdmesh$(OO): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h
macrgraf$(OO): rgraph.h
# ----
Mesh2$(OG): Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h SetOfE4.h
MeshDraw$(OG): Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h SetOfE4.h
MeshGeom$(OG): Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h SetOfE4.h
MeshQuad$(OG): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h \
	SetOfE4.h
MeshRead$(OG): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h \
	SetOfE4.h
MeshWrite$(OG): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h \
	SetOfE4.h
Meshio$(OG): Meshio.h
Metric$(OG): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h
QuadTree$(OG): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h
R2$(OG): R2.h
SetOfE4$(OG): meshtype.h SetOfE4.h
Xrgraph$(OG): rgraph.h
bamg$(OG): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h
cvmsh2$(OG): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h
drawbdmesh$(OG): Meshio.h Mesh2.h meshtype.h R2.h rgraph.h Metric.h QuadTree.h
macrgraf$(OG): rgraph.h
