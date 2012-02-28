include $(TAXIS_HOME)/src/makefile.in

#INC=-I$(OPTOOL_HOME)/src

SRC=oseen.cc
LIB=liboseen.so

all: $(LIB)

$(LIB): $(SRC)
	$(CXX) $(CFLAGS) -fPIC -c $(SRC) $(INC)
	$(CXX) $(SHAREDFLAGS) -o $(LIB) *.o

clean:
	rm -f *.o $(LIB)
