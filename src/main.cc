#include <iostream>
#include <cstdlib>
#include <cassert>
#include <string>
#include <map>
#include "fv.h"

using namespace std;

bool debug;
bool restart;
bool preprocess;
bool bounds;
bool convert_to_vtk;
bool convert_to_tec;
map<string,double> constants;

void process_command_line (int argc, char* argv[], int& ifile);

int main(int argc, char* argv[])
{
   cout << "Starting flo3d\n";   
   int ifile;
   process_command_line (argc, argv, ifile);

   FiniteVolume problem (argv[ifile]);
   problem.run ();
}
