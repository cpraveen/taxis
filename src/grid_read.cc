#include <iostream>
#include <iomanip>
#include <string>
#include <cstdlib>
#include <cassert>
#include "grid.h"
#include "parameter.h"

using namespace std;

//------------------------------------------------------------------------------
// Read grid from file
//------------------------------------------------------------------------------
void Grid::read (const Parameter& param)
{
   // copy cell type into grid
   cell_type = param.cell_type;

   if(param.grid_type == gmsh)
      read_gmsh (param.grid_file);
   else
   {
      cout << "Unknown grid type specified !!!" << endl;
      abort ();
   }

   // At this stage, we have only boundary faces. We save this number.
   n_boundary_face = n_face;

   check_face_type (param.boundary_condition);
   preproc ();
   info ();
}

//------------------------------------------------------------------------------
// Print some grid information to screen
//------------------------------------------------------------------------------
void Grid::info ()
{
   double min_mcarea =  1.0e20;
   double max_mcarea = -1.0e20;
   double min_dcarea =  1.0e20;
   double max_dcarea = -1.0e20;
   double min_vradius =  1.0e20;
   double max_vradius = -1.0e20;

   for(unsigned int i=0; i<n_vertex; ++i)
   {
      min_mcarea = min ( min_mcarea, mcarea[i] );
      max_mcarea = max ( max_mcarea, mcarea[i] );
      min_dcarea = min ( min_dcarea, dcarea[i] );
      max_dcarea = max ( max_dcarea, dcarea[i] );
      min_vradius = min ( min_vradius, vertex[i].radius );
      max_vradius = max ( max_vradius, vertex[i].radius );
   }

   double min_face_length =  1.0e20;
   double max_face_length = -1.0e20;
   double min_fradius =  1.0e20;
   double max_fradius = -1.0e20;
   for(unsigned int i=0; i<n_vertex; ++i)
   {
      min_face_length = min ( min_face_length, face[i].normal.norm() );
      max_face_length = max ( max_face_length, face[i].normal.norm() );
      min_fradius = min ( min_fradius, face[i].radius );
      max_fradius = max ( max_fradius, face[i].radius );
   }
   
   double min_cell_area =  1.0e20;
   double max_cell_area = -1.0e20;
   double min_cradius =  1.0e20;
   double max_cradius = -1.0e20;
   for(unsigned int i=0; i<n_cell; ++i)
   {
      min_cell_area = min ( min_cell_area, cell[i].area );
      max_cell_area = max ( max_cell_area, cell[i].area );
      min_cradius = min ( min_cradius, cell[i].radius );
      max_cradius = max ( max_cradius, cell[i].radius );
   }

   cout << "Grid information:\n";
   cout << setw(30) << "min" << setw(15) << "max" << endl;
   cout << "  cell area    =  " << setw(15) << min_cell_area 
                                << setw(15) << max_cell_area << endl;
   cout << "  median area  =  " << setw(15) << min_mcarea 
                                << setw(15) << max_mcarea << endl;
   cout << "  dual area    =  " << setw(15) << min_dcarea
                                << setw(15) << max_dcarea << endl;
   cout << "  face length  =  " << setw(15) << min_face_length
                                << setw(15) << max_face_length << endl;
   cout << "  vertex radius = " << setw(15) << min_vradius 
                                << setw(15) << max_vradius << endl;
   cout << "  face   radius = " << setw(15) << min_fradius
                                << setw(15) << max_fradius << endl;
   cout << "  tri    radius = " << setw(15) << min_cradius
                                << setw(15) << max_cradius << endl;

   assert (min_vradius > 0.0);
   assert (min_fradius > 0.0);
   assert (min_cradius > 0.0);
}

//------------------------------------------------------------------------------
// Check that all boundary faces have been assigned a bc type
//------------------------------------------------------------------------------
void Grid::check_face_type (const map<int,BoundaryCondition>& bc)
{
   for(unsigned int i=0; i<face.size(); ++i)
   {
      assert (face[i].type != -1);
      if(bc.find(face[i].type) == bc.end())
      {
         cout << "check_face_type:\n";
         cout << "   No boundary condition specified for\n";
         cout << "   face = " << i << " whose type = " << face[i].type << endl;
         cout << "   There may be more faces with similar problem.\n";
         abort ();
      }
   }
}
