#include <iostream>
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
   min_mcarea =  1.0e20;
   max_mcarea = -1.0e20;
   min_dcarea =  1.0e20;
   max_dcarea = -1.0e20;
   min_radius =  1.0e20;
   max_radius = -1.0e20;

   for(unsigned int i=0; i<n_vertex; ++i)
   {
      min_mcarea = min ( min_mcarea, mcarea[i] );
      max_mcarea = max ( max_mcarea, mcarea[i] );
      min_dcarea = min ( min_dcarea, dcarea[i] );
      max_dcarea = max ( max_dcarea, dcarea[i] );
      min_radius = min ( min_radius, vertex[i].radius );
      max_radius = max ( max_radius, vertex[i].radius );
   }

   min_face_length =  1.0e20;
   max_face_length = -1.0e20;
   for(unsigned int i=0; i<n_vertex; ++i)
   {
      min_face_length = min ( min_face_length, face[i].normal.norm() );
      max_face_length = max ( max_face_length, face[i].normal.norm() );
   }
   
   cout << "Grid information:\n";
   cout << "  Number of vertices   = " << n_vertex << endl;
   cout << "  Number of cells      = " << n_cell << endl;
   cout << "  Number of faces      = " << n_face << endl;
   cout << "  Number of bdry faces = " << n_boundary_face << endl;
   cout << "  Minimum cell area    = " << min_cell_area << endl;
   cout << "  Maximum cell area    = " << max_cell_area << endl;
   cout << "  Minimum median area  = " << min_mcarea << endl;
   cout << "  Maximum median area  = " << max_mcarea << endl;
   cout << "  Minimum dual area    = " << min_dcarea << endl;
   cout << "  Maximum dual area    = " << max_dcarea << endl;
   cout << "  Minimum face length  = " << min_face_length << endl;
   cout << "  Maximum face length  = " << max_face_length << endl;
   cout << "  Minimum radius       = " << min_radius << endl;
   cout << "  Maximum radius       = " << max_radius << endl;
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
