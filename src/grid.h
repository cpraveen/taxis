#ifndef __GRID_H__
#define __GRID_H__

#include <vector>
#include "parameter.h"
#include "vec.h"
#include "face.h"

class Cell
{
   public:
      Vector       centroid;
      unsigned int vertex[3];
      int          face[3];
      double       area;
      std::vector<Vector> normal; // Inward normal
};

struct Vertex
{
   Vector coord;
   std::vector<unsigned int> nbr_vertex;
   std::vector<unsigned int> face;
};

class Grid
{
   public:
      Grid () { n_vertex = n_cell = n_face = n_boundary_face = 0; };
      CellType cell_type;
      unsigned int n_vertex;
      unsigned int n_cell;
      unsigned int n_face;
      unsigned int n_boundary_face;
      double min_cell_area;
      double max_cell_area;
      double min_mcarea;
      double max_mcarea;
      std::vector<Vertex> vertex;
      std::vector<Cell>   cell;
      std::vector<Face>   face;
      std::vector<Face>   bface;
      std::vector<double> mcarea;
      std::vector<double> dcarea;
      std::vector<unsigned int> old_num;
      std::vector<unsigned int> new_num;

      void read (const Parameter& param);
      void find_cell_neighbour(const unsigned int& face_no,
                               const unsigned int& cell_no,
                               int&                neighbour_cell_no);


   private:
      void read_gmsh (std::string grid_file);
      void check_face_type (const std::map<int,BoundaryCondition>& bc);
      void preproc ();
      void compute_cell_centroid ();
      void compute_face_centroid ();
      void compute_cell_area ();
      void compute_face_normal_and_area ();
      void add_face (const Face& new_face);
      void make_faces ();
      void weight_average () ;
      void vertex_weight_check () ;
      void find_cell_faces ();
      void info ();
      void renumber();
      void find_nbr_vertex ();

      std::vector< std::vector<unsigned int> > node_face;

};

#endif
