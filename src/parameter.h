#ifndef __PARAMETER_H__
#define __PARAMETER_H__

#include <vector>
#include <string>
#include <map>
#include <fstream>
#include "reader.h"
#include "vec.h"
#include "material.h"
#include "force.h"
#include "ic.h"
#include "bc.h"

// Coefficients for 3-stage RK scheme of Shu-Osher
static const double a_rk[] = {0.0, 3.0/4.0, 1.0/3.0};
static const double b_rk[] = {1.0, 1.0/4.0, 2.0/3.0};

static const double ALBADA11 = 2.0/3.0;
static const double ALBADA12 = 1.0 - ALBADA11;
static const double ALBADA21 = 4.0/3.0;
static const double ALBADA22 = 1.0 - ALBADA21;

enum GridType {gmsh};
enum CellType {median};

class Parameter
{
   public:
      char* file;

      std::string time_mode;
      std::string time_scheme;
      unsigned int n_rks;
      unsigned int max_iter;
      double cfl;
      double final_time;
      double min_residue;

      enum ReconstructScheme 
      { 
         first, second, limited,
      };

      ReconstructScheme reconstruct_scheme;

      Material material;

      std::string grid_file;
      GridType    grid_type;
      CellType    cell_type;

      InitialCondition initial_condition;

      std::map<int,BoundaryCondition> boundary_condition;

      std::string  write_format;
      unsigned int write_frequency;
      std::vector<std::string> write_variables;
      bool write_restart;

      std::vector<ForceData> force_data;

      void read ();

   private:
      void read_constants (Reader&);
      void read_grid (Reader&);
      void read_numeric (Reader&);
      void read_material (Reader&);
      void read_initial_condition (Reader&);
      void read_boundary (Reader&);
      void read_integrals (Reader&);
      void read_output (Reader&);
};

#endif
