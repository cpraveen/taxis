#ifndef __WRITER_H__
#define __WRITER_H__

#include <vector>
#include <string>
#include "grid.h"
#include "material.h"

class Writer
{
   public:
      Writer (const Grid&     grid)
         : 
         grid (&grid),
         has_primitive (false),
         write_mach (false),
         write_density (false)
         {};
      Writer (const Grid&     grid,
              const Material& material) 
         : 
         grid (&grid),
         material (&material),
         has_primitive (false),
         write_mach (false),
         write_density (false)
         {};
      void attach_data (std::vector<PrimVar>& data);
      void attach_variables (const std::vector<std::string>& variables);
      void output_vtk (std::string filename);
      void output_restart ();

   private:

      const Grid*     grid;
      const Material* material;

      std::vector< std::vector<double>* > vertex_data;
      std::vector<std::string> vertex_data_name;
      std::vector<PrimVar>* vertex_primitive;
      bool has_primitive;

      bool write_mach;
      bool write_density;

};

#endif
