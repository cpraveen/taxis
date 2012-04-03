#ifndef __FV_H__
#define __FV_H__

#include <fstream>
#include <vector>
#include "parameter.h"
#include "material.h"
#include "grid.h"

// Main class for finite volume scheme
class FiniteVolume
{
   public:
      FiniteVolume (char* file) 
      { 
         param.file = file;
         param.read ();

         res_file.open ("residue.dat");

         if(param.force_data.size() > 0)
            force_file.open ("force.dat");

         if(param.has_global == true)
            global_file.open ("global.dat");
      };
      ~FiniteVolume () 
      {
         res_file.close ();
         if(param.force_data.size() > 0)
            force_file.close ();
         if(global_file.is_open())
            global_file.close ();
      };
      void run ();

   private:
      std::ofstream res_file;
      std::ofstream force_file;
      std::ofstream global_file;
      Parameter param;
      Grid      grid;

      std::vector<PrimVar> primitive;
      std::vector<ConVar>  conserved_old;
      std::vector<Flux>    residual;
      std::vector<Vector>  dT, dU, dV, dW, dP;
      std::vector<Vector>  dT_cell, dU_cell, dV_cell, dW_cell, dP_cell;
      Flux                 residual_norm;
      double               residual_norm_total;
      double               residual_norm_total0;
      std::vector<double>  dt;
      double               dt_global;
      std::vector<Force>   force;
      double               elapsed_time;

      void reconstruct (const unsigned int&      f,
                        std::vector<PrimVar>&    state) const;
      void reconstruct_first (const unsigned int&      f,
                              std::vector<PrimVar>&    state) const;
      void reconstruct_second (const unsigned int&      f,
                               std::vector<PrimVar>&    state) const;
      void reconstruct_limited (const unsigned int&      f,
                                std::vector<PrimVar>&    state) const;
      PrimVar limited_slope (const PrimVar& ul, const PrimVar& ur) const;
      void reconstruct_minmod (const unsigned int&      f,
                               std::vector<PrimVar>&    state) const;
      PrimVar minmod_slope (const PrimVar& ul, const PrimVar& ur) const;

      void initialize ();
      void interpolate_vertex ();
      void compute_gradients ();
      void store_conserved_old ();
      void compute_inviscid_residual ();
      void compute_viscous_residual ();
      void compute_residual ();
      void compute_dt ();
      void compute_residual_norm (const unsigned int iter);
      void log_messages (const unsigned int iter);
      void update_solution (const unsigned int r);
      void solve ();
      void compute_bounds (const unsigned int iter);
      void output (const unsigned int iter, bool write_variables = true);
      void output_restart ();
      void lusgs ();
      void create_force_face_list ();
      void compute_forces (unsigned int iter);
      void compute_global (unsigned int iter);
      
};

#endif
