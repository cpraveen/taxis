#include <iostream>
#include <iomanip>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <sstream>
#include <ctime>
#include "parameter.h"
#include "fv.h"
#include "writer.h"

extern bool restart;
extern bool preprocess;
extern bool bounds;

using namespace std;

//------------------------------------------------------------------------------
// Set initial condition
//------------------------------------------------------------------------------
void FiniteVolume::initialize ()
{
   cout << "Initializing memory\n";
   primitive.resize (grid.n_vertex);
   conserved_old.resize (grid.n_vertex);
   residual.resize (grid.n_vertex);
   dt.resize (grid.n_vertex);

   // we need gradient for second order scheme
   dR.resize (grid.n_vertex);
   dU.resize (grid.n_vertex);
   dV.resize (grid.n_vertex);
   dW.resize (grid.n_vertex);
   dP.resize (grid.n_vertex);

   dU_cell.resize (grid.n_cell);
   dV_cell.resize (grid.n_cell);
   dW_cell.resize (grid.n_cell);
   dT_cell.resize (grid.n_cell);

   // If restart option specified, read previous solution from file
   if(restart)
   {
      cout << "Reading restart file restart.dat ...\n";
      ifstream fi;
      fi.open ("restart.dat");
      assert (fi.is_open());
      for(unsigned int i=0; i<grid.n_vertex; ++i)
         fi >> primitive[i].density
            >> primitive[i].velocity.x
            >> primitive[i].velocity.y
            >> primitive[i].velocity.z
            >> primitive[i].pressure;
      fi.close ();
   }
   else
   {
      cout << "Setting initial condition to input values ...";
      for(unsigned int i=0; i<grid.n_vertex; ++i)
      {
         primitive[i] = param.initial_condition.value (grid.vertex[i]);
         assert (primitive[i].density  > 0.0);
         assert (primitive[i].pressure > 0.0);
      }
      cout << " Done\n";
   }
}

//------------------------------------------------------------------------------
// Compute derivatives of velocity and temperature at grid vertices
//------------------------------------------------------------------------------
void FiniteVolume::compute_gradients ()
{
   for(unsigned int i=0; i<grid.n_vertex; ++i)
   {
      dR[i] = 0.0;
      dU[i] = 0.0;
      dV[i] = 0.0;
      dW[i] = 0.0;
      dP[i] = 0.0;
   }

   for(unsigned int i=0; i<grid.n_cell; ++i)
   {
      unsigned int v0 = grid.cell[i].vertex[0];
      unsigned int v1 = grid.cell[i].vertex[1];
      unsigned int v2 = grid.cell[i].vertex[2];

      Vector& normal0 = grid.cell[i].normal[0];
      Vector& normal1 = grid.cell[i].normal[1];
      Vector& normal2 = grid.cell[i].normal[2];

      // density
      Vector dR_cell = normal0 * primitive[v0].density +
                       normal1 * primitive[v1].density +
                       normal2 * primitive[v2].density;
      dR[v0] += dR_cell;
      dR[v1] += dR_cell;
      dR[v2] += dR_cell;

      // x velocity
      dU_cell[i] = normal0 * primitive[v0].velocity.x +
                   normal1 * primitive[v1].velocity.x +
                   normal2 * primitive[v2].velocity.x;
      dU[v0] += dU_cell[i];
      dU[v1] += dU_cell[i];
      dU[v2] += dU_cell[i];

      // y velocity
      dV_cell[i] = normal0 * primitive[v0].velocity.y +
                   normal1 * primitive[v1].velocity.y +
                   normal2 * primitive[v2].velocity.y;
      dV[v0] += dV_cell[i];
      dV[v1] += dV_cell[i];
      dV[v2] += dV_cell[i];

      // z velocity
      dW_cell[i] = normal0 * primitive[v0].velocity.z +
                   normal1 * primitive[v1].velocity.z +
                   normal2 * primitive[v2].velocity.z;
      dW[v0] += dW_cell[i];
      dW[v1] += dW_cell[i];
      dW[v2] += dW_cell[i];

      // pressure
      Vector dP_cell = normal0 * primitive[v0].pressure +
                       normal1 * primitive[v1].pressure +
                       normal2 * primitive[v2].pressure;
      dP[v0] += dP_cell;
      dP[v1] += dP_cell;
      dP[v2] += dP_cell;

      // Temperature gradient
      dT_cell[i] = normal0 * param.material.temperature(primitive[v0]) +
                   normal1 * param.material.temperature(primitive[v1]) +
                   normal2 * param.material.temperature(primitive[v2]);

   }

   // vertex gradients
   for(unsigned int i=0; i<grid.n_vertex; ++i)
   {
      dR[i] /= 6.0 * grid.mcarea[i];
      dU[i] /= 6.0 * grid.mcarea[i];
      dV[i] /= 6.0 * grid.mcarea[i];
      dW[i] /= 6.0 * grid.mcarea[i];
      dP[i] /= 6.0 * grid.mcarea[i];
   }

   // cell gradients
   for(unsigned int i=0; i<grid.n_cell; ++i)
   {
      dU_cell[i] /= 2.0 * grid.cell[i].area;
      dV_cell[i] /= 2.0 * grid.cell[i].area;
      dW_cell[i] /= 2.0 * grid.cell[i].area;
      dT_cell[i] /= 2.0 * grid.cell[i].area;
   }
}

//------------------------------------------------------------------------------
// Compute residual for each cell
//------------------------------------------------------------------------------
void FiniteVolume::compute_residual ()
{

   // Set residual vector to zero
   for(unsigned int i=0; i<grid.n_vertex; ++i)
      residual[i].zero ();

   compute_gradients ();

   // Loop over interior faces and accumulate flux
   for(unsigned int i=0; i<grid.n_face; ++i)
   {
      vector<PrimVar> state(2);
      reconstruct ( i, state );

      Flux flux;
      param.material.num_flux ( state[0], state[1], grid.face[i].normal, flux );

      unsigned int cl = grid.face[i].vertex[0];
      unsigned int cr = grid.face[i].vertex[1];
      residual[cl] += flux;
      residual[cr] -= flux;
   }

   // Loop over boundary faces and accumulate flux
   for(unsigned int i=0; i<grid.bface.size(); ++i)
   {
      vector<PrimVar> state(2);
      Flux flux;

      int face_type = grid.bface[i].type;
      BoundaryCondition& bc = param.boundary_condition[face_type];

      unsigned int cl = grid.bface[i].vertex[0];
      state[0] = primitive[cl];
      state[1] = primitive[cl];
      bc.apply (grid.vertex[cl], grid.bface[i], state);
      param.material.num_flux ( state[0], state[1], grid.bface[i].normal, flux );
      residual[cl] += flux * 0.5;

      unsigned int cr = grid.bface[i].vertex[1];
      state[0] = primitive[cr];
      state[1] = primitive[cr];
      bc.apply (grid.vertex[cr], grid.bface[i], state);
      param.material.num_flux ( state[0], state[1], grid.bface[i].normal, flux );
      residual[cr] += flux * 0.5;
   }

   // Viscous fluxes
   if(param.material.model == Material::ns)
   {
      for(unsigned int i=0; i<grid.n_cell; ++i)
      {
         unsigned int v0 = grid.cell[i].vertex[0];
         unsigned int v1 = grid.cell[i].vertex[1];
         unsigned int v2 = grid.cell[i].vertex[2];
         PrimVar prim_avg = (primitive[v0] + primitive[v1] + primitive[v2]) * (1.0/3.0);
         Flux flux0, flux1, flux2;
         Vector& normal0 = grid.cell[i].normal[0];
         Vector& normal1 = grid.cell[i].normal[1];
         Vector& normal2 = grid.cell[i].normal[2];
         param.material.viscous_flux (prim_avg, 
                                      dU_cell[i], 
                                      dV_cell[i], 
                                      dW_cell[i], 
                                      dT_cell[i],
                                      normal0, flux0,
                                      normal1, flux1,
                                      normal2, flux2);

         // negative sign because normal sign is opposite
         residual[v0] -= flux0 / 2.0;
         residual[v1] -= flux1 / 2.0;
         residual[v2] -= flux2 / 2.0;
      }

      // Diffusive flux from boundary faces
      for(unsigned int i=0; i<grid.bface.size(); ++i)
      {
         vector<PrimVar> state(2);
         int face_type = grid.bface[i].type;
         BoundaryCondition& bc = param.boundary_condition[face_type];

         unsigned int cl = grid.bface[i].lcell;
         unsigned int v0 = grid.bface[i].vertex[0];
         unsigned int v1 = grid.bface[i].vertex[1];
         Vector& normal = grid.bface[i].normal;
         Flux flux0, flux1;

         state[0] = primitive[v0];
         state[1] = primitive[v0];
         bc.apply (grid.vertex[v0], grid.bface[i], state);
         param.material.viscous_flux (bc.adiabatic,
                                      state[0], 
                                      dU_cell[cl], 
                                      dV_cell[cl], 
                                      dW_cell[cl], 
                                      dT_cell[cl],
                                      normal, flux0);
         state[0] = primitive[v1];
         state[1] = primitive[v1];
         bc.apply (grid.vertex[v1], grid.bface[i], state);
         param.material.viscous_flux (bc.adiabatic,
                                      state[0], 
                                      dU_cell[cl], 
                                      dV_cell[cl], 
                                      dW_cell[cl], 
                                      dT_cell[cl],
                                      normal, flux1);

         residual[v0] += flux0 / 2.0;
         residual[v1] += flux1 / 2.0;
      }
   }
}

//------------------------------------------------------------------------------
// Compute time step
//------------------------------------------------------------------------------
void FiniteVolume::compute_dt ()
{
   for(unsigned int i=0; i<grid.n_vertex; ++i)
      dt[i] = 0.0;

   // Interior faces
   for(unsigned int i=0; i<grid.n_face; ++i)
   {
      double area = grid.face[i].area;

      unsigned int cl = grid.face[i].vertex[0];
      double vel_normal_left = primitive[cl].velocity * grid.face[i].normal;
      double c_left = param.material.sound_speed (primitive[cl]);
      dt[cl] += fabs(vel_normal_left) + c_left * area;

      unsigned int cr = grid.face[i].vertex[1];
      double vel_normal_right = primitive[cr].velocity * grid.face[i].normal;
      double c_right = param.material.sound_speed (primitive[cr]);
      dt[cr] += fabs(vel_normal_right) + c_right * area;
   }

   // Boundary faces
   for(unsigned int i=0; i<grid.bface.size(); ++i)
   {
      double area = grid.bface[i].area;

      unsigned int cl = grid.bface[i].vertex[0];
      double vel_normal_left = primitive[cl].velocity * grid.bface[i].normal;
      double c_left = param.material.sound_speed (primitive[cl]);
      dt[cl] += 0.5 * (fabs(vel_normal_left) + c_left * area);

      unsigned int cr = grid.bface[i].vertex[1];
      double vel_normal_right = primitive[cr].velocity * grid.bface[i].normal;
      double c_right = param.material.sound_speed (primitive[cr]);
      dt[cr] += 0.5 * (fabs(vel_normal_right) + c_right * area);
   }

   // Compute global time step
   dt_global = 1.0e20;

   if( param.time_scheme != "lusgs") 
   {
      for(unsigned int i=0; i<grid.n_vertex; ++i)
      {
         dt[i] = param.cfl * grid.dcarea[i] / dt[i];
         dt_global = min( dt_global, dt[i] );
      }
   }
   else
      dt_global = 1.0;

   // For unsteady simulation, use global time step
   if(param.time_mode == "unsteady")
   {
      // Adjust time step so that final time is exactly reached
      if(elapsed_time + dt_global > param.final_time)
         dt_global = param.final_time - elapsed_time;
      for(unsigned int i=0; i<grid.n_vertex; ++i)
         dt[i] = dt_global;
   }
}

//------------------------------------------------------------------------------
// Store old conserved variables for multi-stage RK
//------------------------------------------------------------------------------
void FiniteVolume::store_conserved_old ()
{
   for(unsigned int i=0; i<grid.n_vertex; ++i)
      conserved_old[i] = param.material.prim2con (primitive[i]);
}

//------------------------------------------------------------------------------
// Update solution to new time level
//------------------------------------------------------------------------------
void FiniteVolume::update_solution (const unsigned int r)
{
   double factor;
   ConVar conserved;

   if(param.time_scheme == "rk1" || param.time_scheme == "rk3")
   {
      for(unsigned int i=0; i<grid.n_vertex; ++i)
      {
         factor      = dt[i] / grid.dcarea[i];
         conserved   = param.material.prim2con (primitive[i]);
         conserved   = conserved_old[i] * a_rk[r] +
                       (conserved - residual[i] * factor) * b_rk[r];
         primitive[i]= param.material.con2prim (conserved);
      }
   }
   else if(param.time_scheme == "lusgs")
   { 
   }

   for(unsigned int i=0; i<grid.bface.size(); ++i)
   {
      int face_type = grid.bface[i].type;
      BoundaryCondition& bc = param.boundary_condition[face_type];
      if(bc.type == BC::noslip)
      {
         unsigned int v0 = grid.bface[i].vertex[0];
         unsigned int v1 = grid.bface[i].vertex[1];
         bc.apply_noslip (grid.vertex[v0], primitive[v0]);
         bc.apply_noslip (grid.vertex[v1], primitive[v1]);
      }
   }
}

//------------------------------------------------------------------------------
// Compute L2 norm of mass, momentum and energy residuals
//------------------------------------------------------------------------------
void FiniteVolume::compute_residual_norm (const unsigned int iter)
{
   residual_norm.mass_flux     = 0.0;
   residual_norm.momentum_flux = 0.0;
   residual_norm.energy_flux   = 0.0;

   // Sum of squares for each component
   for(unsigned int i=0; i<grid.n_vertex; ++i)
   {
      double area = grid.dcarea[i];
      residual_norm.mass_flux       += pow(residual[i].mass_flux       / area, 2);
      residual_norm.momentum_flux.x += pow(residual[i].momentum_flux.x / area, 2);
      residual_norm.momentum_flux.y += pow(residual[i].momentum_flux.y / area, 2);
      residual_norm.momentum_flux.z += pow(residual[i].momentum_flux.z / area, 2);
      residual_norm.energy_flux     += pow(residual[i].energy_flux     / area, 2);
   }

   // Take square root and normalize by n_vertex
   residual_norm.mass_flux       = sqrt (residual_norm.mass_flux)       / grid.n_vertex;
   residual_norm.momentum_flux.x = sqrt (residual_norm.momentum_flux.x) / grid.n_vertex;
   residual_norm.momentum_flux.y = sqrt (residual_norm.momentum_flux.y) / grid.n_vertex;
   residual_norm.momentum_flux.z = sqrt (residual_norm.momentum_flux.z) / grid.n_vertex;
   residual_norm.energy_flux     = sqrt (residual_norm.energy_flux)     / grid.n_vertex;

   // Total residual of all components
   residual_norm_total = pow(residual_norm.mass_flux, 2) +
                         residual_norm.momentum_flux.square () +
                         pow(residual_norm.energy_flux, 2);
   residual_norm_total = sqrt (residual_norm_total);

   // Copy residual in first iteration for normalization
   if(iter == 0)
   {
      residual_norm_total0 = residual_norm_total;
      cout << "  Initial residual = " << residual_norm_total0 << endl;
      if(residual_norm_total0 == 0.0)
      {
         cout << "  WARNING: Initial residual is zero !!!\n";
         cout << "  WARNING: Setting it to 1.0\n";
         residual_norm_total0 = 1.0;
      }
   }

   residual_norm_total /= residual_norm_total0;
}

//------------------------------------------------------------------------------
// Log messages to screen and file
//------------------------------------------------------------------------------
void FiniteVolume::log_messages (const unsigned int iter)
{

   if(param.time_mode == "steady")
   {
      // File output
      res_file  << setw(8) << iter << "  " 
                << scientific
                << setprecision (4) 
                << dt_global << "  " 
                << residual_norm_total << "  "
                << residual_norm.mass_flux << "  "
                << residual_norm.momentum_flux.x << "  "
                << residual_norm.momentum_flux.y << "  "
                << residual_norm.momentum_flux.z << "  "
                << residual_norm.energy_flux
                << endl;

      // Screen output
      cout << setw(8) << iter << "  " 
           << scientific
           << setprecision (4) 
           << dt_global << "  " 
           << residual_norm_total << "  "
           << residual_norm.mass_flux << "  "
           << residual_norm.momentum_flux.x << "  "
           << residual_norm.momentum_flux.y << "  "
           << residual_norm.momentum_flux.z << "  "
           << residual_norm.energy_flux
           << endl;
   }
   else
   {
      // File output
      res_file  << setw(8) << iter << "  " 
                << scientific
                << setprecision (4) 
                << dt_global << "  " 
                << elapsed_time 
                << endl;

      // Screen output
      cout << setw(8) << iter << "  " 
           << scientific
           << setprecision (4) 
           << dt_global << "  " 
           << elapsed_time << "  "
           << residual_norm_total
           << endl;
   }

   if(bounds)
      compute_bounds ();
}

//------------------------------------------------------------------------------
// Save solution to file for visualization
//------------------------------------------------------------------------------
void FiniteVolume::output (const unsigned int iter)
{
   Writer writer (grid, param.material);
   writer.attach_data (primitive);
   if(param.write_variables.size() > 0)
      writer.attach_variables (param.write_variables);

   static int counter = 0;
   string filename = "sol";
   if(param.time_mode == "unsteady")
   {
      stringstream ss;
      ss << counter;
      filename += "-" + ss.str() + ".vtk";
      ++counter;
   }
   else
   {
      filename += ".vtk";
   }

   if(param.write_format == "vtk")
      writer.output_vtk (filename);
}

//------------------------------------------------------------------------------
// Save solution to file for restart
//------------------------------------------------------------------------------
void FiniteVolume::output_restart ()
{
   Writer writer (grid);
   writer.attach_data (primitive);
   writer.output_restart ();
}

//------------------------------------------------------------------------------
// Find minimum and maximum values in the solution
//------------------------------------------------------------------------------
void FiniteVolume::compute_bounds () const
{
   PrimVar prim_min;
   PrimVar prim_max;

   prim_min.density    =  1.0e20;
   prim_min.velocity.x =  1.0e20;
   prim_min.velocity.y =  1.0e20;
   prim_min.velocity.z =  1.0e20;
   prim_min.pressure   =  1.0e20;

   prim_max.density    = -1.0e20;
   prim_max.velocity.x = -1.0e20;
   prim_max.velocity.y = -1.0e20;
   prim_max.velocity.z = -1.0e20;
   prim_max.pressure   = -1.0e20;

   for(unsigned int i=0; i<grid.n_vertex; ++i)
   {
      prim_min.density    = min(prim_min.density,    primitive[i].density);
      prim_min.velocity.x = min(prim_min.velocity.x, primitive[i].velocity.x);
      prim_min.velocity.y = min(prim_min.velocity.y, primitive[i].velocity.y);
      prim_min.velocity.z = min(prim_min.velocity.z, primitive[i].velocity.z);
      prim_min.pressure   = min(prim_min.pressure  , primitive[i].pressure  );

      prim_max.density    = max(prim_max.density,    primitive[i].density);
      prim_max.velocity.x = max(prim_max.velocity.x, primitive[i].velocity.x);
      prim_max.velocity.y = max(prim_max.velocity.y, primitive[i].velocity.y);
      prim_max.velocity.z = max(prim_max.velocity.z, primitive[i].velocity.z);
      prim_max.pressure   = max(prim_max.pressure  , primitive[i].pressure  );
   }

   cout << "\t\t Density  :" 
        << setw(15) << prim_min.density 
        << setw(15) << prim_max.density << endl;
   cout << "\t\t xVelocity:"
        << setw(15) << prim_min.velocity.x 
        << setw(15) << prim_max.velocity.x << endl;
   cout << "\t\t yVelocity:"
        << setw(15) << prim_min.velocity.y 
        << setw(15) << prim_max.velocity.y << endl;
   cout << "\t\t zVelocity:"
        << setw(15) << prim_min.velocity.z 
        << setw(15) << prim_max.velocity.z << endl;
   cout << "\t\t Pressure :"
        << setw(15) << prim_min.pressure 
        << setw(15) << prim_max.pressure << endl;

}

//------------------------------------------------------------------------------
// Perform time marching iterations
//------------------------------------------------------------------------------
void FiniteVolume::solve ()
{
   unsigned int iter = 0;
   elapsed_time = 0.0;
   residual_norm_total = 1.0e20;

   while (residual_norm_total > param.min_residue &&
          iter < param.max_iter && 
          elapsed_time < param.final_time)
   {
      store_conserved_old ();
      compute_dt ();
      for(unsigned int r=0; r<param.n_rks; ++r)
      {
         compute_residual ();

         if(r == param.n_rks-1)
            compute_residual_norm (iter);
         update_solution (r);
      }

      ++iter;
      elapsed_time += dt_global;
      log_messages (iter);

      compute_forces (iter);
      if(iter % param.write_frequency == 0) output (iter);
   }

   // Save final solution
   output (iter);

   if(param.write_restart) output_restart ();
}

//------------------------------------------------------------------------------
// This is where the real work starts
//------------------------------------------------------------------------------
void FiniteVolume::run ()
{
   // Read grid from file
   grid.read (param);

   create_force_face_list ();

   // Set initial condition
   initialize ();

   // If -p flag given on command line, then we stop
   if(preprocess)
      return;

   // Measure time taken for solution
   // Store current time
   time_t start_time = time(NULL);

   // Solve the problem
   solve ();

   time_t end_time = time(NULL);
   cout << "Time taken for computation = " << difftime(end_time, start_time)/3600.0 << " hours\n";
}
