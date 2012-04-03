#include <iostream>
#include <iomanip>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include "parameter.h"
#include "fv.h"
#include "writer.h"

extern bool restart;
extern bool preprocess;
extern bool bounds;

using namespace std;

//------------------------------------------------------------------------------
// Compute inviscid residual for each cell
// NOTE: BOUNDARY FLUXES WORK ONLY IF ALL BOUNDARIES AS NOSLIP
//------------------------------------------------------------------------------
void FiniteVolume::compute_inviscid_kep_residual ()
{
   // Loop over interior faces and accumulate flux
   for(unsigned int i=0; i<grid.n_face; ++i)
   {
      unsigned int cl = grid.face[i].vertex[0];
      unsigned int cr = grid.face[i].vertex[1];

      Flux flux;
      param.material.kep_flux ( primitive[cl], primitive[cr], grid.face[i].normal, flux );

      residual[cl] += flux;
      residual[cr] -= flux;
   }

   // Loop over boundary faces and accumulate flux
   for(unsigned int i=0; i<grid.bface.size(); ++i)
   {
      Vector& normal = grid.bface[i].normal;

      vector<PrimVar> state(2);
      Flux flux;

      int face_type = grid.bface[i].type;
      BoundaryCondition& bc = param.boundary_condition[face_type];

      unsigned int v0 = grid.bface[i].vertex[0];
      unsigned int v1 = grid.bface[i].vertex[1];
      unsigned int vl = grid.bface[i].lvertex;

      state[0] = primitive[v0];
      state[1] = primitive[v0];
      bc.apply (grid.vertex[v0].coord, grid.bface[i], state);

      double p0_mom = (1.0/2.0) * primitive[v0].pressure + 
                      (1.0/6.0) * primitive[v1].pressure + 
                      (1.0/3.0) * primitive[vl].pressure;
      double p0_ene = (primitive[v0].pressure + 
                       primitive[v1].pressure + 
                       primitive[vl].pressure)/3.0;

      double vn0 = state[1].velocity * normal;
      double density0 = param.material.Density (primitive[v0]);
      flux.mass_flux = density0 * vn0;
      flux.momentum_flux = normal * p0_mom + primitive[v0].velocity * flux.mass_flux;
      double E0 = param.material.total_energy (primitive[v0]);
      flux.energy_flux = (E0 + p0_ene) * vn0;

      residual[v0] += flux * 0.5;

      state[0] = primitive[v1];
      state[1] = primitive[v1];
      bc.apply (grid.vertex[v1].coord, grid.bface[i], state);
      double p1_mom = (1.0/2.0) * primitive[v1].pressure + 
                      (1.0/6.0) * primitive[v0].pressure + 
                      (1.0/3.0) * primitive[vl].pressure;
      double p1_ene = (primitive[v1].pressure + 
                       primitive[v0].pressure + 
                       primitive[vl].pressure)/3.0;

      double vn1 = state[1].velocity * normal;
      double density1 = param.material.Density (primitive[v1]);
      flux.mass_flux = density1 * vn1;
      flux.momentum_flux = normal * p1_mom + primitive[v1].velocity * flux.mass_flux;
      double E1 = param.material.total_energy (primitive[v1]);
      flux.energy_flux = (E1 + p1_ene) * vn1;

      residual[v1] += flux * 0.5;
   }
}
