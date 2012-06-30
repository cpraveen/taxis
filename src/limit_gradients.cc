#include "fv.h"

using namespace std;

//------------------------------------------------------------------------------
// Update phi using Barth-Jespersen scheme
//------------------------------------------------------------------------------
void minmax (const double& T, 
             const double& u, 
             const double& v, 
             const double& w, 
             const double& p, 
             const PrimVar& pmin, 
             const PrimVar& pmax, 
             const PrimVar& prim, 
             PrimVar& phi)
{
   // temperature
   if(T > pmax.temperature)
   {
      double fact = (pmax.temperature - prim.temperature) / 
                    (T - prim.temperature);
      phi.temperature = min(phi.temperature, fact);
   }
   else if(T < pmin.temperature)
   {
      double fact = (pmin.temperature - prim.temperature) / 
                    (T - prim.temperature);
      phi.temperature = min(phi.temperature, fact);
   }

   // x velocity
   if(u > pmax.velocity.x)
   {
      double fact = (pmax.velocity.x - prim.velocity.x) / 
                    (u - prim.velocity.x);
      phi.velocity.x = min(phi.velocity.x, fact);
   }
   else if(u < pmin.velocity.x)
   {
      double fact = (pmin.velocity.x - prim.velocity.x) / 
                    (u - prim.velocity.x);
      phi.velocity.x = min(phi.velocity.x, fact);
   }

   // y velocity
   if(v > pmax.velocity.y)
   {
      double fact = (pmax.velocity.y - prim.velocity.y) / 
                    (v - prim.velocity.y);
      phi.velocity.y = min(phi.velocity.y, fact);
   }
   else if(v < pmin.velocity.y)
   {
      double fact = (pmin.velocity.y - prim.velocity.y) / 
                    (v - prim.velocity.y);
      phi.velocity.y = min(phi.velocity.y, fact);
   }

   // z velocity
   if(w > pmax.velocity.z)
   {
      double fact = (pmax.velocity.z - prim.velocity.z) / 
                    (w - prim.velocity.z);
      phi.velocity.z = min(phi.velocity.z, fact);
   }
   else if(w < pmin.velocity.z)
   {
      double fact = (pmin.velocity.z - prim.velocity.z) / 
                    (w - prim.velocity.z);
      phi.velocity.z = min(phi.velocity.z, fact);
   }

   // pressure
   if(p > pmax.pressure)
   {
      double fact = (pmax.pressure - prim.pressure) / 
                    (p - prim.pressure);
      phi.pressure = min(phi.pressure, fact);
   }
   else if(p < pmin.pressure)
   {
      double fact = (pmin.pressure - prim.pressure) / 
                    (p - prim.pressure);
      phi.pressure = min(phi.pressure, fact);
   }

}

//------------------------------------------------------------------------------
// Modify gradient using Minmax limiter
//------------------------------------------------------------------------------
void FiniteVolume::limit_gradients ()
{
   vector<PrimVar> pmin (grid.n_vertex);
   vector<PrimVar> pmax (grid.n_vertex);
   vector<PrimVar> phi  (grid.n_vertex);

   for(unsigned int i=0; i<grid.n_vertex; ++i)
   {
      pmin[i] = primitive[i];
      pmax[i] = primitive[i];
      phi[i]  = 1.0;
   }

   // For each vertex, find min and max of surrounding values
   for(unsigned int i=0; i<grid.n_face; ++i)
   {
      unsigned int v0 = grid.face[i].vertex[0];
      unsigned int v1 = grid.face[i].vertex[1];
      pmin[v0].min(primitive[v1]);
      pmax[v0].max(primitive[v1]);
      pmin[v1].min(primitive[v0]);
      pmax[v1].max(primitive[v0]);
   }

   for(unsigned int i=0; i<grid.n_face; ++i)
   {
      unsigned int n0 = grid.face[i].vertex[0];
      unsigned int n1 = grid.face[i].vertex[1];
      Vector dr = grid.vertex[n1].coord - grid.vertex[n0].coord;

      double T0 = primitive[n0].temperature + (dT[n0] * dr);
      double u0 = primitive[n0].velocity.x  + (dU[n0] * dr);
      double v0 = primitive[n0].velocity.y  + (dV[n0] * dr);
      double w0 = primitive[n0].velocity.z  + (dW[n0] * dr);
      double p0 = primitive[n0].pressure    + (dP[n0] * dr);

      minmax (T0, u0, v0, w0, p0, pmin[n0], pmax[n0], primitive[n0], phi[n0]);

      double T1 = primitive[n1].temperature - (dT[n1] * dr);
      double u1 = primitive[n1].velocity.x  - (dU[n1] * dr);
      double v1 = primitive[n1].velocity.y  - (dV[n1] * dr);
      double w1 = primitive[n1].velocity.z  - (dW[n1] * dr);
      double p1 = primitive[n1].pressure    - (dP[n1] * dr);

      minmax (T1, u1, v1, w1, p1, pmin[n1], pmax[n1], primitive[n1], phi[n1]);
   }

   for(unsigned int i=0; i<grid.n_vertex; ++i)
   {
      dT[i] *= phi[i].temperature;
      dU[i] *= phi[i].velocity.x;
      dV[i] *= phi[i].velocity.y;
      dW[i] *= phi[i].velocity.z;
      dP[i] *= phi[i].pressure;
   }
      
}
