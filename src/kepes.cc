#include <algorithm>
#include <cmath>
#include "material.h"

using namespace std;

//------------------------------------------------------------------------------
// KEPS flux with entropy dissipation
//------------------------------------------------------------------------------
void Material::kepes_flux (const PrimVar& left,
                           const PrimVar& right,
                           const Vector& normal,
                           Flux& flux) const
{
   double area = normal.norm();
   Vector unit_normal = normal / area;

   double rhol = Density(left);
   double rhor = Density(right);
   double rho = logavg( rhol, rhor );
   Vector vel = (left.velocity + right.velocity) / 2.0;
   double vel2= 0.5 * (left.velocity.square() + right.velocity.square());
   double betal = 0.5 / (gas_const * left.temperature);
   double betar = 0.5 / (gas_const * right.temperature);
   double beta = logavg(betal, betar);

   double p     = 0.5 * (rhol + rhor) / (betal + betar);

   double vel_normal = vel * unit_normal;

   // central flux
   flux.mass_flux = rho * vel_normal;
   flux.momentum_flux = unit_normal * p + vel * flux.mass_flux;
   flux.energy_flux = 0.5 * ( 1.0/((gamma-1.0)*beta) - vel2) * flux.mass_flux + 
                      flux.momentum_flux *  vel;

   flux *= area;
}
