#include <algorithm>
#include <cmath>
#include "material.h"

using namespace std;

//------------------------------------------------------------------------------
// Numerical flux function
//------------------------------------------------------------------------------
void Material::kep_flux (const PrimVar& left,
                         const PrimVar& right,
                         const Vector& normal,
                         Flux& flux) const
{
   double left_density  = Density (left);
   double right_density = Density (right);

   // Enthalpy
   double h_left  = gamma*left.pressure/(left_density*(gamma-1.0))
      + 0.5 * left.velocity.square();
   double h_right = gamma*right.pressure/(right_density*(gamma-1.0))
      + 0.5 * right.velocity.square();

   double density = 0.5 * ( left_density + right_density );
   Vector velocity= (left.velocity + right.velocity) / 2.0;
   double h       = 0.5 * ( h_left + h_right);

   double vn = velocity * normal;
   double pressure = 0.5 * (left.pressure + right.pressure);

   flux.mass_flux = density * vn;
   flux.momentum_flux = normal * pressure + velocity * density * vn;
   flux.energy_flux = density * h * vn;
}
