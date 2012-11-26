#include <algorithm>
#include <cmath>
#include "material.h"

using namespace std;

//------------------------------------------------------------------------------
// KEPS flux with entropy dissipation
//------------------------------------------------------------------------------
void Material::kepes_roe_flux (const PrimVar& left,
                               const PrimVar& right,
                               const Vector& normal,
                               Flux& flux) const
{
   static const double BETA = 1.0/6.0;
   
   double area = normal.norm();
   Vector unit_normal = normal / area;

   double rhol = Density(left);
   double rhor = Density(right);
   double rho = logavg( rhol, rhor );
   double vel2= 0.5 * (left.velocity.square() + right.velocity.square());
   double betal = 0.5 / (gas_const * left.temperature);
   double betar = 0.5 / (gas_const * right.temperature);
   double beta = logavg(betal, betar);
   double a   = sqrt(0.5 * gamma / beta);

   //Vector vel = (left.velocity + right.velocity) / 2.0;
   double sbetal = sqrt(betal);
   double sbetar = sqrt(betar);
   Vector vel = (left.velocity * sbetal + right.velocity * sbetar) / (sbetal + sbetar);

   double p     = 0.5 * (rhol + rhor) / (betal + betar);

   double vel_normal = vel * unit_normal;

   // central flux
   flux.mass_flux = rho * vel_normal;
   flux.momentum_flux = unit_normal * p + vel * flux.mass_flux;
   flux.energy_flux = 0.5 * ( 1.0/((gamma-1.0)*beta) - vel2) * flux.mass_flux + 
                      flux.momentum_flux *  vel;

   // entropy dissipation
   // eigenvectors
   double H = a*a/(gamma-1.0) + 0.5*vel.square();
   double v1 = vel.x * unit_normal.y - vel.y * unit_normal.x;
   double v2 = vel.z * unit_normal.x - vel.x * unit_normal.z;
   double R[][5] = {
      {            1,             1,               0,              0,                      1             },
      {vel.x - a*unit_normal.x, vel.x,             unit_normal.y, -unit_normal.z, vel.x + a*unit_normal.x}, 
      {vel.y - a*unit_normal.y, vel.y,            -unit_normal.x,  0,             vel.y + a*unit_normal.y},
      {vel.z - a*unit_normal.z, vel.z,             0,              unit_normal.x, vel.z + a*unit_normal.z},
      {H     - a*vel_normal,    0.5*vel.square(),  v1,             v2,            H     + a*vel_normal   } 
   };

   // eigenvalues
   double vnl = left.velocity  * unit_normal;
   double vnr = right.velocity * unit_normal;
   double al  = sound_speed (left);
   double ar  = sound_speed (right);
   double LambdaL[] = { vnl - al, vnl, vnl, vnl, vnl + al };
   double LambdaR[] = { vnr - ar, vnr, vnr, vnr, vnr + ar };
   double Lambda[]  = { fabs(vel_normal - a) + BETA*fabs(LambdaL[0]-LambdaR[0]), 
                        fabs(vel_normal),
                        fabs(vel_normal),
                        fabs(vel_normal),
                        fabs(vel_normal + a) + BETA*fabs(LambdaL[4]-LambdaR[4])};

   double S[] = { 0.5*rho/gamma, (gamma-1.0)*rho/gamma, p, p, 0.5*rho/gamma };
   double D[] = { Lambda[0]*S[0], 
                  Lambda[1]*S[1],
                  Lambda[2]*S[2],
                  Lambda[3]*S[3],
                  Lambda[4]*S[4] };

   // jump in entropy: s = log(p) - gamma*log(rho) = (1-gamma)*log(p) + gamma*log(T) + const
   double ds    = (1.0-gamma)*log(right.pressure/left.pressure) + 
                  gamma * log(right.temperature/left.temperature);
   // Jump in entropy variables
   double dV[] = { -ds/(gamma-1.0) - 
                       (betar*right.velocity.square() - betal*left.velocity.square()),
                    2.0*(betar*right.velocity.x - betal*left.velocity.x),
                    2.0*(betar*right.velocity.y - betal*left.velocity.y),
                    2.0*(betar*right.velocity.z - betal*left.velocity.z),
                   -2.0*(betar - betal) };

   // DRT = D * R^T
   double DRT[5][5];
   for(unsigned int i=0; i<5; ++i)
      for(unsigned int j=0; j<5; ++j)
         DRT[i][j] = D[i]*R[j][i];

   // diffusive flux = R * Lambda * S * R^T * dV
   double Diff[] = {0.0, 0.0, 0.0, 0.0, 0.0};
   for(unsigned int i=0; i<5; ++i)
      for(unsigned int j=0; j<5; ++j)
         for(unsigned int k=0; k<5; ++k)
            Diff[i] += R[i][j] * DRT[j][k] * dV[k];

   flux.mass_flux       -= 0.5 * Diff[0];
   flux.momentum_flux.x -= 0.5 * Diff[1];
   flux.momentum_flux.y -= 0.5 * Diff[2];
   flux.momentum_flux.z -= 0.5 * Diff[3];
   flux.energy_flux     -= 0.5 * Diff[4];

   flux *= area;
}
