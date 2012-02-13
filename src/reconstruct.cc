#include <cmath>
#include "fv.h"

#define EPSILON  1.0e-14
#define limit_albada(a,b)  (a*b <= 0.0) ? 0.0 : \
                              ((a*a + EPSILON)*b + (b*b + EPSILON)*a)/(a*a + b*b + 2.0*EPSILON)

using namespace std;

//------------------------------------------------------------------------------
// First order Reconstruct left and right states
//------------------------------------------------------------------------------
void FiniteVolume::reconstruct_first
(
 const unsigned int& f,
 vector<PrimVar>&    state
) const
{
   // Left state
   unsigned int cl = grid.face[f].vertex[0];
   state[0] = primitive[cl];

   unsigned int cr = grid.face[f].vertex[1];
   state[1] = primitive[cr];
}

//------------------------------------------------------------------------------
// Second order Reconstruct left and right states
//------------------------------------------------------------------------------
void FiniteVolume::reconstruct_second
(
 const unsigned int& f,
 vector<PrimVar>&    state
) const
{
   unsigned int cl = grid.face[f].vertex[0];
   unsigned int cr = grid.face[f].vertex[1];

   Vector  dr    = grid.vertex[cr].coord - grid.vertex[cl].coord;
   PrimVar dprim = primitive[cr] - primitive[cl];

   // left state
   PrimVar dpriml;
   dpriml.temperature= dT[cl] * dr;
   dpriml.velocity.x = dU[cl] * dr;
   dpriml.velocity.y = dV[cl] * dr;
   dpriml.velocity.z = dW[cl] * dr;
   dpriml.pressure   = dP[cl] * dr;

   state[0] = primitive[cl] + (dpriml * ALBADA11 + dprim * ALBADA12) * 0.5;

   // right state
   PrimVar dprimr;
   dprimr.temperature= dT[cr] * dr;
   dprimr.velocity.x = dU[cr] * dr;
   dprimr.velocity.y = dV[cr] * dr;
   dprimr.velocity.z = dW[cr] * dr;
   dprimr.pressure   = dP[cr] * dr;

   state[1] = primitive[cr] - (dprimr * ALBADA11 + dprim * ALBADA12) * 0.5;
}

//------------------------------------------------------------------------------
// Reconstruct left and right states
//------------------------------------------------------------------------------
void FiniteVolume::reconstruct_limited
(
 const unsigned int& f,
 vector<PrimVar>&    state
) const
{
   unsigned int cl = grid.face[f].vertex[0];
   unsigned int cr = grid.face[f].vertex[1];

   Vector  dr    = grid.vertex[cr].coord - grid.vertex[cl].coord;
   PrimVar dprim = primitive[cr] - primitive[cl];

   // left state
   PrimVar dpriml;
   dpriml.temperature= dT[cl] * dr;
   dpriml.velocity.x = dU[cl] * dr;
   dpriml.velocity.y = dV[cl] * dr;
   dpriml.velocity.z = dW[cl] * dr;
   dpriml.pressure   = dP[cl] * dr;
   dpriml = dpriml * ALBADA21 + dprim * ALBADA22;
   PrimVar si = limited_slope (dpriml, dprim);

   state[0] = primitive[cl] + si * 0.5;

   // right state
   PrimVar dprimr;
   dprimr.temperature= dT[cr] * dr;
   dprimr.velocity.x = dU[cr] * dr;
   dprimr.velocity.y = dV[cr] * dr;
   dprimr.velocity.z = dW[cr] * dr;
   dprimr.pressure   = dP[cr] * dr;
   dprimr = dprimr * ALBADA21 + dprim * ALBADA22;
   PrimVar sj = limited_slope (dprimr, dprim);

   state[1] = primitive[cr] - sj * 0.5;
}

//------------------------------------------------------------------------------
// Computed limited slope
//------------------------------------------------------------------------------
PrimVar FiniteVolume::limited_slope (const PrimVar& ul, const PrimVar& ur) const
{
   PrimVar result;

   result.temperature = limit_albada (ul.temperature, ur.temperature);
   result.velocity.x  = limit_albada (ul.velocity.x , ur.velocity.x);
   result.velocity.y  = limit_albada (ul.velocity.y , ur.velocity.y);
   result.velocity.z  = limit_albada (ul.velocity.z , ur.velocity.z);
   result.pressure    = limit_albada (ul.pressure   , ur.pressure);

   return result;
}

//------------------------------------------------------------------------------
// Reconstruct left and right states
//------------------------------------------------------------------------------
void FiniteVolume::reconstruct (const unsigned int& f,
                                vector<PrimVar>&    state) const
{
   switch(param.reconstruct_scheme)
   {
      case Parameter::first:
         reconstruct_first (f, state);
         break;

      case Parameter::second:
         reconstruct_second (f, state);
         break;

      case Parameter::limited:
         reconstruct_limited (f, state);
         break;

      default:
         cout << "reconstruct: unknown reconstruction scheme = " 
              << param.reconstruct_scheme << endl;
         abort ();
   }
}
