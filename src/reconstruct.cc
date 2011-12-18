#include <cmath>
#include "fv.h"

#define LIMITER(r)  (2*(r)/(1+(r)*(r)))

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

   Vector  dr    = grid.vertex[cr] - grid.vertex[cl];
   PrimVar dprim = primitive[cr] - primitive[cl];

   // left state
   PrimVar dpriml;
   dpriml.density    = dR[cl] * dr;
   dpriml.velocity.x = dU[cl] * dr;
   dpriml.velocity.y = dV[cl] * dr;
   dpriml.velocity.z = dW[cl] * dr;
   dpriml.pressure   = dP[cl] * dr;

   state[0] = primitive[cl] + (dpriml * ALBADA11 + dprim * ALBADA12) * 0.5;

   // right state
   PrimVar dprimr;
   dprimr.density    = dR[cr] * dr;
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
