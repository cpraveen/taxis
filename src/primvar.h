#ifndef __PRIMVAR_H__
#define __PRIMVAR_H__

#include "vec.h"

//------------------------------------------------------------------------------
// Primitive variable
//------------------------------------------------------------------------------
class PrimVar
{
   public:
      double temperature, pressure;
      Vector velocity;

      PrimVar  operator+  (const PrimVar& prim_var) const;
      PrimVar  operator-  (const PrimVar& prim_var) const;
      PrimVar  operator*  (const double& scalar) const;
      PrimVar  operator/  (const double& scalar) const;
      PrimVar  operator*  (const PrimVar& prim_var) const; // componentwise multi
      PrimVar& operator*= (const double& scalar);
      PrimVar& operator+= (const PrimVar& prim_var);
      void zero ();
};

//------------------------------------------------------------------------------
// Add two primitive variables and return the result
//------------------------------------------------------------------------------
inline
PrimVar PrimVar::operator+ (const PrimVar& prim_var) const
{
   PrimVar result;

   result.temperature = temperature  + prim_var.temperature;
   result.velocity    = velocity + prim_var.velocity;
   result.pressure    = pressure + prim_var.pressure;

   return result;
}

//------------------------------------------------------------------------------
// Subtract two primitive variables and return the result
//------------------------------------------------------------------------------
inline
PrimVar PrimVar::operator- (const PrimVar& prim_var) const
{
   PrimVar result;

   result.temperature = temperature  - prim_var.temperature;
   result.velocity    = velocity - prim_var.velocity;
   result.pressure    = pressure - prim_var.pressure;

   return result;
}

//------------------------------------------------------------------------------
// Multiply primitive by scalar and return result
//------------------------------------------------------------------------------
inline
PrimVar PrimVar::operator* (const double& scalar) const
{
   PrimVar result;

   result.temperature = temperature * scalar;
   result.velocity    = velocity    * scalar; 
   result.pressure    = pressure    * scalar;

   return result;
}

//------------------------------------------------------------------------------
// Divide primitive by scalar and return result
//------------------------------------------------------------------------------
inline
PrimVar PrimVar::operator/ (const double& scalar) const
{
   double rscalar = 1.0/scalar;
   PrimVar result;

   result.temperature = temperature * rscalar;
   result.velocity    = velocity    * rscalar; 
   result.pressure    = pressure    * rscalar;

   return result;
}

//------------------------------------------------------------------------------
// Multiply two primitive variables componentwise
// Result is another primitive variable
// NOTE: This is not scalar dot product
//------------------------------------------------------------------------------
inline
PrimVar PrimVar::operator* (const PrimVar& prim_var) const
{
   PrimVar result;

   result.temperature = temperature * prim_var.temperature;
   result.velocity.x  = velocity.x  * prim_var.velocity.x;
   result.velocity.y  = velocity.y  * prim_var.velocity.y;
   result.velocity.z  = velocity.z  * prim_var.velocity.z;
   result.pressure    = pressure    * prim_var.pressure;

   return result;
}

//------------------------------------------------------------------------------
// Multiply given primitive by scalar
//------------------------------------------------------------------------------
inline
PrimVar& PrimVar::operator*= (const double& scalar)
{
   temperature *= scalar;
   velocity    *= scalar; 
   pressure    *= scalar;

   return *this;
}

//------------------------------------------------------------------------------
// Add primitive variable to given primitive variable
//------------------------------------------------------------------------------
inline
PrimVar& PrimVar::operator+= (const PrimVar& prim_var)
{
   temperature += prim_var.temperature;
   velocity    += prim_var.velocity;
   pressure    += prim_var.pressure;

   return *this;
}

#endif
