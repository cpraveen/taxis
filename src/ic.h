#ifndef __IC_H__
#define __IC_H__

#include <iostream>
#include <string>
#include "vec.h"
#include "material.h"
#include "fparser.h"

//------------------------------------------------------------------------------
// Class to store initial condition functions
//------------------------------------------------------------------------------
class InitialCondition
{
   public:
      void    add (std::string, std::string);
      PrimVar value (const Vector& p);

   private:
      FParser temperature;
      FParser xvelocity;
      FParser yvelocity;
      FParser zvelocity;
      FParser pressure;
};

//------------------------------------------------------------------------------
// Add function as defined in "fun"
//------------------------------------------------------------------------------
inline
void InitialCondition::add (std::string variable, std::string fun)
{
   if(variable == "temperature")
      temperature.FParse (fun);
   else if(variable == "xvelocity")
      xvelocity.FParse (fun);
   else if(variable == "yvelocity")
      yvelocity.FParse (fun);
   else if(variable == "zvelocity")
      zvelocity.FParse (fun);
   else if(variable == "pressure")
      pressure.FParse (fun);
   else
   {
      std::cout << "InitialCondition::add: Unknown variable " << variable << std::endl;
      abort ();
   }
}

//------------------------------------------------------------------------------
// Evaluate primitive variables for given point
//------------------------------------------------------------------------------
inline
PrimVar InitialCondition::value (const Vector& p)
{
   PrimVar result;

   double vals[3] = {p.x, p.y, p.z};

   result.temperature= temperature.Eval (vals);
   result.velocity.x = xvelocity.Eval (vals);
   result.velocity.y = yvelocity.Eval (vals);
   result.velocity.z = zvelocity.Eval (vals);
   result.pressure   = pressure.Eval (vals);

   return result;
}

#endif
