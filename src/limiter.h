#ifndef __LIMITER_H__
#define __LIMITER_H__

#include <cmath>

inline
double minmod (const double &a, const double &b)
{
   double result;

   if( a < 0.0 && b < 0.0 )
      result = -std::min(-a, -b);
   else if( a > 0.0 && b > 0.0 )
      result = std::min(a, b);
   else
      result = 0.0;

   return result;
}

#endif
