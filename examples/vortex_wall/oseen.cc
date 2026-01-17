/* Oseen vortex: see http://en.wikipedia.org/wiki/Lamb–Oseen_vortex */
#include <cmath>

using namespace std;

extern "C" 
void INITIAL_CONDITION(double x, double y, double& T, double& vx, double& vy, double& vz, double& p)
{
   double alpha = 1.25643;
   double xc1 = -2.0;
   double xc2 = +2.0;
   double yc  = 7.5;

   double r1 = sqrt((x-xc1)*(x-xc1) + (y-yc)*(y-yc));
   double r2 = sqrt((x-xc2)*(x-xc2) + (y-yc)*(y-yc));

   double vt1 = (1.0+0.5/alpha)*(1.0 - exp(-alpha*r1*r1))/r1;
   double vt2 = (1.0+0.5/alpha)*(1.0 - exp(-alpha*r2*r2))/r2;

   double theta1 = atan2( y-yc, x - xc1);
   double theta2 = atan2( y-yc, xc2 - x);

   T =  2.85714285714286;
   p =  2.85714285714286;
   vx=  vt1 * sin(theta1) - vt2 * sin(theta2);
   vy= -vt1 * cos(theta1) - vt2 * cos(theta2);
   vz=  0.0;
}

