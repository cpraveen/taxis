gamma=1.4;
R=1;
mach=0.63
aoa=2

aoa=aoa*pi/180;
u=cos(aoa)
v=sin(aoa)
p=1/(gamma*mach^2)
rho=1
T=p/(R*rho)
