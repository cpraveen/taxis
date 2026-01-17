gamma=1.67;
R=208.87;
mu_ref=50.7e-6;
T_ref=1000;
omega=0.734;

r=0.1524;
Tinf=200
Uinf=2624.1
pinf=47e-1
cinf=sqrt(gamma*R*Tinf);
mach=Uinf/cinf
muinf=mu_ref*(Tinf/T_ref)^omega;
rhoinf=pinf/(R*Tinf)

% Reynolds number
Rey = rhoinf * Uinf * r / muinf

Kn = (muinf/(rhoinf*2*r))*sqrt(pi/(2*R*Tinf))

