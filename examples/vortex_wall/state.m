gamma=1.4;
R=1;

mach=0.5
Re=25

rho=1.0;
utheta=1.0;
rc=1.0;

mu = (rho * utheta * rc)/Re
T = utheta^2/mach^2/(gamma * R)

% grid stretching
ymin=0; ymax=15;
ny=128
r = 1.015
dy = (ymax-ymin)*(r-1)/(r^ny - 1)
