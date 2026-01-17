format compact
gamma=1.4
R=1
L=1

u    = 1
mach = 0.1
Re   = 100

c = u / mach;
T = c^2/(gamma * R)
rho = 1;
p = rho * R * T
mu = rho * u * L / Re
