D    = 0.1
mu   = 1e-3
rho  = 1.0

Um   = 1.5

Ub   = (2/3)*Um
Re   = rho * D * Ub / mu

gamma=1.4
R = 1
mach=0.1
c = Um/mach;
T = c^2/(gamma*R)
p = rho * R * T
