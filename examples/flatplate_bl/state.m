format compact
mach_inf = 0.10
rho_inf = 0.1
%mu_inf = 8.0e-4
mu_inf = 3.5e-5
T_inf = 300
gamma=1.4
L = 1.0

R = 287.0;
p_inf = rho_inf * R * T_inf
c_inf = sqrt(gamma * p_inf / rho_inf)
u_inf = c_inf * mach_inf
Re = rho_inf * u_inf * L / mu_inf

% BL thickness at end of plate
d = 4.5*L/sqrt(Re)
eta=50;
y=sqrt(2*L*L/Re)*eta
