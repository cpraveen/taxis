format compact
T_inf = 350
p_inf = 1.05e5
p_out = 1e5;
mu_inf = 1.12
gamma=1.4
H = 1.0 % height of channel
L = 5   % length of channel

dpdx = (p_out - p_inf)/L
u_0 = -dpdx/(2*mu_inf) % u(y) = u_0 y (1-y)
u_inf = u_0/4 % maximum velocity, on centerline y=1/2

R = 287.0;
rho_inf = p_inf / (R * T_inf)
c_inf = sqrt(gamma * R * T_inf);
mach_inf = u_inf / c_inf
Re = rho_inf * u_inf * H / mu_inf

