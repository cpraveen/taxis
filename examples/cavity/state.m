gamma=1.4
R=287
mu=1.0e-0
L=1

T = 300
p = 86100

mach = 0.3
u = mach * sqrt(gamma * R * T)
rho = p / (R * T)
Re = rho * u * L / mu
