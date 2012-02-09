gamma=1.4
R=287
omg=2*pi*10
r1=1
r2=1.5
rho1=1.0
T=300

A = omg^2 * r1^2 / (2 * R * T)
rho2 = rho1 * exp(A * (r2^2/r1^2 - 1))

p1 = rho1 * R * T
p2 = rho2 * R * T

u1 = omg * r1
u2 = omg * r2

mach1 = u1/sqrt(gamma * R * T)
mach2 = u2/sqrt(gamma * R * T)
