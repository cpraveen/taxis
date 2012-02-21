gamma=1.4
R=287
mu=1e-3
r1=0.05      % inner cylinder
r2=0.1       % outer cylinder
r=0.01       % small cylinder
rho2=1.0
mach2=0.5
T=300

u2 = mach2 * sqrt(gamma * R * T)
omg=u2/r2

u1 = omg * r1
mach1 = u1/sqrt(gamma * R * T)

A = omg^2 * r1^2 / (2 * R * T)
rho1 = rho2 * exp(-A * (r2^2/r1^2 - 1))

p1 = rho1 * R * T
p2 = rho2 * R * T

Re=rho2*u2*(r2-r1)/mu
Re=rho2*u2*r/mu
