%------------------------------------------------------------------------------
% mach = 0.5 for subsonic and 2.0 for supersonic
%------------------------------------------------------------------------------
function state(mach)

format long e

gamma=1.67
R=208.87
mu=2.2730e-5
r1=0.05      % inner cylinder
r2=0.1       % outer cylinder
r=0.01       % small cylinder
rho2=0.001
mach2=mach
T=300

u2 = mach2 * sqrt(gamma * R * T)
omg=u2/r2

u1 = omg * r1
mach1 = u1/sqrt(gamma * R * T)

A = omg^2 * r1^2 / (2 * R * T)
rho1 = rho2 * exp(-A * (r2^2/r1^2 - 1))

p1 = rho1 * R * T
p2 = rho2 * R * T

Re=rho1*u1*r/mu
Re=rho2*u2*r/mu
Re=0.5*(rho1+rho2)*0.5*(u1+u2)*r/mu

Kn1 = (mu/(rho1*(r2-r1)))*sqrt(pi/(2*R*T))
Kn2 = (mu/(rho2*(r2-r1)))*sqrt(pi/(2*R*T))

