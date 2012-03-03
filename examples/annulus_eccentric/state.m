% Argon gas
gamma=1.67
R=208.87
mach=0.5
T=300
rho=1.0e-2
mu=2.2730e-5

% pressure
p = rho*R*T

% R = Ri/Re
Rb = 0.5

% Radius of outer cylinder
Re=0.01

% Radius of inner cylinder
Ri=Rb * Re

c = sqrt(gamma*R*T)
utheta = mach * c;
omega = utheta/Re

% Reynolds number
Rey = rho * utheta * (Re-Ri) / mu

Kn = (mu/(rho*(Re-Ri)))*sqrt(pi/(2*R*T))
