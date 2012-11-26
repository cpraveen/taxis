gamma=1.4;
R=1;

M=0.8
aoa=10
Re=500
mu=1/Re

%---------
aoa = aoa*pi/180;
u   = cos(aoa)
v   = sin(aoa)
T   = 1/(gamma*M^2)
p   = 1/(gamma*M^2)

%---------
M=0.5
aoa=0
Re=5000
mu=1/Re

%---------
aoa = aoa*pi/180;
u   = cos(aoa)
v   = sin(aoa)
T   = 1/(gamma*M^2)
p   = 1/(gamma*M^2)
