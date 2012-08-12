gamma=1.67
R=208
omg=3140
ri=0.1
ro=0.2
T=300

po=15.237768

A = omg^2 * ri^2 / (2 * R * T)
pi= po * exp(-A * (ro^2/ri^2 - 1))

ri = pi/(R*T)
ro = po/(R*T)
