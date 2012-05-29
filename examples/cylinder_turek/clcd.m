load force.dat;
t=force(:,2);
fx=force(:,3);
fy=force(:,4);
clear force;
rho=1;
D=0.1;
Ub=1;
fact = 0.5*rho*Ub^2*D;
fx=fx/fact;
fy=fy/fact;
figure(1)
plot(t,fx)
xlabel('Time')
ylabel('Drag')

figure(2)
plot(t,fy)
xlabel('Time')
ylabel('Lift')
