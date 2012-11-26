format compact
gamma=1.4
M=0.8
aoa=10
pinf = 1/(gamma*M*M);

force=load('force.dat');
n=length(force(:,1));
fx = force(n,3);
fy = force(n,4);
aoa= aoa*pi/180;
cd = fx*cos(aoa) + fy*sin(aoa);
cl =-fx*sin(aoa) + fy*cos(aoa);
% normalization
cl = 2*cl
cd = 2*cd

fdata=load('f0000_100001.dat');
fref=load('cf_Re500_M0.8_10deg');
x  = fdata(:,1);
y  = fdata(:,2);
cf = fdata(:,3);
cf = 2*cf;
figure(1)
plot(x,cf,'-',fref(:,1),fref(:,2),'--','LineWidth',2)
set(gca,'FontSize',16)
xlabel('x/c')
ylabel('C_f')

vdata=load('v0000_100001.dat');
vref=load('cp_Re500_M0.8_10deg');
x  = vdata(:,1);
y  = vdata(:,2);
p  = vdata(:,3);
cp = -2*(p - pinf);
figure(2)
plot(x,cp,'-',vref(:,1),vref(:,2),'--','LineWidth',2)
set(gca,'FontSize',16)
xlabel('x/c')
ylabel('-C_p')
