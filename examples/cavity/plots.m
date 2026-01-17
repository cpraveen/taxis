xprof=load('xprof.ghia');
vert=load('vertical.dat');
yprof=load('yprof.ghia');
hori=load('horizontal.dat');

figure(1)
%plot(0.5+xprof(:,1),xprof(:,2),'o',vert(:,1),vert(:,2),'LineWidth', 1.5)
plot(xprof(:,2),0.5+xprof(:,1),'o',vert(:,2),vert(:,1),'LineWidth', 1.5)
xlabel('u')
ylabel('y')
axis tight

figure(2)
plot(0.5+yprof(:,1),yprof(:,2),'o',hori(:,1),hori(:,3),'LineWidth', 1.5)
xlabel('x')
ylabel('v')
axis tight
