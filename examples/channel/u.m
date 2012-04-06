load u.dat

T = 7.14285714285714e1

plot(u(:,1), u(:,2), 'o', u(:,1), u(:,3), '-', u(:,1), u(:,4)/T, 'o--', ...
     'LineWidth', 1.5)
xlabel('y', 'FontSize', 14)
leg=legend('Velocity', 'Velocity (exact)', 'Temperature');
set(leg, 'FontSize', 14)
set(gca, 'FontSize', 14)


clear all
