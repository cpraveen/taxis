set term postscript enhanced solid
set out 'vel.eps'

set key left top
set xlabel 'eta'

set xran[0:1.2]
set yran[0:7]
set ylabel 'u/uinf'
p 'u.blasius' t 'Blasius' w p pt 6 ps 2,'x.dat' u 2:1 t 'Taxis' w l lw 2

set xran[0:1.3]
set yran[0:7]
set ylabel 'v*sqrt(2*Rex)/uinf'
p 'v.blasius' t 'Blasius' w p pt 6 ps 2,'x.dat' u 3:1 t 'Taxis' w l lw 2
