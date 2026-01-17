set term postscript
set out 'plot.ps'

set xlabel 'r'
set ylabel 'utheta'
p 'x.dat' u 1:2 t 'Taxis' w lp lw 2, \
  'x.dat' u 1:4 t 'Exact' w l  lw 2

set xlabel 'r'
set ylabel 'p/p0'
p 'x.dat' u 1:3 t 'Taxis' w lp lw 2, \
  'x.dat' u 1:5 t 'Exact' w l  lw 2
