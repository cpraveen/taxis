set term postscript enhanced color
set out 'residue.ps'

set logscale y
set xlabel 'Number of iterations'
set ylabel 'Total residual'
p 'residue.dat' u 1:3 w l lw 4

set style line 1 lc rgb "red"     lw 4
set style line 2 lc rgb "blue"    lw 4
set style line 3 lc rgb "magenta" lw 4
set style line 4 lc rgb "brown"    lw 4
set style line 5 lc rgb "black"   lw 4

set logscale y
set xlabel 'Number of iterations' font "Times-Roman, 20"
set ylabel 'Residual' font "Times-Roman, 20"
set key font "Times-Roman, 20"
p 'residue.dat' u 1:4 t 'Density'    w l ls 1, \
  'residue.dat' u 1:5 t 'x momentum' w l ls 2, \
  'residue.dat' u 1:6 t 'y momentum' w l ls 3, \
  'residue.dat' u 1:8 t 'Energy'     w l ls 4
