#!/bin/bash

if [ $# -lt 1 ]
then
   gnuplot $TAXIS_HOME/utils/res_plot.gnu
else
   gnuplot $TAXIS_HOME/utils/res_plot_axi.gnu
fi
