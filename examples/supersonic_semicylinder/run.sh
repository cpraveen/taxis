#!/bin/sh
#   the -e option to stop  on error
#   we are with awk to do real operation in the shell
#
bamg=bamg
AWK=awk
SED=sed

# for awk because in french the number 1/1000 is written 0,001 not 0.001
# to be sure the RADIXCHAR  is '.'   (cf.  Native Language Support)
LANG=C
export LANG

#  some VAR
METRIC="-iso"
ANISOMAX=2.5
MAXVERT=45000
# ---------
HMIN=1e-4
HMINGLOBAL=$HMIN
HCOEF=1
HMAX=1.5
# -------
#  end of some parameters 
# ----------
#  clean of the output file 
#rm -f [A-Z]*
rm -f MESH

#  create the geometry file 
$AWK -f semicyl.awk </dev/null > MESH_g.msh 

#  create the initial mesh  MESH_0.amdba
$bamg  -g MESH_g.msh -o MESH -hmax $HMAX $METRIC

###############################################################################
