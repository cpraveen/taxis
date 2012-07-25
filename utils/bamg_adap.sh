#!/bin/sh -eu
#   the -e option to stop  on error
#   we are with awk to do real operation in the shell
#
bamg=$TAXIS_HOME/extern/bamg-v0.68/bamg
TAXIS=$TAXIS_HOME/src/taxis

#GEO_FILE=wedge.awk
#PAR_FILE=param.in

SOL_FILE=restart.dat
RES_FILE=residue.dat
VTK_FILE=sol0000.vtk
TEC_FILE=sol0000.plt
GRD_FILE=grid.plt

# for awk because in french the number 1/1000 is written 0,001 not 0.001
# to be sure the RADIXCHAR  is '.'   (cf.  Native Language Support)
LANG=C
export LANG

#  default variables
ifin=5
# ---------
HMIN=1e-6
HMINGLOBAL=$HMIN
HMAX=0.5
HCOEF=1
RATIO=2.5
ANISOMAX=10
METRIC=-aniso
# -------
ERR=0.01
ERRCOEF=0.8608916593317348
ERRGLOBAL=0.01
ERRGEO=0.05
MAXSUBDIV=1.8
# -----------
#  end of default parameters 
# ----------
# Read variables from file
# ----------
. ./adap.in

# Set iteration counter
j=0

#  clean of the output file 
rm -f DATA_bamg INIT_*.bb MESH MESH_*.msh METRIC.bb SOL_*.bb

#  create the geometry file 
awk -f $GEO_FILE < /dev/null > MESH_g.msh 

#  create the initial mesh  MESH_0.msh
$bamg  -g MESH_g.msh -o MESH_$j.msh -hmax $HMAX

# This is empty for first iteration
RESTART_FLAG=

while [ $j -lt $ifin ] ; do
   #  i = j + 1
   i=`expr $j + 1`

   ## set the current MESH
   rm -f  MESH
   ln -s MESH_$j.msh MESH

   echo "--------- TAXIS iteration $j    -----------"
   $TAXIS -i $PAR_FILE $RESTART_FLAG

   ##  put all the residual in one file 
   cat $RES_FILE >> RESIDU

   # Copy vtk/plt solution file
   if [ -f $VTK_FILE ]
   then
      mv -f $VTK_FILE sol_$j.vtk
   fi
   if [ -f $TEC_FILE ]
   then
      mv -f $TEC_FILE sol_$j.plt
   fi
   if [ -f $GRD_FILE ]
   then
      mv -f $GRD_FILE grid_$j.plt
   fi

   # Check whether to stop
   jj=`expr $ifin - 1`
   if [ $j -eq $jj ]
   then
      mv RESIDU $RES_FILE
      exit
   fi

   ##  find the nb of vertices in the file MESH 
   nbv=`head -n 13 MESH | tail -n 1 | awk '{print $1}'`

   ##  create the bb file for interpolation 
   echo "2 5 $nbv  2" > SOL_$j.bb
   cat  $SOL_FILE >> SOL_$j.bb

   ## create the bb file for metric construction 
   ## in file SOL_NS  on each line i we have  ro ro*u ro*v  energy 
   ## at vertex i 
   ## + a last line with 2 number last iteration and last time
   #echo "2 1 $nbv  2" > MACH.bb
   #awk 'NF==4 { print sqrt($2*$2+$3*$3)/$1}' SOL_NS >>  MACH.bb
   echo "2 4 $nbv  2" > METRIC.bb
   cat  $SOL_FILE | awk '{print $1, $2, $3, $5}'  >> METRIC.bb

   ## set HMIN = MAX($HMINGLOBAL,$HMIN*$HCOEF) 
   HMIN=`awk "END {c=$HMIN*$HCOEF;c=c<$HMINGLOBAL ?$HMINGLOBAL:c; print c};" </dev/null`
   ERR=`awk "END {c=$ERR*$ERRCOEF;c=c<$ERRGLOBAL ?$ERRGLOBAL:c; print c};" </dev/null`

   echo " -b MESH_$j.msh -err $ERR -errg $ERRGEO -AbsError  
          -hmin $HMIN -hmax $HMAX -Mbb METRIC.bb  -o  MESH_$i.msh 
          $METRIC -anisomax $ANISOMAX
          -ratio $RATIO -rbb SOL_$j.bb   
          -splitpbedge  -maxsubdiv $MAXSUBDIV
          -wbb INIT_$i.bb -v 4 " > DATA_bamg

   echo --- bamg parameters ---
   cat DATA_bamg
   echo ----------------------
   $bamg 

   ##  creation of the INIT_NS for NSC2KE
   ##  remove fisrt line form bb file and  add the last line of  SOL_NS
   sed 1d < INIT_$i.bb > INIT
   tail -1 $SOL_FILE >> INIT
   mv INIT $SOL_FILE

   # change i and not initialisation
   j=$i
   RESTART_FLAG=-r
done
