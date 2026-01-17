# 2-D vertex FV code for Euler/NS on triangular grids

Set the following in your .bashrc file

```shell
export TAXIS_HOME = <path to taxis>
```

e.g.,

```shell
export TAXIS_HOME = /home/praveen/taxis
```

Set the compiler in $TAXIS_HOME/src/makefile.in and type "make" inside src directory to compile the code.

You should also add following lines to your .bashrc file

```shell
PATH=$PATH:$TAXIS_HOME/src
PATH=$PATH:$TAXIS_HOME/utils
PATH=$PATH:$TAXIS_HOME/extern/bamg-v0.68
PATH=$PATH:$TAXIS_HOME/extern/delaundo/std
export PATH
```

## GMSH

GMSH is a free grid generation tool which you can get from here

http://www.geuz.org/gmsh/

There are binary versions available for Linux and Mac. In the taxis/examples directories, you will find files with extension "geo" which are gmsh files. To generate a mesh, open it in gmsh

```shell
gmsh annulus.geo
```

and then click on 

```shell
Mesh -> 2D -> Save
```

This creates the mesh file annulus.msh which can be read by taxis. You can also run gmsh from the command line

```shell
gmsh -2 annulus.geo
```

which creates the msh file without starting the gui.

## BAMG

BAMG is a 2-d grid generation tool with metric-based mesh adaptation capabilities. BAMG is included inside taxis/extern directory. You need to compile it first. It is setup for linux and should also compile on mac.

```shell
cd $TAXIS_HOME/extern/bamg-v0.68
make
```

This should create executables "bamg" and "drawbdmesh" which are useful.

## delaundo

Delaundo is a 2-d triangular grid generation code written by J.-D. Muller. To compile delaundo

```shell
cd $TAXIS_HOME/extern/delaundo/std
make
```

This creates the executable "delaundo" in the std directory which is already added to your PATH variable.

## FAQ

Q: How to see the dual cells ?  
A: Run taxis with -d flag which then creates the files tri.dat and dual.dat containing the primal grid of triangles and the dual grid (median or voronoi). The grid can be seen in gnuplot. Start gnuplot and type

```
gnuplot> plot 'tri.dat' w l
gnuplot> plot 'dual.dat' w l
```

If you dont want to run the computation but only want to see the grids, give the -p flag also.

Q: How to plot residual convergence history ?  
A: Type the following in the terminal

```
bash $ res_plot.sh
```

This will create the file residue.ps which can be opened in gv or any other postscript viewer. You can run this command even while taxis is running so that convergence can be monitored.

---

* `Origin`: https://codeberg.org/cpraveen/taxis
* `Mirror`: https://git.sr.ht/~cpraveen/taxis
* `Mirror`: https://github.com/cpraveen/taxis
