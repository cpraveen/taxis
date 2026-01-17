#!/usr/bin/env python
"""
Generates cut?.vtp and section?.dat files
cut?.vtp is a VTK XMLPolyData file -> Use paraview to visualize this
section?.dat is a plain ascii file which contains x, y, z, -Cp
"""

import sys
import math

#Check if a vtk file argument is given
if len(sys.argv) == 1:
   print "Please specify a vtk file name."
   print "Example:"
   print "  ", sys.argv[0], " vol_200.vtk"
   sys.exit(1)

#Set the vtk input file name
vtkfile = sys.argv[1]

import vtk

#Free stream values, q_inf = 0.5 * rho * (velocity)**2
omg=100.0
T=300.0
r0=1.0
R=287.0
A = omg*omg*r0*r0/(2*R*T)

#Read the unstructured grid data
reader = vtk.vtkUnstructuredGridReader()
reader.ReadAllScalarsOn()
reader.ReadAllVectorsOn()
reader.SetFileName(vtkfile)
reader.Update()

 # Create three the line source to use for the probe lines.
line = vtk.vtkLineSource()
line.SetPoint1(1.0, 1.0, 0.0)
line.SetPoint2(2.0, 1.0, 0.0)
line.SetResolution(50)

# Move the line into place and create the probe filter.  For
# vtkProbeFilter, the probe line is the input, and the underlying data
# set is the source.
probe = vtk.vtkProbeFilter()
probe.SetInputConnection(line.GetOutputPort())
probe.SetSource(reader.GetOutput())
probe.Update()
data=probe.GetOutput()

#Extract velocity from point data
ptdata   = data.GetPointData()
arrayid  = ptdata.SetActiveVectors("velocity")
velocity = ptdata.GetArray(arrayid)
arrayid  = ptdata.SetActiveScalars("pressure")
pressure = ptdata.GetArray(arrayid)

# Inner wall value
pre0 = pressure.GetValue(0)
# Outer wall value
pre1 = pressure.GetValue(pressure.GetNumberOfTuples()-1)

f = open('x.dat','w')
for i in range(velocity.GetNumberOfTuples()):
   p  = data.GetPoint(i)
   a  = velocity.GetTuple3(i)
   pre= pressure.GetValue(i)
   r  = p[0]
   ut = a[2]
   s  = str(r) + " " + str(ut) + " " + str(pre/pre0) + "  "
   s += str(omg*r) + " " + str(math.exp(A*(r**2/r0**2-1))) + "\n"
   f.write(s)
