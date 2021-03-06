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

#Read the unstructured grid data
reader = vtk.vtkUnstructuredGridReader()
reader.ReadAllScalarsOn()
reader.ReadAllVectorsOn()
reader.SetFileName(vtkfile)
reader.Update()

# Create three the line source to use for the probe lines.
line = vtk.vtkLineSource()
line.SetPoint1(2.5, 0.0, 0.0)
line.SetPoint2(2.5, 1.0, 0.0)
line.SetResolution(24)

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
arrayid  = ptdata.SetActiveScalars("temperature")
temperature = ptdata.GetArray(arrayid)

uerror = 0
np = 0

f = open('u.dat','w')
for i in range(velocity.GetNumberOfTuples()):
   p  = data.GetPoint(i)
   a  = velocity.GetTuple3(i)
   y  = p[1]
   u  = a[0] # z is along axis of pipe
   ue = y    # linear velocity
   uerror = uerror + (u - ue)*(u-ue)
   np = np + 1
   T  = temperature.GetValue(i)
   s  = str(y) + " " + str(u) + " " + str(ue) + " " + str(T) + "\n"
   f.write(s)

from math import sqrt
uerror = sqrt(uerror/np)
print "Number of points = ", np
print "u error =", uerror
