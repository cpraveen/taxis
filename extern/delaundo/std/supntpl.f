      SUBROUTINE SUPNTPL (NB1,VP1,X1,Y1,NB2,VP2,X2,Y2,NB3,VP3,X3,Y3,
     &                    VL,UL,VR,UR,SP,SPInv,X,Y,U)
C
C Last update:
C 16Sep93;15:45;cap ValMap from below at 0. for the thin layer of
C               constant thickness.
C 11Nov92;20:45;conceived.
C
C Interpolate values assigned to the nodes of a background
C triangle on that triangle linearly. Map the resulting scalar
C on a supercircle with power SP between the the points 
C (VL,UL,VR,UR).
C
C Input:
C NB1/2/3: Numbers of the nodes to interpolate between.
C VP1/2/3: Values to the power 1/RP to interpolate linearly.
C X/Y/1/2/3: Coordinates of the nodes between to interpolate.
C VL/R:    "X" values of the two points of the linear variation.
C UL/R:    "Y" values of the two points of the linear variation.
C SP:      Exponent of the supercircle.
C SPInv:   Inverse of SP
C X,Y:     Coordinates of the point to interpolate at. 
C
C Output:
C U:       Interpolated value at x,y.
C     
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C     
      IMPLICIT INTEGER(I-P)
      DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3,X,Y,X21,X31,Y21,Y31
      REAL SP,SPInv
C
C     Coordinate differences.
      X21 = X2-X1
      X31 = X3-X1
      Y21 = Y2-Y1
      Y31 = Y3-Y1
C
C     Differences in values.
      V21 = VP2-VP1
      V31 = VP3-VP1
C
C     Calculate the interpolated value.
      ValInt = VP1-((Y21*V31-Y31*V21)*(X-X1)+
     &              (V21*X31-V31*X21)*(Y-Y1))/(X21*Y31-X31*Y21)
C
C     Map the value on the supercircle.
      ValMap = MAX(0.,(ValInt-VL)/(VR-VL))
      UMap = (1-ValMap**SP)**(SPInv)
      U = (UL-UR)*UMap+UR
C
      RETURN
      END
