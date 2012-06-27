      SUBROUTINE DPWNTPL1 (NB1,UP1,NB2,UP2,NB3,UP3,
     &                     RP,X,Y,U,GradUX,GradUY)
C
C Last update:
C 9May92, 0:15. (converted from PWNTPL to DOUBLE PRECISION.)
C
C Interpolate values assigned to the nodes of a background
C triangle on that triangle with a power RP.
C
C Input:
C NB1/2/3: Numbers of the nodes to interpolate between.
C UP1/2/3: Values to the power 1/RP to interpolate linearly.
C RP:      The linearly interpolated value UP will be taken with
C          RP as power.
C X,Y:     Coordinates of the point to interpolate at. 
C /NODES/: Coordinates of the nodes. See main program DELAUNDO for
C          details.
C
C Output:
C U:       Interpolated value at x,y.
C GradU:   Local gradient of the value at x,y.
C     
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C     
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
C
      INCLUDE 'nodes.cbl'
      REAL RP
      DOUBLE PRECISION UP1,UP2,UP3,U,GradUX,GradUY
C
C     Coordinate differences.
      DX1 = XNode(NB3)-XNode(NB2)
      DX2 = XNode(NB1)-XNode(NB3)
      DX3 = XNode(NB2)-XNode(NB1)
      DY1 = YNode(NB3)-YNode(NB2)
      DY2 = YNode(NB1)-YNode(NB3)
      DY3 = YNode(NB2)-YNode(NB1)
C
C     Calculate the area.
      Area = .5*(DX1*DY2-DX2*DY1)
C
C     Calculate the gradient in UP.
      GradUPX = -.5*(DY1*UP1+DY2*UP2+DY3*UP3)/Area
      GradUPY = .5*(DX1*UP1+DX2*UP2+DX3*UP3)/Area
C
C     Find the constant in UP = GradUPX*X+GradUPY*Y+Const.
      Const = UP1-GradUPX*XNode(NB1)-GradUPY*YNode(NB1)
C
C     Find the interpolated UP.
      UP = GradUPX*X+GradUPY*Y+Const
      U = UP**RP
      GradUX = GradUPX*RP*UP**(RP-1.)
      GradUY = GradUPY*RP*UP**(RP-1.)
C
      RETURN
      END
