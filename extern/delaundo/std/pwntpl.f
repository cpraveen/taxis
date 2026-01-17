      SUBROUTINE PWNTPL (NB1,UP1,NB2,UP2,NB3,UP3,RP,X,Y,U)
C
C Last update:
C 6Dec93,16:00; remove gradients, interpolate more precisely.
C 8May92, 22:40. (conceived.)
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
C     
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C     
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
C
      INCLUDE 'nodes.cbl'
      REAL RP
C
C     Shorthands.
      X1 = XNode(NB1)
      Y1 = YNode(NB1)
C
C     Coordinate differences.
      X21 = XNode(NB2)-X1
      X31 = XNode(NB3)-X1
      Y21 = YNode(NB2)-Y1
      Y31 = YNode(NB3)-Y1
C
C     Differences in values.
      UP21 = UP2-UP1
      UP31 = UP3-UP1
C
C     Calculate the interpolated value.
      UP = UP1-((Y21*UP31-Y31*UP21)*(X-X1)+(UP21*X31-UP31*X21)*(Y-Y1))/
     &         (X21*Y31-X31*Y21)
C
      U = UP**RP
C
      RETURN
      END
