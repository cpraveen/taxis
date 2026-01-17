      SUBROUTINE CALCVVR (NFrmNde,XVvr,YVvr,RadVvr,FLinDeg)
C
C Last update:
C 6Oct93,1:35; new tolerance 1.E-8 for FLinDeg.
C 25Sep93,22:09; remove NVvr and pass the relevant NFrmNde directly.
C     3Sep91, 17:38. (Remove bug with Vec13.)
C     7Jul91, 12:45. (Passing grid as argument.)
C
C     Calculate the location of the Voronoi vertex 
C     given the 3 forming nodes. Welcome to DOMINO software.
C
C     Input:
C     NFrmNde: Three forming nodes of the circumcircle.
C     /NODES/: Coordinates of the forming nodes. See main program
C              DELAUNDO for details.
C
C     Output:
C     X/YVvr:  Location of the circumcenter.
C     RadVvr:  Radius of the circumcircle.
C     FLinDeg: Flag to indicate that the three forming nodes of the
C              triangle are collinear when set to true.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      INCLUDE 'nodes.cbl'
      DIMENSION NFrmNde(3)
C
C     Three interior nodes:
C
C     Normal on 1-2.
      XNor12 = YNode(NFrmNde(1))-YNode(NFrmNde(2))
      YNor12 = XNode(NFrmNde(2))-XNode(NFrmNde(1))
      RNor12 = DSQRT(XNor12**2+YNor12**2) 
C
C     Vector 1-3.
      XVec13 = XNode(NFrmNde(3))-XNode(NFrmNde(1))
      YVec13 = YNode(NFrmNde(3))-YNode(NFrmNde(1))
      RVec13 = DSQRT(XVec13**2+YVec13**2)
C
C     Scalar product of Nor12 and Vec13.
      XYPrd1 = XNor12*XVec13+YNor12*YVec13
C
      IF (DABS(XYPrd1).LE.1.D-10*RNor12*RVec13) THEN
C       Degenerate case: Three nodes on a line.
        FLinDeg = .TRUE.
C
      ELSE
C       Valid triangle.
        FLinDeg = .FALSE.
C
C       Normal on side 2-3.
        XNor23 = YNode(NFrmNde(2))-YNode(NFrmNde(3))
        YNor23 = XNode(NFrmNde(3))-XNode(NFrmNde(2))
C
C       Calculate scalar product of the normal on 1-2 and the
C       normal on 2-3.
        XYPrd2 = XNor12*XNor23+YNor12*YNor23
C
C       Calculate the length of the normal from side 1-3 to the
C       center of the circumcircle, i.e. the Voronoi vertex.
        RNor13 = .5*(XYPrd2/XYPrd1)
C
C       Calculate the location of the new Voronoi vertex.
        XVvr = .5*(XNode(NFrmNde(1))+
     &                   XNode(NFrmNde(3)))-RNor13*YVec13
        YVvr = .5*(YNode(NFrmNde(1))+
     &                   YNode(NFrmNde(3)))+RNor13*XVec13
C
C       Calculate the squared radius of the new Voronoi vertex.
        RadVvr = (XVvr-XNode(NFrmNde(1)))**2+(YVvr-YNode(NFrmNde(1)))**2
C
      END IF
C
      RETURN
      END
