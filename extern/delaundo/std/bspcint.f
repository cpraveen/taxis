      REAL FUNCTION BSPCINT (X,Y,NBVvr,FOutCnv)
C
C Last update:
C 20May96; replace WALK with IWALK.
C 6Dec93,16:00; new PWNTPL.
C     8May92, 21:50. (You got the power!)
C     2Feb92, 0:40. (conversion to single precision.)
C
C     Interpolate spacing values assigned to the nodes of a background
C     triangle on that triangle.
C
C     Input:
C     X:        X-coordinate of the node to interpolate for.
C     Y:        Y-coordinate of the node to interpolate for.
C     NBVvr:    Background cell to start the search with.
C     /NODES/:  Coordinates of the nodes. See main program
C               DELAUNDO for details.
C     /BKGRD/:  Connectivity of the background mesh. See SUBROUTINE
C               INTCLD for details.
C
C     Output:
C     BSpcInt:  Interpolated spacing at x,y.
C     NBVvr:    Background cell that contains x,y.
C     FOutCnv:  .TRUE. if X,Y out of the convex hull.
C     
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C     
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
C
      INCLUDE 'nodes.cbl'
      INCLUDE 'bkgrd.cbl'
C
      INTEGER IWALK
C
C     Find the background cell of the point.
      PCell = IWALK(X,Y,NBVvr,XNode,YNode,NBFrmNde,NghBVvr,MBVvr,
     &              FOutCnv)

      IF (FOutCnv) THEN
C       Return if the node is outside the convex hull.
C       Give a bogus return state to avoid compiler warnings.
        BSpcInt = 1.e25
        RETURN
      END IF
C
C     Forming nodes of the background cell.
      NB1 = NBFrmNde(1,PCell)
      NB2 = NBFrmNde(2,PCell)
      NB3 = NBFrmNde(3,PCell)
C
C     Interpolate with the power SpcPw.
      CALL PWNTPL(NB1,BkgSpc(NB1),NB2,BkgSpc(NB2),NB3,BkgSpc(NB3),
     &            SpcPw,X,Y,BSpcInt)
C
      RETURN
      END
