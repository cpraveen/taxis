      SUBROUTINE SETUP (MBNde,XNode,YNode,
     &                  MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &                  RadTolRel,RadTol,RadTolMi,RadTolPl,
     &                  IVerbose,FLogFile,NtLog)
C
C Last update:
C 22Apr93,18:55;eliminate common blocks.
C
C Setup an initial simplex (quadrilateral) such that all points 
C to be added are within the convex hull. Welcome to DOMINO
C software.
C
C Input:
C MBNde:
C X/YNode:
C RadTolRel:
C IVerbose:
C FLogFile:
C NtLog:
C 
C
C Output:
C MVvr:
C X/YVvr:
C NFrmNde:
C NghVvr:
C RadVvr:
C RadTol:
C RadTolMi:
C RadTolPl:
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
C
      DIMENSION XNode(*),YNode(*),XVvr(*),YVvr(*),NFrmNde(3,*),
     &          NghVvr(3,*),RadVvr(*)
C
C     Give data for the initial simplex. Set the nonphysical
C     Voronoi vertices 1:4 far out.
c      DATA ((NFrmNde(KFrmNde,NVvr),KFrmNde=1,3),NVvr=1,6)
c     &       /-10,1,4, -10,2,1, -10,3,2 ,-10,4,3, 4,1,3, 1,2,3/,
c     &     ((NghVvr(KNghVvr,NVvr),KNghVvr=1,3),NVvr=1,6)
c     &       /5,-10,-10, 6,-10,-10, 6,-10,-10, 5,-10,-10,
c     &        6,4,1, 3,5,2/
c     &     (RadVvr(NVvr),NVvr=1,4)/4*0./
C     Use simple assignment rather than DATA statement.(Seems to take
C     horrendous compilation time and object size.)
C
C     Forming nodes.
      NFrmNde(1,1) = -10
      NFrmNde(2,1) = 1
      NFrmNde(3,1) = 4
      NFrmNde(1,2) = -10
      NFrmNde(2,2) = 2
      NFrmNde(3,2) = 1
      NFrmNde(1,3) = -10
      NFrmNde(2,3) = 3
      NFrmNde(3,3) = 2
      NFrmNde(1,4) = -10
      NFrmNde(2,4) = 4
      NFrmNde(3,4) = 3
      NFrmNde(1,5) = 4
      NFrmNde(2,5) = 1
      NFrmNde(3,5) = 3
      NFrmNde(1,6) = 1
      NFrmNde(2,6) = 2
      NFrmNde(3,6) = 3
C
C     Neighboring Voronoi vertices.
      NghVvr(1,1) = 5
      NghVvr(2,1) = -10
      NghVvr(3,1) = -10
      NghVvr(1,2) = 6
      NghVvr(2,2) = -10
      NghVvr(3,2) = -10
      NghVvr(1,3) = 6
      NghVvr(2,3) = -10
      NghVvr(3,3) = -10
      NghVvr(1,4) = 5
      NghVvr(2,4) = -10
      NghVvr(3,4) = -10
      NghVvr(1,5) = 6
      NghVvr(2,5) = 4
      NghVvr(3,5) = 1
      NghVvr(1,6) = 3
      NghVvr(2,6) = 5
      NghVvr(3,6) = 2
C
C     Radii of the circumcircles.
      RadVvr(1) = 0.D0
      RadVvr(2) = 0.D0
      RadVvr(3) = 0.D0
      RadVvr(4) = 0.D0
C
C     Find the extension of the domain.
      XMax = XNode(5)
      XMin = XNode(5)
      YMax = YNode(5)
      YMin = YNode(5)
      DO NNode = 6,MBNde
        IF (XNode(NNode).GT.XMax) XMax=XNode(NNode)
        IF (XNode(NNode).LT.XMin) XMin=XNode(NNode)
        IF (YNode(NNode).GT.YMax) YMax=YNode(NNode)
        IF (YNode(NNode).LT.YMin) YMin=YNode(NNode)
      END DO
C
C     Define the "thickness" of the circumcircle.
      RadTol = MAX(XMax-XMin,YMax-YMin)*RadTolRel
      RadTolPl = (1.D0+RadTol)**2
      RadTolMi = (1.D0-RadTol)**2
C
C     Calculate midsides and half of the sidelengths of the convex hull.
      XMid = .5*(XMax+XMin)
      YMid = .5*(YMax+YMin)
      XDel = .75*(XMax-XMin)
      YDel = .75*(YMax-YMin)
C
C     Define - somehow - a scaling factor to
C     fix the diagonal from 3 to 1.
      Squeeze = 1.1D0
C
C     Calculate the first four forming nodes, i.e. the edges of
C     the initial simplex.
      XNode(1) = XMid+XDel
      XNode(2) = XMid+Squeeze*XDel
      XNode(3) = XMid-XDel
      XNode(4) = XNode(3)
      YNode(1) = YMid-YDel
      YNode(2) = YMid+YDel
      YNode(3) = YMid+YDel
      YNode(4) = YMid-Squeeze*YDel
C
C     Locate the Voronoi vertices 1:4 on the midpoints of the
C     faces of the setup hull with zero radius.
C      XVvr(1) = .5*(XNode(4)+XNode(1))
C      XVvr(2) = .5*(XNode(1)+XNode(2))
C      XVvr(3) = .5*(XNode(2)+XNode(3))
C      XVvr(4) = .5*(XNode(3)+XNode(4))
C      YVvr(1) = .5*(YNode(4)+YNode(1))
C      YVvr(2) = .5*(YNode(1)+YNode(2))
C      YVvr(3) = .5*(YNode(2)+YNode(3))
C      YVvr(4) = .5*(YNode(3)+YNode(4))
C     Locate the Voronoi vertices at the number-equivalent
C     vertex. This might make obsolete the passing of XYNode for the special
C     case in IVVALK, when the walk leads around the outside
C     of a setup-vertex.
      DO NVvr = 1,4
        XVvr(NVvr) = XNode(NVvr)
        YVvr(NVvr) = YNode(NVvr)
      END DO
C
C     Calculate the coordinates of the two physical Voronoi vertices.
      DO NVvr = 5,6
        CALL CALCVVR (NFrmNde(1,NVvr),XVvr(NVvr),YVvr(NVvr),
     &                RadVvr(NVvr),FLinDeg)
        IF (FLinDeg) THEN
          IF (IVerbose.GT.0) 
     &    WRITE (*,11) NVvr,(NFrmNde(I,NVvr),I=1,3),
     &                 (XNode(NFrmNde(I,NVvr)),I=1,3),
     &                 (YNode(NFrmNde(I,NVvr)),I=1,3)
          IF (FLogFile) 
     &    WRITE (NtLog,11) NVvr,(NFrmNde(I,NVvr),I=1,3),
     &                     NVvr,(XNode(NFrmNde(I,NVvr)),I=1,3),
     &                     NVvr,(YNode(NFrmNde(I,NVvr)),I=1,3)
   11     FORMAT ('FATAL: Voronoi vertex',I7,
     &            ' could not be calculated in SETUP'/
     &            '       NFrmNde(1:3,',I7,'):',3I7,/
     &            '       XNode  (1:3,',I7,'):',3E15.7/
     &            '       YNode  (1:3,',I7,'):',3E15.7/)
          STOP
        END IF
      END DO
C
C     Initialize the amount of nodes and Voronoi vertices in the
C     structure
      MVvr = 6
C
      RETURN
      END
