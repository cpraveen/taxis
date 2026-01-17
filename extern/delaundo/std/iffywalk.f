      INTEGER FUNCTION IFFYWALK (X,Y,NVvr,XNode,YNode,NFrmNde,NghVvr,
     &                           MVvr,FOutCnv)
C
C Last update:
C 20Aug93,16:15; derived from IWALK.
C
C Find the cell the point X,Y lies in, given a cell to start walking
C from. Walking is done in the direction of the maximum scalar
C product of the vector from the midside of a face to the point x,y
C and the outward normal on that face. The difference to IWALK is that
C scaled vectors are used for the scalar product, in order to avoid
C terminating the search in a nearby cell in very finely discretized
C regions.
C
C Input:
C X:        X coordinate of the point to check for.
C Y:        Y coordinate of the point to check for.
C NVvr:     Mesh cell to start checking.
C XNode:    Vector of x-coordinates of the nodes.
C YNode:    Vector of y-coordinates of the nodes.
C NFrmNde:  Array of forming nodes of the triangulation.
C NghVvr:   Array of neighbouring cells of the triangulation.
C MVvr:     Maximum number of cells to walk over.
C
C Output:
C IWalk:    Mesh cell that contains the point.
C NVvr:     Mesh cell that contains the point.
C FOutCnv:  Flag indicating that search path left the convex hull
C           when set to true.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION (R-S,X-Y)
      DIMENSION NFrmNde(3,*),NghVvr(3,*)
      DOUBLE PRECISION X,Y,XNode(*),YNode(*)
C
      IF (NFrmNde(1,NVvr).LE.0.OR.NFrmNde(2,NVvr).LE.0.OR.
     &    NFrmNde(3,NVvr).LE.0) THEN
        FOutCnv = .TRUE.
        IFFYWALK = 0
        RETURN
      END IF
      FOutCnv = .FALSE.
C
C     Calculate scalar products in NVvr.
      N1 = NFrmNde(1,NVvr)
      N2 = NFrmNde(2,NVvr)
      N3 = NFrmNde(3,NVvr)
C
C     Midpoint on first face.
      XMid1 = .5*(XNode(N2)+XNode(N3))
      YMid1 = .5*(YNode(N2)+YNode(N3))
C     Outward normal on that face.
      XNrml1 = YNode(N3)-YNode(N2)
      YNrml1 = XNode(N2)-XNode(N3)     
      SNrml1 = SQRT(XNrml1**2+YNrml1**2)
      XNrml1 = XNrml1/SNrml1 
      YNrml1 = YNrml1/SNrml1  
C     Vector from midside to point.
      XVec1 = X-XMid1
      YVec1 = Y-YMid1
      SVec1 = MAX(1.D-14,SQRT(XVec1**2+YVec1**2))
      XVec1 = XVec1/SVec1
      YVec1 = YVec1/SVec1
C     Scalar product.
      ScPrd1 = XVec1*XNrml1+YVec1*YNrml1
C
C     Midpoint on second face.
      XMid2 = .5*(XNode(N3)+XNode(N1))
      YMid2 = .5*(YNode(N3)+YNode(N1))
C     Outward normal on that face.
      XNrml2 = YNode(N1)-YNode(N3)
      YNrml2 = XNode(N3)-XNode(N1)     
      SNrml2 = SQRT(XNrml2**2+YNrml2**2)
      XNrml2 = XNrml2/SNrml2 
      YNrml2 = YNrml2/SNrml2  
C     Vector from midside to point.
      XVec2 = X-XMid2
      YVec2 = Y-YMid2
      SVec2 = MAX(1.D-14,SQRT(XVec2**2+YVec2**2))
      XVec2 = XVec2/SVec2
      YVec2 = YVec2/SVec2
C     Scalar product.
      ScPrd2 = XVec2*XNrml2+YVec2*YNrml2
C
C     Midpoint on third face.
      XMid3 = .5*(XNode(N1)+XNode(N2))
      YMid3 = .5*(YNode(N1)+YNode(N2))
C     Outward normal on that face.
      XNrml3 = YNode(N2)-YNode(N1)
      YNrml3 = XNode(N1)-XNode(N2)     
      SNrml3 = SQRT(XNrml3**2+YNrml3**2)
      XNrml3 = XNrml3/SNrml3  
      YNrml3 = YNrml3/SNrml3 
C     Vector from midside to point.
      XVec3 = X-XMid3
      YVec3 = Y-YMid3
      SVec3 = MAX(1.D-14,SQRT(XVec3**2+YVec3**2))
      XVec3 = XVec3/SVec3
      YVec3 = YVec3/SVec3
C     Scalar product.
      ScPrd3 = XVec3*XNrml3+YVec3*YNrml3
C
      IF (ScPrd1.GE.ScPrd2.AND.ScPrd1.GE.ScPrd3.AND.
     &    ScPrd1.GT.1.E-10) THEN
C       Continue search along face 1.
        KNgh = 1
C
      ELSE IF (ScPrd2.GE.ScPrd3.AND.ScPrd2.GE.ScPrd1.AND.
     &         ScPrd2.GT.1.E-10) THEN
C       Continue search along face 2.
        KNgh = 2
C
      ELSE IF (ScPrd3.GE.ScPrd1.AND.ScPrd3.GE.ScPrd2.AND.
     &         ScPrd3.GT.1.E-10) THEN
C       Continue search along face 3.
        KNgh = 3
C
      ELSE
C       All scalar products are non positive. The point is inside
C       or on the triangle.
        IffyWalk = NVvr
        RETURN
C
      END IF
C
C     Start walking.
      MChk = 1
      DO WHILE (MChk.LT.MVvr)
        MChk = MChk+1
C
C       Next neighbour to look at.
        NVvrNxt = NghVvr(KNgh,NVvr)
        IF (NVvrNxt.LE.0) THEN
          FOutCnv = .TRUE.
          IFFYWALK = 0
          RETURN
        END IF

C       Find back the initial cell in the new neighbour.
        KNgh = 0
        DO KKNgh = 1,3
          IF (NghVvr(KKNgh,NVvrNxt).EQ.NVvr) KNgh = KKNgh
        END DO
        IF (KNgh.EQ.0) THEN
          WRITE (*,'(2(A,I7))') ' While walking,',NVvr,' not found in',
     &                          NVvrNxt
          STOP
        END IF
C
        NVvr = NVvrNxt
C
C       Cyclic succession.
        K1 = KNgh
        K2 = JCYCL(KNgh+1)
        K3 = JCYCL(KNgh+2)
        N1 = NFrmNde(K1,NVvr)
        N2 = NFrmNde(K2,NVvr)
        N3 = NFrmNde(K3,NVvr)
C
C       Midpoint on face KNgh+1.
        XMid2 = .5*(XNode(N3)+XNode(N1))
        YMid2 = .5*(YNode(N3)+YNode(N1))
C       Outward normal on that face.
        XNrml2 = YNode(N1)-YNode(N3)
        YNrml2 = XNode(N3)-XNode(N1)     
        SNrml2 = SQRT(XNrml2**2+YNrml2**2)
        XNrml2 = XNrml2/SNrml2  
        YNrml2 = YNrml2/SNrml2  
C       Vector from midside to point.
        XVec2 = X-XMid2
        YVec2 = Y-YMid2
        SVec2 = MAX(1.D-14,SQRT(XVec2**2+YVec2**2))
        XVec2 = XVec2/SVec2
        YVec2 = YVec2/SVec2
C       Scalar product.
        ScPrd2 = XVec2*XNrml2+YVec2*YNrml2
C
C       Midpoint on face KNgh+2.
        XMid3 = .5*(XNode(N1)+XNode(N2))
        YMid3 = .5*(YNode(N1)+YNode(N2))
C       Outward normal on that face.
        XNrml3 = YNode(N2)-YNode(N1)
        YNrml3 = XNode(N1)-XNode(N2)     
        SNrml3 = SQRT(XNrml3**2+YNrml3**2)
        XNrml3 = XNrml3/SNrml3  
        YNrml3 = YNrml3/SNrml3  
C       Vector from midside to point.
        XVec3 = X-XMid3
        YVec3 = Y-YMid3
        SVec3 = MAX(1.D-14,SQRT(XVec3**2+YVec3**2))
        XVec3 = XVec3/SVec3
        YVec3 = YVec3/SVec3
C       Scalar product.
        ScPrd3 = XVec3*XNrml3+YVec3*YNrml3
C
        IF (ScPrd2.GE.ScPrd3.AND.ScPrd2.GT.1.E-10) THEN
C         Continue search along face K2.
          KNgh = K2
C
        ELSE IF (ScPrd3.GE.ScPrd2.AND.ScPrd3.GT.1.E-10) THEN
C         Continue search along face K3.
          KNgh = K3
C
        ELSE
C         All scalar products are non positive. The point is inside
C         or on the triangle.
          IFFYWALK = NVvr
          RETURN
C
        END IF
C
C       Check whether the chosen neighbor is physical.
        IF (NghVvr(KNgh,NVvr).LE.0) THEN
          FOutCnv = .TRUE.
          IFFYWALK = NVvr
          RETURN
        END IF
C
      END DO
C
      WRITE (*,'(2A/A,2E14.6)') 
     &               ' Fatal: IFFYWALK caught in an endless loop in',
     &               ' search for',' X,Y=',X,Y
      STOP
C
      END












