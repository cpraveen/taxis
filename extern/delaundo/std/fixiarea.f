      SUBROUTINE FIXIAREA (NBVvr,NBFrmNde,NghBVvr,MBVvr,BkgStr,IArea,
     &                     XNode,YNode,IVerbose,NtLog,FLogFile)
C
C Last update:
C 30Oct93,17:30; intro IVerbose.
C 3Dec92,22:58;conceived.
C
C Find an IArea value for a triangle that specifies stretching but
C is not assigned to any frontal element. Welcome to DOMINO software.
C
C Input:
C NBVvr:    Number of the triangle to find a IArea for.
C NBFrmNde: Array of forming nodes of the background triangles.
C NghBVvr:  Array of neighbors of the background triangles.
C MBVvr:    Number of triangles in the background grid.
C BkgStr:   Vector of stretching values at the nodes of the background
C           grid.
C IArea:    Vector of pointers to stretching areas for all triangles.
C X/YNode:  Coordinates of the nodes.
C IVerbose: Warnings only given for IVerbose = 5.
C NtLog:    Logical unit of the logfile.
C FLogFile: .T. if output to logfile shall be written.
C
C Output:
C IArea:    Vector of pointers to stretching areas for all triangles.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      DOUBLE PRECISION XNode(*),YNode(*)
      DIMENSION NBFrmNde(3,*),NghBVvr(3,*),BkgStr(*),IArea(*)
C
C     Walk in the direction of the gradient across the background
C     grid, i.e. pick the side with the highest average gradient,
C     until a cell with a given IArea is found.
      NBWalk = NBVvr
      MChk = 0
      DO WHILE (IArea(NBWalk).EQ.0.AND.MChk.LT.MBVvr)
        MChk = MChk+1
C       Forming nodes.
        NB1 = NBFrmNde(1,NBWalk)
        NB2 = NBFrmNde(2,NBWalk)
        NB3 = NBFrmNde(3,NBWalk)
C       Stretching values at the nodes.
        BS1 = BkgStr(NB1)
        BS2 = BkgStr(NB2)
        BS3 = BkgStr(NB3)
C       Coordinates.
        X1 = XNode(NB1)
        X2 = XNode(NB2)
        X3 = XNode(NB3)
        Y1 = YNode(NB1)
        Y2 = YNode(NB2)
        Y3 = YNode(NB3)
C       Coordinate differences.
        X21 = X2-X1
        X32 = X3-X2
        X13 = X1-X3
        Y21 = Y2-Y1
        Y32 = Y3-Y2
        Y13 = Y1-Y3
C       Calculate the area.
        Area = .5*(X32*Y13-X13*Y32)
C       Calculate the gradient in linear stretching.
        GradBSX = -.5*(Y32*BS1+Y13*BS2+Y21*BS3)/Area
        GradBSY = .5*(X32*BS1+X13*BS2+X21*BS3)/Area
C       Calculate the dot products of outward normals and gradient.
        ScPrd1 = Y32*GradBSX-X32*GradBSY
        ScPrd2 = Y13*GradBSX-X13*GradBSY
        ScPrd3 = Y21*GradBSX-X21*GradBSY
C
C       Walk in the direction of the largest scalar product.
        IF (ScPrd1.GE.ScPrd2.AND.ScPrd1.GE.ScPrd3) THEN
          NBWalk = NghBVvr(1,NBWalk)
        ELSE IF (ScPrd2.GE.ScPrd3) THEN
          NBWalk = NghBVvr(2,NBWalk)
        ELSE 
          NBWalk = NghBVvr(3,NBWalk)
        END IF
C
      END DO
C
      IF (IArea(NBWalk).EQ.0.) THEN
        IF (IVerbose.GT.4) 
     &    WRITE (*,'(A,I7,A/A,3(I7,A))')
     &    ' WARNING: No IArea could be found for NBVvr',NBVvr,',',
     &    '          formed by:',(NBFrmNde(I,NBVvr),',',I=1,3)
        IF (FLogFile) 
     &    WRITE (NtLog,'(A,I7,A/A,3(I7,A))')
     &    ' WARNING: No IArea could be found for NBVvr',NBVvr,',',
     &    '          formed by:',(NBFrmNde(I,NBVvr),',',I=1,3)
      ELSE
        IArea(NBVvr) = IArea(NBWalk)
      END IF
C
      RETURN
      END

