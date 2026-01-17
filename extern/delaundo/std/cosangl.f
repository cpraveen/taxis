      SUBROUTINE COSANGL (N1,N2,N3,N4,XNode,YNode,Cos1,Cos2,Cos3,Cos4,
     &                    Cos5,Cos6,Cos7,Cos8,Cos9,Cos10,Cos11,Cos12)
C
C Last update:
C 8Nov93,18:30;from INTEGER*2 to *4.
C 3Dec92,20:30;derived from cosangl for manip.
C 
C Calculate the cosines of the interior angles of the triangles 123,
C 243, 124 and 143. Welcome to DOMINO software.
C
C Input:
C N1/2/3/4: Number of the forming nodes of the quadrilateral.
C X/YNode:  Coordinates of the nodes.
C
C Output:
C Cos1/..:  Cosines of the interior angles of the triangles.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      DOUBLE PRECISION XNode(*),YNode(*)
C
      DX21 = XNode(N2)-XNode(N1)
      DY21 = YNode(N2)-YNode(N1)
      D21 = SQRT(DX21**2+DY21**2)
      DX31 = XNode(N3)-XNode(N1)
      DY31 = YNode(N3)-YNode(N1)
      D31 = SQRT(DX31**2+DY31**2)
      DX32 = XNode(N3)-XNode(N2)
      DY32 = YNode(N3)-YNode(N2)
      D32 = SQRT(DX32**2+DY32**2)
C
C     Find the angle in triangle 123.
C     Angle at 3.
      Cos1 = (DX32*DX31+DY32*DY31)/D32/D31
C     Angle at 1.
      Cos2 = (DX21*DX31+DY21*DY31)/D21/D31
C     Angle at 2.
      Cos3 = -(DX21*DX32+DY21*DY32)/D21/D32
C
      IF (N4.GT.0) THEN
C       Look at other possible triangles with these four nodes.
C
        DX41 = XNode(N4)-XNode(N1)
        DY41 = YNode(N4)-YNode(N1)
        D41 = SQRT(DX41**2+DY41**2)
        DX42 = XNode(N4)-XNode(N2)
        DY42 = YNode(N4)-YNode(N2)
        D42 = SQRT(DX42**2+DY42**2)
        DX43 = XNode(N4)-XNode(N3)
        DY43 = YNode(N4)-YNode(N3)
        D43 = SQRT(DX43**2+DY43**2)
C
C       Find the angle in triangle 234.
C       Angle at 4.
        Cos4 = (DX43*DX42+DY43*DY42)/D43/D42
C       Angle at 2.
        Cos5 = (DX42*DX32+DY42*DY32)/D42/D32
C       Angle at 3.
        Cos6 = -(DX32*DX43+DY32*DY43)/D32/D43
C
C       Find the angle in triangle 124.
C       Angle at 4.
        Cos7 = (DX42*DX41+DY42*DY41)/D42/D41
C       Angle at 1.
        Cos8 = (DX21*DX41+DY21*DY41)/D21/D41
C       Angle at 2.
        Cos9 = -(DX21*DX42+DY21*DY42)/D21/D42
C
C       Angle in triangle 143.
C       Angle at 3.
        Cos10 = -(DX31*DX43+DY31*DY43)/D31/D43
C       Angle at 4.
        Cos11 = (DX41*DX43+DY41*DY43)/D41/D43
C       Angle at 1.
        Cos12 = (DX41*DX31+DY41*DY31)/D41/D31
C
      END IF
C
      RETURN
      END
