      REAL FUNCTION AREA (MVert,X,Y)
C
C     Last update:
C     8Apr92, 14:24. (converted to SINGLE from DAREA1.)
C
C     Calculate the area of an arbitray polygon with vertices given
C     in counterclockwise sense evaluating the cross-product of vectors
C     along faces of subtriangles. Welcome to DOMINO software.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4          5         6        7
C
      DIMENSION X(*),Y(*)
C
      Area = 0.
      DX1 = X(2)-X(1)
      DY1 = Y(2)-Y(1)
C
      DO ISub = 2,MVert-1
C
        DX2 = X(ISub+1)-X(ISub)
        DY2 = Y(ISub+1)-Y(ISub)
C
        Area = Area+DX1*DY2-DX2*DY1
C
        DX1 = DX2
        DY1 = DY2
C
      END DO
C
      Area = .5*Area
C
      RETURN
      END
