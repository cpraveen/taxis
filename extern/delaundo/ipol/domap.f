      SUBROUTINE DOMAP (X,Y,
     &                  XRotOrigin,YRotOrigin,
     &                  IReflect,
     &                  XTrans,YTrans,ITrans,
     &                  CosAlpha,SinAlpha,IRot,
     &                  Scale,IScale,
     &                  XMap,YMap)

C Last update
C
C
C 
C Map X,Y onto XMap,YMap performing a translation, a rotation and a
C scaling operation in that order. Welcome to Domino software.
C 
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT NONE
      INTEGER IReflect, ITrans, IRot, IScale
      DOUBLE PRECISION X,Y,XMap,YMap,
     &                 XTrans,YTrans,Scale,
     &                 X1,X2,Y1,Y2,
     &                 XRotOrigin, YRotOrigin, CosAlpha, SinAlpha

      IF ( IReflect .EQ. 1 ) THEN
C       Do a reflection around the x-axis.
        XMap = X
        YMap = -Y
      ELSE
C       Do nothing.
        XMap = X
        YMap = Y
      END IF
C
      IF ( ITrans .NE. 0 .OR. IRot .NE. 0 .OR. IScale .NE. 0 ) THEN
C       Do a rotation, scaling, translation  operation.        
C       Distance from the origin.
        X1 = X - XRotOrigin
        Y1 = Y - YRotOrigin

        IF ( IRot .EQ. 1 ) THEN
C         Rotation.
          X2 = CosAlpha*X1 - SinAlpha*Y1
          Y2 = SinAlpha*X1 + CosAlpha*Y1
        ELSE
          X2 = X1
          Y2 = Y1
        END IF
        
C       Scaling.
        IF ( IScale .EQ. 1 ) THEN
          X2 = Scale * X2
          Y2 = Scale * Y2
        END IF
        
C       Translate to the origin of the map.
        XMap = X2 + XTrans + XRotOrigin
        YMap = Y2 + YTrans + YRotOrigin

      END IF

      RETURN
      END

