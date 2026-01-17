      SUBROUTINE GETDEFAULTS ( XBegin, YBegin, XFinish, YFinish,
     &                         XCenter, YCenter, Radius,
     &                         AlphaBegin, AlphaFinish,
     &                         PiBegin, PiFinish, SinePower,
     &                         PolyCoeff, MNdeCrv, MNdePts, RhoIntOrig,
     &                         MIter, LvlBlend, IClosed, KNdeCF,KNdeCL,
     &                         NmeCurvNew, NmeFNew, NmeLNew, OutTypeNew,
     &                         MNotConNew )

C
C Last update:
C
C Initialize default values.
C     
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT NONE
      DOUBLE PRECISION XBegin, YBegin, XFinish, YFinish,
     &                         XCenter, YCenter, Radius,
     &                         AlphaBegin, AlphaFinish,
     &                         PiBegin, PiFinish,
     &                         SinePower,
     &                         PolyCoeff(0:*)
      REAL RhoIntOrig(0:*)
      INTEGER MNdeCrv, MNdePts, MIter, KNdeCF, KNdeCL, NmeCurvNew,
     &        NmeFNew, NmeLNew, OutTypeNew, MNotConNew, LvlBlend,
     &        IClosed

      XBegin = 0.
      YBegin = 0.
      XFinish = 1.
      YFinish = 1.

      XCenter = 0.
      YCenter = 0.
      Radius = 1.
      AlphaBegin = 0.
      AlphaFinish = 180.
      PiBegin = 0.
      PiFinish = 1.

      PolyCoeff(0) = 1.
      PolyCoeff(1) = 0.
      PolyCoeff(2) = 0.
      PolyCoeff(3) = 0.
      PolyCoeff(4) = 0.

      SinePower = 1.

      RhoIntOrig(0) = .1
      RhoIntOrig(1) = .1

      MNdeCrv = 100
      MNdePts = 0
      MIter = 30
      LvlBlend = 2
      IClosed = 0

      KNdeCF = 1
      KNdeCL = 99999

      NmeCurvNew = 1
      NmeFNew = 1
      NmeLNew = 1
      OutTypeNew = 1
      MNotConNew = 0

      RETURN
      END

C-----------------------------------------------------------------------

      SUBROUTINE MAPDEFAULTS ( DefXYNewBegin, DefXYNewFinish, 
     &                         DefXYTrans, ITrans,
     &                         DefAlpha, DefXYRotOrigin, IRot,
     &                         DefScale, IScale, IReflect )
C     
C Last update:
C
C Initialize default values for mapping.
C     
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT NONE
      INTEGER DefXYNewBegin, DefXYNewFinish, 
     &        DefXYTrans, ITrans,
     &        DefAlpha, DefXYRotOrigin, IRot,
     &        DefScale, IScale, IReflect

      DefXYNewBegin  = 1
      DefXYNewFinish = 1
      DefXYTrans     = 1
      DefAlpha       = 1
      DefXYRotOrigin = 1
      DefScale       = 1
      
      ITrans   = 0
      IRot     = 0
      IScale   = 0
      IReflect = 0
      
      RETURN
      END
      

C-----------------------------------------------------------------------

      SUBROUTINE CRVDEFAULTS ( DefXYBegin, DefXYFinish, DefXYThird,
     &                         DefXYCenter, DefAlphaBeginFinish,
     &                         DefPiBeginFinish, DefSinePower,
     &                         DefRadius, DefPolyCoeff, MPolyCoeff )
C
C Last update:
C
C Initialize default values for curves.
C     
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT NONE
      INTEGER DefXYBegin, DefXYFinish, DefXYCenter,
     &        DefAlphaBeginFinish, DefPiBeginFinish, 
     &        DefSinePower,
     &        DefRadius, DefPolyCoeff(*), MPolyCoeff,
     &        NCoeff, DefXYThird

      DefXYBegin             = 1
      DefXYFinish            = 1
      DefXYThird             = 1
      DefXYCenter            = 1
      DefAlphaBeginFinish    = 1
      DefPiBeginFinish       = 1
      DefSinePower           = 1
      DefRadius              = 1

      DO NCoeff = 1,MPolyCoeff
        DefPolyCoeff(NCoeff) = 1
      END DO

      RETURN
      END
      

      
      
