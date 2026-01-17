      SUBROUTINE LINE ( XBegin, YBegin, XFinish, YFinish,
     &                  MNde, MNdeEff, XNode, YNode )

C Last update
C
C
C Draw a line with Mnde points.
C 
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT NONE
      DOUBLE PRECISION XBegin, XFinish, YBegin, YFinish,
     &                 XNode(*), YNode(*), DX, DY
      INTEGER MNde, MNdeEff, NNde

      DX = ( XFinish - XBegin ) / ( MNde - 1 )
      DY = ( YFinish - YBegin ) / ( MNde - 1 )

      DO NNde = 0,MNde-1
        XNode(NNde+1) = XBegin + NNde * DX
        YNode(NNde+1) = YBegin + NNde * DY
      END DO

      MNdeEff = MNde

      RETURN
      END

C-----------------------------------------------------------------------
      
      SUBROUTINE CIRCLE ( XCenter, YCenter, DefXYCenter,
     &                    AlphaBegin, AlphaFinish, DefAlphaBeginFinish,
     &                    PiBegin, PiFinish, DefPiBeginFinish,
     &                    Radius, DefRadius,
     &                    XBegin, YBegin, DefXYBegin,
     &                    XFinish, YFinish, DefXYFinish,
     &                    XThird, YThird, DefXYThird,
     &                    MNde, MNdeEff, XNode, YNode )

C Last update
C
C
C Draw a circle with MNde points.
C 
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT NONE
      DOUBLE PRECISION XCenter, YCenter, AlphaBegin, AlphaFinish,
     &                 PiBegin, PiFinish,
     &                 Radius, XNode(*), YNode(*),
     &                 DRad, Pi, RadNNde, RadBegin, RadFinish,
     &                 XBegin, YBegin, YFinish, XFinish,
     &                 XThird, YThird, RadThird
      INTEGER MNde, MNdeEff, NNde, Direction,
     &        DefXYCenter, DefAlphaBeginFinish,
     &        DefPiBeginFinish, DefRadius,
     &        DefXYBegin, DefXYFinish, DefXYThird
      DOUBLE PRECISION XNor12, YNor12, RNor12,
     &                 XVec13, YVec13, RVec13, XYPrd1,
     &                 XNor23, YNor23, XYPrd2, RNor13
      Pi = 2. * ACOS(0.)

      IF ( DefXYCenter .NE. 1 .AND. DefAlphaBeginFinish .NE. 1 ) THEN
C       Everything given in terms of angles.
        RadBegin = AlphaBegin / 180.* Pi
        RadFinish = AlphaFinish / 180. * Pi

      ELSE IF ( DefXYCenter .NE. 1 .AND.
     &          DefPiBeginFinish .NE. 1 ) THEN
C       Everything given in terms of radians.
        RadBegin = PiBegin * Pi
        RadFinish = PiFinish* Pi

      ELSE IF ( DefXYBegin .NE. 1 .AND. DefXYFinish .NE. 1 .AND.
     &          DefXYThird .NE. 1 ) THEN
C       3 Points on the circle given. Calculate radius, origin, etc.

C       Normal on 1-2.
        XNor12 = YBegin-YThird
        YNor12 = XThird-XBegin
        RNor12 = DSQRT(XNor12**2+YNor12**2) 
C       
C       Vector 1-3.
        XVec13 = XFinish-XBegin
        YVec13 = YFinish-YBegin
        RVec13 = DSQRT(XVec13**2+YVec13**2)
C       
C       Scalar product of Nor12 and Vec13.
        XYPrd1 = XNor12*XVec13+YNor12*YVec13
C       
        IF ( ABS( XYPrd1 ) .LE. 1.D-10*RNor12*RVec13 ) THEN
C         Degenerate case: Three nodes on a line.
          WRITE (*,'(2A)')
     &          'FATAL: the 3 points given for the circle are',
     &          ' collinear.'
          STOP
C         
        ELSE
C         Normal on side 2-3.
          XNor23 = YThird-YFinish
          YNor23 = XFinish-XThird
C         
C         Calculate scalar product of the normal on 1-2 and the
C         normal on 2-3.
          XYPrd2 = XNor12*XNor23+YNor12*YNor23
C         
C         Calculate the length of the normal from side 1-3 to the
C         center of the circumcircle.
          RNor13 = .5*(XYPrd2/XYPrd1)
C         
C         Circumcenter, radius.
          XCenter = .5*( XBegin + XFinish ) - RNor13 * YVec13
          YCenter = .5*( YBegin + YFinish ) + RNor13 * XVec13
          Radius = SQRT( ( XCenter - XBegin )**2 +
     &                   ( YCenter - YBegin )**2 )

C         Circumference of the ends of the arc.
          RadBegin = ATAN2( YBegin - YCenter, XBegin - XCenter )
          RadFinish = ATAN2( YFinish - YCenter, XFinish - XCenter )
          RadThird = ATAN2( YThird - YCenter, XThird - XCenter )

          IF ( (RadThird-RadBegin)*(RadThird-RadFinish) .GT. 0. ) THEN
C           XYThird does not lie between XYBegin and XYFinish with
C           increasing arclength. Go the other way round.
            RadBegin = RadBegin + 2.*Pi
          END IF
C         
        END IF

      ELSE
        WRITE (*,'(A)')
     &        'FATAL: Not enough information to calculate a circle.'
        STOP
      END IF
C
      IF ( RadFinish .GT. RadBegin ) THEN
        DRad = ( RadFinish-RadBegin ) / ( MNde - 1 )
        Direction = 1
      ELSE IF ( AlphaBegin .EQ. AlphaFinish ) THEN
        RadFinish = RadFinish + 2.*Pi
        DRad = 2.*Pi / ( MNde - 1 )
        Direction = 1
      ELSE
        DRad = ( RadFinish-RadBegin ) / ( MNde - 1 )
        Direction = -1
      END IF
                
      DO NNde = 0,MNde-1
        RadNNde = RadBegin + NNde * DRad
        XNode(NNde+1) = XCenter + Radius * COS( RadNNde )
        YNode(NNde+1) = YCenter + Radius * SIN( RadNNde )
      END DO

      IF ( DefXYFinish .NE. 1 ) THEN
        XNode(MNde) = XFinish
        YNode(MNde) = YFinish
      END IF

      MNdeEff = MNde

      RETURN
      END
      
C-----------------------------------------------------------------------

      SUBROUTINE POLYNOM ( XBegin, XFinish, A,
     &                     MNde, MNdeEff, XNode, YNode )

C Last update
C
C
C Draw a polynomial from XBegin to XFinish with MNde points.
C 
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT NONE
      DOUBLE PRECISION XBegin, XFinish, A(0:4),
     &                 XNode(*), YNode(*), DX, DY, D, DT, 
     &                 CurveLenNew, CurveLenOld, XHi, YHi, XLo, YLo, F,
     &                 DXStep, Secant, X, S, DTEff, EPS
      INTEGER MNde, MNdeEff, NNde, MIntervals, NInterval, MAXINTERVALS
      PARAMETER ( EPS = 1.E-5, MAXINTERVALS = 1000 )
      F(X) = A(0) + X*( A(1) + X*( A(2) + X*( A(3) + A(4) ) ) ) 
      S(X) = ( ( 4*A(4)*X +3*A(3) )*X + 2*A(2) )*X + A(1)

C     Calculate the curve-length while doubling the number of
C     intervals until the result seems fine.
      CurveLenOld = -1.
      MIntervals = 1
      CurveLenNew = 0.
      DO WHILE ( ABS( CurveLenNew - CurveLenOld ) .GT. EPS/MNde .AND.
     &           MIntervals .LT. MAXINTERVALS )
        MIntervals = 2*MIntervals
        DXStep = ( XFinish - XBegin ) / MIntervals
        CurveLenOld = CurveLenNew
        CurveLenNew = 0.
        XHi = XBegin
        YHi = F( XHi )
        DO NInterval = 1,MIntervals
          XLo = XHi
          YLo = YHi
          IF ( NInterval .EQ. MIntervals ) THEN
            XHi = XFinish
          ELSE
            XHi = XBegin + NInterval * DXStep
          ENDIF
          YHi = F( XHi )
          DX = XHi - XLo
          DY = YHi - YLo
          D = SQRT( DX**2 + DY**2 )
          CurveLenNew = CurveLenNew + D 
        END DO
      END DO
      
C     Try to have this much curvelength between nodes.
      DT = CurveLenNew / ( MNde-1 )
C
      XNode(1) = XBegin
      YNode(1) = F( XBegin )
      CurveLenNew = 0.
      NNde = 1
      DO WHILE ( NNde .LT. MNde .AND.
     &           CurveLenNew .LT. 1. )
        NNde = NNde + 1
C       Approximate the gradient at X(NNde-1).
        X = XNode(NNde-1)
        DX = DT / SQRT( 1.+S(X)**2 )
        XNode(NNde) = X + DX
        YNode(NNde) = F( XNode(NNde) )
C       Approximate the gradient at X(NNde-1/2).
        DY = YNode(NNde) - YNode(NNde-1)
        Secant = DY / DX
        DX = DT / SQRT( 1.+Secant**2 )
        XNode(NNde) = X + DX
        YNode(NNde) = F( XNode(NNde) )
        DY = YNode(NNde) - YNode(NNde-1)
        DTEff = SQRT( DX**2 + DY**2 )
        CurveLenNew = CurveLenNew + DTEff
      END DO
      
      MNdeEff = NNde
      XNode(MNdeEff) = XFinish
      YNode(MNdeEff) = F( XFinish )

      RETURN
      END

C-----------------------------------------------------------------------
      
      SUBROUTINE SINE ( AlphaBegin, AlphaFinish, DefAlphaBeginFinish,
     &                  PiBegin, PiFinish, DefPiBeginFinish,
     &                  Radius, DefRadius,
     &                  SinePower, DefSinePower,
     &                  MNde, MNdeEff, XNode, YNode )

C Last update
C
C
C Draw a sine curve with amplitude radius and MNde points.
C 
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT NONE
      DOUBLE PRECISION AlphaBegin, AlphaFinish,
     &                 PiBegin, PiFinish,
     &                 Radius, SinePower, XNode(*), YNode(*),
     &                 DRad, Pi, RadNNde, RadBegin, RadFinish,
     &                 RAD, Alpha
      INTEGER MNde, MNdeEff, NNde,
     &        DefAlphaBeginFinish, DefPiBeginFinish,
     &        DefRadius, DefSinePower
      RAD(Alpha) = Alpha / 180.D0 * Pi
      Pi = 2. * ACOS(0.)

      IF ( DefAlphaBeginFinish  .EQ. 1 .AND.
     &     DefPiBeginFinish .EQ. 1 ) THEN
        WRITE (*,'(A)')
     &        ' FATAL: Not enough information for sine-curve given.'
        STOP
        
      ELSE IF ( DefAlphaBeginFinish .NE. 1 ) THEN
C       Convert degrees into radian.
        RadBegin = RAD( AlphaBegin )
        RadFinish = RAD( AlphaFinish )

      ELSE IF ( DefPiBeginFinish .NE. 1 ) THEN
C       Convert units of pi into radian.
        RadBegin = PiBegin * Pi
        RadFinish = PiFinish * Pi

      END IF
      DRad = ( RadFinish - RadBegin ) / ( MNde - 1 )

      IF ( DefSinePower .NE. 1 ) THEN
C       Apply the exponent.
        DO NNde = 0,MNde
          RadNNde = RadBegin + NNde * DRad
          XNode(NNde+1) = RadNNde / Pi
          YNode(NNde+1) = Radius * ( SIN( RadNNde ) )**SinePower
        END DO
        XNode(MNde) = RadFinish/Pi
        YNode(NNde) = Radius * ( SIN( RadFinish ) )**SinePower

      ELSE
C       No exponent.
        DO NNde = 0,MNde
          RadNNde = RadBegin + NNde * DRad
          XNode(NNde+1) = RadNNde / Pi
          YNode(NNde+1) = Radius * SIN( RadNNde ) 
        END DO
        XNode(MNde) = RadFinish/Pi
        YNode(NNde) =  Radius * SIN( RadFinish )

      END IF
          
      MNdeEff = MNde

      RETURN
      END
      
C-----------------------------------------------------------------------

      SUBROUTINE NACA ( MNde, CharCoeff, MNdeEff, X, Y )

C Last update
C
C
C Draw a 4-digit NACA profile with Mnde points. The airfoil is closed
C at the trailing edge by fudging with the coefficient of x**4. The
C points are distributed on the airfoil with a cosine distribution.
     
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT NONE
      DOUBLE PRECISION X(*), Y(*),
     &                 CMax, CLoc, Thck,
     &                 Pi, DTh, Th, Yc, Yt
      INTEGER MNde, MNdeEff, NNde, KPos, I0, I9, ILetter(4),
     &        KLetter, K, I, MInt
      CHARACTER CharCoeff*80
      LOGICAL FLetter(4)

      I0 = ICHAR('0')
      I9 = ICHAR('9')
C     Check consecutiveness of the ASCII table.
      IF (I9-I0.NE.9) THEN
        WRITE (*,'(/A)') ' FATAL: character table screwed up. Sorry.'
        STOP
      END IF

C     Get the coefficients.
      KPos = 1
      DO WHILE ( CharCoeff(KPos:KPos) .EQ. ' '.AND. KPos.LT.70 )
        KPos = KPos+1
      END DO
        
      IF ( KPos .LT. 70 ) THEN
        DO KLetter = 1,4
          K = KPos - 1 + KLetter
          I = ICHAR( CharCoeff(K:K) )
          FLetter(KLetter) = .FALSE.
          IF ( I .GE. I0 .AND. I .LE. I9 ) THEN
            ILetter(KLetter) = I - I0
            FLetter(KLetter) = .TRUE.
          END IF
        END DO
      END IF

      IF ( FLetter(1) .AND. FLetter(2) .AND.
     &     FLetter(3) .AND. FLetter(4) ) THEN
        CMax = .1*ILetter(1)
        CLoc = .1*ILetter(2)
        Thck = .1*ILetter(3) + .01*ILetter(4)
      ELSE
        WRITE (*,'(A/2A/A)')
     &        ' WARNING: NACA airfoil coefficients not parseable:',
     &        '          ',CharCoeff,
     &        '          you''ll get a NACA0012.'
        CMax = .0
        CLoc = .0
        Thck = .12
      END IF

C     Make MNde even.
      MInt = 2*INT(MNde/2.)
      MNdeEff = MInt+1
      
C     Coordinates.
      Pi = 2.*Acos(0.)
      Dth = 2.*Pi / MInt
      X(1) = 1.
      Y(1) = 0.

      DO NNde = 2,MInt/2
        Th = Dth*(NNde-1)
        X(NNde) = .5+.5*COS(Th)
        X(Mint-NNde+2) = X(NNde)
        IF (X(NNde).lt.CLoc) THEN
          Yc = CMax/CLoc*CLoc*(2.*CLoc*X(NNde)-X(NNde)**2)
        ELSE
          Yc = CMax/(1.-CLoc)**2*((1.-2.*CLoc)+2.*CLoc*X(NNde)-
     &                            X(NNde)**2)
        END IF
        Yt = 5.*Thck*(.2969*Sqrt(X(NNde))-.1260*X(NNde)-
     &                .3516*X(NNde)**2+.2843*X(NNde)**3-
     &                .1036*X(NNde)**4)
C       This is the original NACA coefficient:
C     &                .1015*X(NNde)**4)
        Y(NNde) = Yc-Yt
        Y(Mint-NNde+2) = Yc+Yt

      End Do
      X(MInt/2+1) = 0.
      Y(MInt/2+1) = 0.
      X(MInt+1) = 1.
      Y(MInt+1) = 0.

      RETURN
      END
      
