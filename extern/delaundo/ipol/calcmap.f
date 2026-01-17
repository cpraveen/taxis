      SUBROUTINE CALCMAP ( XBeg, YBeg, XEnd, YEnd,
     &                     XMBeg, YMBeg, XMEnd, YMEnd,
     &                     XTrans, YTrans, ITrans,
     &                     Alpha, CosAlpha, SinAlpha,
     &                     XRotOrigin, YRotOrigin, IRot,
     &                     Scale, IScale,
     &                     DefXYMBeg, DefXYTrans, DefXYMEnd,
     &                     DefAlpha, DefXYRotOrigin,
     &                     DefScale, IVerbose )

C Last update
C
C
C 
C Calculate the coefficients for the translation, rotation and
C scaling operation on a straight line in that order. Welcome
C to Domino software.
C 
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT NONE
      DOUBLE PRECISION XBeg,YBeg,XEnd,YEnd,
     &                 XMBeg,YMBeg,XMEnd,YMEnd,Eps,
     &                 XTrans,YTrans,
     &                 CosAlpha,SinAlpha,Alpha,
     &                 Scale,
     &                 DX,DY,DXM,DYM,Length,LengthM,
     &                 XRotOrigin, YRotOrigin,
     &                 XFixed, YFixed, XMFixed, YMFixed,
     &                 Pi, RadAlpha
      INTEGER ITrans,IRot,IScale,
     &        DefXYMBeg, DefXYMEnd, DefXYTrans, DefAlpha, DefScale,
     &        DefXYRotOrigin, IVerbose
      PARAMETER ( Eps = 1.E-5 )
      Pi = 2.*ACOS( 0.D0 )

C     The only case that does not lead to a rotation calculation
C     resets IRot=0.
      IRot = 1

      IF ( DefXYRotOrigin .EQ. 1 ) THEN
C       Undefined rotation origin. Use XBeg, YBeg.
        XRotOrigin = XBeg
        YRotOrigin = YBeg
      END IF

      IF ( DefAlpha .NE. 1 ) THEN
C       A rotation is explicitely given.

        IF ( DefXYTrans .NE. 1 ) THEN
C         Translation given. This is easy.

        ELSE IF ( DefXYMBeg .NE. 1 ) THEN
C         Translate between beginnings of curves.
          XTrans = XMBeg - XBeg
          YTrans = YMBeg - YBeg
          
          IF ( DefXYMEnd .NE. 1 .AND. IVerbose .GT. 1 )
     &      WRITE (*,'(A/A)')
     &        ' WARNING: specifiying TRNVEC and NEWBEG and NEWEND ',
     &        '          overconstrains the problem. NEWEND ignored.'
            
        ELSE IF ( DefXYMEnd .NE. 1 ) THEN
C         Translate between ends of curves.
          XTrans = XMEnd - XEnd
          YTrans = YMEnd - YEnd

        ELSE
C         Zero translation.
          XTrans = 0
          YTrans = 0

        END IF
            
      ELSE
C       Try to calculate a rotation. For this we need a rotation
C       origin, either given or assumed, and a distant second point.
        
        IF ( DefXYTrans .NE. 1 ) THEN
C         Translation given

          IF ( DefXYRotOrigin .EQ. 1 ) THEN
C           Require an origin for the translation.
            WRITE (*,'(A/A)')
     &            'FATAL: Either of origin of rotation ROTORG or',
     &            '       rotation angle ROTALF is required.'
            STOP

          ELSE IF ( DefXYMBeg .NE. 1 ) THEN
C           Fix new curve beginning.
            XFixed = XBeg
            YFixed = YBeg
            XMFixed = XMBeg
            YMFixed = YMBeg
            
            IF ( DefXYMEnd .NE. 1 .AND. IVerbose .GT. 1 )
     &        WRITE (*,'(A/A)')
     &        ' WARNING: specifiying TRNVEC and NEWBEG and NEWEND ',
     &        '          overconstrains the problem. NEWEND ignored.'
            
          
          ELSE IF ( DefXYMEnd .NE. 1 ) THEN
C           Fix curve end.
            XFixed = XEnd
            YFixed = YEnd
            XMFixed = XMEnd
            YMFixed = YMEnd

          END IF

        ELSE IF ( DefXYMBeg .NE. 1 .AND. DefXYMEnd .NE. 1 ) THEN
C         Matching beginnings and ends given. Rotate around beginning.
          XTrans = XMBeg - XBeg
          YTrans = YMBeg - YBeg
          XRotOrigin = XBeg
          YRotOrigin = YBeg
          XFixed = XEnd
          YFixed = YEnd
          XMFixed = XMEnd
          YMFixed = YMEnd

        ELSE
C         Not enough info for a rotation. So, there will be none.
          IRot = 0
        END IF
      END IF

      IF ( IRot .EQ. 0 ) THEN
C       
      ELSE IF ( DefAlpha .NE. 1 ) THEN
C       Rotation angle given.
        IF ( ABS( Alpha ) .GT. Eps ) THEN
          RadAlpha = Alpha / 180.D0 * Pi
          CosAlpha = COS( RadAlpha )
          SinAlpha = SIN( RadAlpha )
        END IF
        
      ELSE
C       Calculate a rotation..
C       Angle between the original line and the mapped one.
        DX = XFixed - XRotOrigin 
        DY = YFixed - YRotOrigin
        Length = SQRT( DX**2 + DY**2 )
        DXM = XMFixed - ( XRotOrigin + XTrans )
        DYM = YMFixed - ( YRotOrigin + YTrans )
        LengthM = SQRT( DXM**2 + DYM**2 )
        CosAlpha = ( DX*DXM + DY*DYM )/Length/LengthM
        SinAlpha = ( DX*DYM - DY*DXM )/Length/LengthM

        IF ( ABS( 1. - CosAlpha**2+SinAlpha**2 ) .GT. Eps )
     &    WRITE ( *,'(2A/A,E12.7)')
     &          ' WARNING: Rotation angle ROTANG might be',
     &          ' calculated inaccurately,',
     &          '          1-sin^2+cos^2=',1-CosAlpha**2+SinAlpha**2
        Alpha = 180. * ACOS( CosAlpha ) / Pi
      END IF
           
      IF ( DefScale .NE. 1 ) THEN
C       Scaling given.
        IScale = 1

      ELSE IF ( IRot.EQ.1 .AND. DefAlpha .NE. 1 ) THEN
C       There is a calculated rotation, so calculate a new scale.
        IScale = 1
        Scale = LengthM/Length
        
      END IF

C     Check values.
      ITrans = 1
      IF ( ABS( Alpha ) .LT. Eps )
     &     IRot = 0
      IF ( XTrans**2+YTrans**2 .LT. EPS**2 ) ITrans = 0
      IF ( ABS( 1. - Scale ) .LT. EPS .OR.
     &     ABS( Scale ) .LT. EPS ) IScale = 0

      RETURN
      END
