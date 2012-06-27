      SUBROUTINE SUPCALC (DltStar,BndStr,DomStr,DMax,StrPw,
     &                    IVerbose,NtLog,FLogFile)
C
C Last update:
C 22May96; fall back on StrPw=1.
C 23Aug93,19:45;add wall distance as output.
C
C Calculate the exponent of a supercircle to match the altitude
C variation in the stretching layer.
C
C Input:
C DltStar: Thickness of the stretching layer.
C BndStr:  Maximum aspect ratio.
C DomStr:  Stretching value in the inviscid domain.
C DMax:    Longest frontal face.
C
C Output:
C StrPw    Power to use with the supercircle interpolation.
C     
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C     
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      REAL StrPw,P,StepLo,StepHi,StrLoc
      PARAMETER (MaxIter=100,Eps=1.E-3,StepLo=.1,StepHi=2.)
      U(UL,UR,XL,XR,X,P) = UR+(UL-UR)*(1-((X-XL)/(XR-XL))**P)**(1./P)
C
C     Calculate the size variation C and the number of cells in the
C     layer K.
      C = DMax/BndStr*(BndStr-1.)/DltStar+1.
      RK = LOG(BndStr)/LOG(C)
      K = NINT(RK)
C
C     Loop over all cell steps to average the exponent.
      EAv = 0.
      MAv = 0
      XLoc = DMax/BndStr
      XTotal = 0.
C
      IF (IVerbose.GE.3) 
     &WRITE (*,'(6X,A/10X,A)') ' The inner edge of the viscous cells:',
     &  ' No.  wall dist.     cell thk.   loc. stretching'
      IF (IVerbose.GE.3) 
     &WRITE (*,'(8X,I3,2F15.8,F15.3)') 1,0.,0.,BndStr
      IF (FLogFile) 
     &WRITE (NtLog,'(6X,A/10X,A)') 
     &  ' The inner edge of the viscous cells:',
     &  ' No.  wall dist.     cell thk.   loc. stretching'
      IF (FLogFile) 
     &WRITE (NtLog,'(8X,I3,2F15.8,F15.3)') 1,0.,0.,BndStr
C
      DO N = 2,K
C       Calculate the stretching.
        CNm1 = C**(N-1)
        StrLoc = BndStr/CNm1
        XTotal = XTotal+XLoc
        IF (IVerbose.GE.3) 
     &  WRITE (*,'(8X,I3,2F15.8,F15.3)') N,XTotal,XLoc,StrLoc
        IF (FLogFile) 
     &  WRITE (NtLog,'(8X,I3,2F15.8,F15.3)') N,XTotal,XLoc,StrLoc
C
        IF (XLoc.LT.1.) THEN
C         Find the supercicle exponent for this position.
          E1 = 1.
          U1 = U(BndStr,DomStr,0.,DltStar,XLoc,E1)
          IF ((U1-StrLoc)*(BndStr-DomStr).GT.0.) THEN
            E2 = .5
          ELSE
            E2 = 2.
          END IF
          U2 = U(BndStr,DomStr,0.,DltStar,XLoc,E2)
C
C         Iterate MaxIter times.
          MIter = 2
          DO WHILE (ABS(U2-StrLoc).GT.Eps.AND.MIter.LT.MaxIter)
            MIter = MIter+1
            EDiff = (U2-StrLoc)/(U2-U1)*(E1-E2)
            IF (E2.GT.1.) THEN
              EDiff = SIGN(AMIN1(StepHi,ABS(EDiff)),EDiff)
            ELSE 
              EDiff = SIGN(AMIN1(StepLo,ABS(EDiff)),EDiff)
            END IF
            ENew = AMAX1(0.,E2+EDiff)
            E1 = E2
            U1 = U2
            E2 = ENew
            U2 = U(BndStr,DomStr,0.,DltStar,XLoc,E2)
          END DO
C
C         If the solution was found, add it to the average.
          IF (ABS(U2-StrLoc).LE.Eps) THEN
            EAv = EAv+ENew
            MAv = MAv+1
          END IF
C
        END IF
C
C       Calculate the next cell interface position.
        XLoc = XLoc+DMax/BndStr*CNm1
C
      END DO
C
      IF (MAv.GT.0) THEN
        StrPw = EAv/MAv
      ELSE
        StrPw = 1.
        WRITE (*,99)
        IF (FLogFile) WRITE (NtLog,99)
 99     FORMAT ( ' WARNING: no value for StrPw could be found,'/
     &           '          StrPw set to 1.' )
      END IF
C
      RETURN
      END
