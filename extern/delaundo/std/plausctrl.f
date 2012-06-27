      SUBROUTINE PLAUSCTRL (NtLog,IVerbose,FLogFile,
     &                      FIntNde,FConNde,SpcGrdRt,DTol,QTol,BTol,
     &                      FStretch,DltStar,BndStr,ISmooth,FFlatSwap,
     &                      CharOutType,CharOutForm,
     &                      Beta,RelFac,MLevel,
     &                      FDFIntNde,FDFConNde,FDSpcGrdRt,
     &                      FDISmooth,FDFFlatSwap,FDRelFac,FDMLevel)
C
C Last update:
C 12July94,18:20; cut out of READCTRL.
C
C Perform some plausibility tests and eventual corrections on the
C user input in the .ctr file. Note that these defaults are set in
C defctrl.f. For the sake of simplicity, there is no communication
C between the two routines about their default settings other than
C your favorite editor. Welcome to Domino software.
C
C I/O: see READCTRL.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
      PARAMETER (MaxCharTxt=20)
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      CHARACTER CharOutType*1,CharOutForm*1
C
C     Plausibility tests.
      IF (FConNde.AND.FIntNde) THEN
        FConNde = .FALSE.
        FDConNde = .TRUE.
        IF (IVerbose.GT.2) WRITE (*,1) FConNde
        IF (FLogFile) WRITE (NtLog,1) FConNde
 1      FORMAT (
     &  ' WARNING: You may not specify NODEUSe and NODECOnstruct.'/
     &  '          Use type 3 boundaries for your interior specified',
     &             ' nodes'/
     &  '          NODEUSe set to:',L2,'.')
      END IF
C
      IF (FConNde.AND..NOT.FStretch.AND.ISmooth.GT.0) THEN
        ISmooth = 0
        FDISmooth = .TRUE.
        IF (IVerbose.GT.2) WRITE (*,2) ISmooth
        IF (FLogFile) WRITE (NtLog,2) ISmooth
 2      FORMAT (
     &  ' WARNING: You may not specify a nonzero ISMOOTh without.'/
     &  '          setting STRETChing to .TRUE.'/
     &  '          ISMOOTh set to',I3,'.')
      END IF
C
      IF (SpcGrdRt.LT.0.) THEN
        SpcGrdRt = 1.
        FDSpcGrdRt = .TRUE.
        IF (IVerbose.GT.2) WRITE (*,3) SpcGrdRt
        IF (FLogFile) WRITE (NtLog,3) SpcGrdRt
 3      FORMAT (
     &  ' WARNING: You may not specify a negative value for SPCRAT.'/
     &  '          SPCRAT set to',F5.1,'.')
      END IF
C
      IF (BTol.LT.0.) THEN
        BTol = 2.
        FDBTol = .TRUE.
        IF (IVerbose.GT.2) WRITE (*,4) BTol
        IF (FLogFile) WRITE (NtLog,4) BTol
 4      FORMAT (
     &  ' WARNING: You may not specify a negative value for BTOLER.'/
     &  '           set to',F5.1,'.')
      END IF
C
      IF (QTol.LT.0.) THEN
        QTol = .65
        FD = .TRUE.
        IF (IVerbose.GT.2) WRITE (*,5) QTol
        IF (FLogFile) WRITE (NtLog,5) QTol
 5      FORMAT (
     &  ' WARNING: You may not specify a negative value for QTOLER.'/
     &  '          QTOLER set to',F5.3,'.')
      END IF
C
      IF (RELFAC.LT.0.) THEN
        RelFac = .75
        FDRelFac = .TRUE.
        IF (IVerbose.GT.2) WRITE (*,6) RelFac
        IF (FLogFile) WRITE (NtLog,6) RelFac
 6      FORMAT (
     &  ' WARNING: You may not specify a negative value for RELFAC.'/
     &  '          RELFAC set to',F5.3,'.')
      END IF
C
      IF (QTol.GT.1.) THEN
        IF (IVerbose.GT.4) WRITE (*,7)
        IF (FLogFile) WRITE (NtLog,7)
 7      FORMAT (
     &  ' WARNING: You specified a value > 1 for QTOLER.'/
     &  '          This might get you in trouble.'/
     &  '          You do so at your own risk.')
      END IF
C
      IF (DTol.GT.1.) THEN
        IF (IVerbose.GT.4) WRITE (*,8)
        IF (FLogFile) WRITE (NtLog,8)
 8      FORMAT (
     &  ' WARNING: You specified a value > 1 for DTOLER.'/
     &  '          This might get you in trouble.'/
     &  '          You do so at your own risk.')
      END IF
C
      IF (Beta.GT.1..OR.Beta.LT.0.) THEN
        IF (IVerbose.GT.4) WRITE (*,16)
        IF (FLogFile) WRITE (NtLog,16)
 16     FORMAT (
     &  ' WARNING: You specified a value not in [0,1] for LPBETA.'/
     &  '          This might get you in trouble.'/
     &  '          You do so at your own risk.')
      END IF
C
      IF (RelFac.GT.1.) THEN
        IF (IVerbose.GT.4) WRITE (*,17)
        IF (FLogFile) WRITE (NtLog,17)
 17     FORMAT (
     &  ' WARNING: You specified a value > 1 for RELFAC.'/
     &  '          This might get you in trouble.'/
     &  '          You do so at your own risk.')
      END IF
C
      IF (FStretch.AND.MLevel.GT.1) THEN
        MLevel = 1
        FDFMLevel = .TRUE.
        IF (IVerbose.GT.2) WRITE (*,9) MLevel
        IF (FLogFile) WRITE (NtLog,9) MLevel
 9      FORMAT (
     &  ' WARNING: You may not specify MLEVEL > 1 and STRETC.'/
     &  '          Only the finest grid level is created. '/
     &  '          MLEVEL set to',I3,'.')
      END IF
C
      IF (FConNde.AND.CharOutType.EQ.'b') THEN
        FConNde = .FALSE.
        FDFConNde = .TRUE.
        IF (IVerbose.GT.2) WRITE (*,10) FConNde
        IF (FLogFile) WRITE (NtLog,10) FConNde
 10     FORMAT (
     &  ' WARNING: No interior nodes are constructed for the ',
     &             ' background grid'/
     &  '          NODEUSe set to',L2,'.')
      END IF
C
      IF (FConNde.AND.CharOutType.EQ.'i') THEN
        FConNde = .FALSE.
        FDFConNde = .TRUE.
        IF (IVerbose.GT.2) WRITE (*,11) FConNde
        IF (FLogFile) WRITE (NtLog,11) FConNde
 11     FORMAT (
     &  ' WARNING: You may not specify NODECOnstruct and OUTTYPe i.'/
     &  '          No interior nodes are constructed for the ',
     &             ' initial triangulation,'/
     &  '          NODEUSe set to:',L2,'.')
      END IF
C
      IF (FFlatSwap.AND.CharOutType.EQ.'b') THEN
        FFlatSwap = .FALSE.
        FDFFlatSwap = .TRUE.
        IF (IVerbose.GT.2) WRITE (*,12) FFlatSwap
        IF (FLogFile) WRITE (NtLog,12) FFlatSwap
 12     FORMAT (
     &  ' WARNING: You may not specify FLATSWap and OUTTYPe b.'/
     &  '          No swapping is done for the background grid'/
     &  '          FLATSWap set to:',L2,'.')
      END IF
C
      IF (FFlatSwap.AND.CharOutType.EQ.'i') THEN
        FFlatSwap = .FALSE.
        FDFFlatSwap = .TRUE.
        IF (IVerbose.GT.2) WRITE (*,13) FFlatSwap
        IF (FLogFile) WRITE (NtLog,13) FFlatSwap
 13     FORMAT (
     &  ' WARNING: You may not specify FLATSWap and OUTTYPe i.'/
     &  '          No swapping is done for the initial triangulation,'/
     &  '          FLATSWap set to:',L2,'.')
      END IF
C
      IF (CharOutType.EQ.'b'.AND.
     &    (CharOutForm.EQ.'g'.OR.CharOutForm.EQ.'o')) THEN
        WRITE (*,14)
        IF (FLogFile) WRITE (NtLog,14)
 14     FORMAT (
     &  ' FATAL: Incompatible OUTTYPe: b and OUTFORMat: g or o.'/
     &  '        Sorry, but use .dpl or .ucd formats to see the',
     &             ' background grid.'/)
        STOP
      END IF
C
      IF (CharOutType.EQ.'q'.AND.CharOutForm.EQ.'u') THEN
        WRITE (*,15)
        IF (FLogFile) WRITE (NtLog,15)
 15     FORMAT (
     &  ' FATAL: Incompatible OUTTYPe: q and OUTFORMat: u.'/
     &  '        Sorry, but use .dpl or .vkb formats to see the',
     &             ' quadratic triangulation.'/)
        STOP
      END IF
C
      RETURN
      END
      
