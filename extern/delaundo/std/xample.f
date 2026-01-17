      SUBROUTINE XAMPLE (IVerbose,NpFlNm,KHiNpFlNm,
     &                   FNpFlFrm,FNotCon,FStretch,DltStar,
     &                   BndStr,FFlatSwap,OpFlNm,KHiOpFlNm,
     &                   CharOutType,CharOutForm,
     &                   FDIVerbose,FDFStretch,FDDltStar,FDBndStr,
     &                   FDFFlatSwap,FDFNotCon)
C
C Last update:
C 19May96; new PATH.
C 21Sep94; fix spello. Thanx VKB.
C ; towards 4.0.
C
C Fill the control parameters necessary for Delaundo for an example.
C Welcome to DOMINO software.
C
C Input:
C
C Output:
C IVerbose:   Verbosity.
C NpFlNm:     Name of the .pts input file.
C KHiNpFlNm:  Length of the input file name.
C FNpFlFrm:   .T. if the input file is of formatted type.
C FNotCon:    .T. if the anti-connectivity-information is to be used.
C FStretch:   .T. if stretched layers are to be built.
C DltStar:    Thickness of the viscous layer.
C BndStr:     Maximum aspect ratio in the viscous layer.
C FFlatSwap:  .T. if swapping to minimize maximum angles is to be done.
C OpFlNm:     Name of the output file.
C KHiOpFlNm:  Length of the name of the output file.
C CharOutType:Type of the output.
C CharOutForm:Format of the output file.
C FDSomething:.T. if Something has been set to something other than
C             the default.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
      PARAMETER (MaxCharTxt=20)
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      CHARACTER*255 Char*1,CharXamplePath, EnvName,
     &          NpFlNm,OpFlNm,CharOutType*1,CharOutForm*1
     &          
C
C     Get the path for the examples.
      EnvName = 'DLNDO_XMPL'
      CALL PATH ( EnvName, CharXamplePath, KHiXamplePath, 255 )
C
      WRITE (*,'(/6X,2A/7X,A/)')
     &' The .ctr files and the .pts files for the examples',
     &' can be found in:',CharXamplePath(1:KHiXamplePath)
C
      Finished = .FALSE.
      DO WHILE (.NOT.Finished)
        WRITE (*,'(10(/6X,A))')
     &  ' Choose from the following examples:',
     &  ' simple grid to demonstrate a .dpl file           ''d'',',
     &  ' isotropic node generation around NACA 0012       ''n'',',
     &  ' 1:10 stretching around a three element aerofoil  ''t'',',
     &  ' 1:1000 stretching around a NACA 0012 with wake   ''w'',',
     &  ' return to DELAUNDO                               ''x'',',
     &  ' or stop                                          ''s''?'
        READ (*,'(A)') Char
C
        IF (Char.EQ.'x'.OR.Char.EQ.'X') THEN
          RETURN
C
        ELSE IF (Char.EQ.'s'.OR.Char.EQ.'S') THEN
          STOP
C
        ELSE IF (Char.EQ.'d'.OR.Char.EQ.'D') THEN
C         test.pts
          Finished = .TRUE.
          IVerbose = 5
          FDIVerbose = .FALSE.
          NpFlNm = CharXamplePath(1:KHiXamplePath)//'test.pts'
          FNpFlFrm = .TRUE.
          OpFlNm = './test.dpl'
          CharOutType = 't'
          CharOutForm = 'd'
C
        ELSE IF (Char.EQ.'n'.OR.Char.EQ.'N') THEN
C         naca.pts
          Finished = .TRUE.
          IVerbose = 5
          FDIVerbose = .FALSE.
          NpFlNm = CharXamplePath(1:KHiXamplePath)//'naca.pts'
          FNpFlFrm = .TRUE.
          OpFlNm = './naca.dpl'
          CharOutType = 't'
          CharOutForm = 'd'
C
        ELSE IF (Char.EQ.'t'.OR.Char.EQ.'T') THEN
C         tea.pts
          Finished = .TRUE.
          IVerbose = 5
          FDIVerbose = .FALSE.
          NpFlNm = CharXamplePath(1:KHiXamplePath)//'tea.pts'
          FNpFlFrm = .TRUE.
          FNotCon = .TRUE.
          FDFNotCon = .FALSE.
          FStretch = .TRUE.
          FDFStretch = .FALSE.
          DltStar = .1
          FDDltStar = .FALSE.
          BndStr = 10.
          FDBndStr = .FALSE.
          FFlatSwap = .TRUE.
          FDFFlatSwap = .FALSE.
          OpFlNm = './tea.dpl'
          CharOutType = 't'
          CharOutForm = 'd'
C
        ELSE IF (Char.EQ.'w'.OR.Char.EQ.'W') THEN
C         wake.pts
          Finished = .TRUE.
          IVerbose = 5
          FDIVerbose = .FALSE.
          NpFlNm = CharXamplePath(1:KHiXamplePath)//'wake.pts'
          FNpFlFrm = .TRUE.
          FStretch = .TRUE.
          FDFStretch = .FALSE.
          DltStar = .1
          FDDltStar = .FALSE.
          BndStr = 1000.
          FDBndStr = .FALSE.
          OpFlNm = './wake.dpl'
          CharOutType = 't'
          CharOutForm = 'd'
C
        END IF
      END DO
C
C     Length of the .pts file.
      KLo = 1
      DO WHILE (KLo.LT.80.AND.NpFlNm(KLo:KLo).EQ.' ')
        KLo = KLo+1
      END DO
      KHi = KLo
      DO WHILE (KHi.LT.80.AND.NpFlNm(KHi:KHi).NE.' ')
        KHi = KHi+1
      END DO
      NpFlNm = NpFlNm(KLo:KHi)
      KHiNpFlNm = KHi-KLo
C
C     Length of the .dpl file. Note that KHiOpFlNm counts one
C     extra. There might be a proper fix sometime.
      KLo = 1
      DO WHILE (KLo.LT.80.AND.OpFlNm(KLo:KLo).EQ.' ')
        KLo = KLo+1
      END DO
      KHi = KLo
      DO WHILE (KHi.LT.80.AND.OpFlNm(KHi:KHi).NE.' ')
        KHi = KHi+1
      END DO
      OpFlNm = OpFlNm(KLo:KHi)
      KHiOpFlNm = KHi-KLo+1
        
C
      RETURN
      END
