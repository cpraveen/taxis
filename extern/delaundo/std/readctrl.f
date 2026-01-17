      SUBROUTINE READCTRL (NtCtrl,NtLog,Code,Version,Date,IVerbose,
     & NpFlNm,KHiNpFlNm,FNpFlFrm,FLogFile,LogFlNm,KHiLogFlNm,
     & FIntNde,FConNde,FAskRow,FNotCon,FAllDisc,
     & SpcGrdRt,DTol,QTol,BTol,MIsoRow,ISmooth,
     & FStretch,MVisRow,BndStr,DltStar,FLplc,Beta,RelFac,MSweep,MLevel,
     & FFlatSwap,AnglMax,MCyclSwap,
     & OpFlNm,KHiOpFlNm,CharOutType,CharOutForm,
     & Title1,KHiTitle1,Title2,KHiTitle2,
     & Title3,KHiTitle3,Title4,KHiTitle4,
     & FDIVerbose,FDNpFlNm,FDFNpFlFrm,
     & FDFLogFile,FDLogFlNm,FDFIntNde,FDFConNde,FDFAskRow,
     & FDFNotCon,FDFAllDisc,FDSpcGrdRt,
     & FDDTol,FDQTol,FDBTol,FDMIsoRow,FDISmooth,
     & FDFStretch,FDMVisRow,FDBndStr,FDDltStar,
     & FDFLplc,FDBeta,FDRelFac,FDMSweep,FDMLevel,
     & FDFFlatSwap,FDAnglMax,FDMCyclSwap,
     & FDOpFlNm,FDCharOutType,FDCharOutForm,
     & FDTitle1,FDTitle2,FDTitle3,FDTitle4)
C
C Last update:
c 2Apr97, clean up for version 5.4
C 9Jan96: IBM-RS6000 compatibility.
C 12July94,18:20; cut out of READCTRL.
C
C Read the contents of the .ctr file or prompt for the selected parameters.
C Throw up a Hello-banner, open and close the .ctr file, open the log file.
C Welcome to Domino software.
C
C I/O: see READCTRL.
C NtI:   Logical unit to write to.
C FD...: .T. indicates that the default value is applied for the
C        parameter ...
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
      PARAMETER (MaxCharTxt=20)
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      CHARACTER String*80,Answer*1,CharTxt(MaxCharTxt)*80,CharKey*6,
     &          CharOpt(MaxCharTxt)*1,Version*5,Code*20,Date*20,
     &          NpFlNm*255,OpFlNm*255,CharOutType*1,CharOutForm*1,
     &          LogFlNm*80,Title1*80,Title2*80,Title3*80,Title4*80,
     &          CharOptYN(8)*1
      LOGICAL*4 FileEx
C
C     Insist on required parameters.
      FNotReq = .FALSE.
      FileEnd = .FALSE.
C
C     Mark all parameters as unread.
      FNoIVerbose = .TRUE.
      FNoNpFlNm = .TRUE.
      FNoFnpFlFrm = .TRUE.
      FNoFLogFile = .TRUE.
      FNoLogFlNm = .TRUE.
      FNoFIntNde = .TRUE.
      FNoFConNde = .TRUE.
      FNoFAskRow = .TRUE.
      FNoFNotCon = .TRUE.
      FNoFAllDisc = .TRUE.
      FNoSpcGrdRt = .TRUE.
      FNoDTol = .TRUE.
      FNoQTol = .TRUE.
      FNoFStretch = .TRUE.
      FNoBTol = .TRUE.
      FNoDltStar = .TRUE.
      FNoBndStr = .TRUE.
      FNoMVisRow = .TRUE.
      FNoISmooth = .TRUE.
      FNoMIsoRow = .TRUE.
      FNoFLplc = .TRUE.
      FNoBeta = .TRUE.
      FNoRelFac = .TRUE.
      FNoMSweep = .TRUE.
      FNoMLevel = .TRUE.
      FNoFFlatSwap = .TRUE.
      FNoAnglMax = .TRUE.
      FNoMCyclSwap = .TRUE.
      FNoOpFlNm = .TRUE.
      FNoCharOutType = .TRUE.
      FNoCharOutForm = .TRUE.
      FNoTitle1 = .TRUE.
      FNoTitle2 = .TRUE.
      FNoTitle3 = .TRUE.
      FNoTitle4 = .TRUE.
C
C     Create some standard CharOpt for y/n.
      MCharOptYN = 8
      CharOptYN(1) = 'y'
      CharOptYN(2) = 'Y'
      CharOptYN(3) = 'n'
      CharOptYN(4) = 'N'
      CharOptYN(5) = 't'
      CharOptYN(6) = 'T'
      CharOptYN(7) = 'f'
      CharOptYN(8) = 'F'
C
C     Find out wether or not to read from control file.
      String = 'delaundo.ctr'
      KPos = 1
      FXample = .FALSE.
      FBanner = .FALSE.
      FileEx = .FALSE.
      DO WHILE (.NOT.FileEx) 
C
        INQUIRE (FILE=String(KPos:81-KPos),EXIST=FileEx)
        IF (.NOT.(FileEx.OR.FBanner)) THEN
          WRITE (*,5) Code,Version,Date
   5      FORMAT(/'    This is ',A8,', version ',A3,' of ',A13,','/
     &           '       Welcome to DOMINO Software.')
          FBanner = .TRUE.
        END IF
C
        IF (FileEx) THEN
C         Read parameters from this file.
          FNoCtr = .FALSE.
          FileEnd = .FALSE.
          FXample = .FALSE.
C
        ELSE IF (String(KPos:KPos+5).EQ.'ALLPAR'.OR.
     &           String(KPos:KPos).EQ.'?') THEN
C         Prompt for all applicable parameters, read no file.
          FAskAll = .TRUE.
          FNoCtr = .TRUE.
          FileEx = .TRUE.
          FileEnd = .TRUE.
          FXample = .FALSE.
C
        ELSE IF (String(KPos:KPos+5).EQ.'HELPME') THEN
C         Throw up one of the help files, then try to read another
C         .ctr file name.
          CALL HELPME
          String = 'delaundo.ctr'
          FileEx = .FALSE.
          FNoCtr = .FALSE.
          FXample = .FALSE.
C
        ELSE IF (String(KPos:KPos+5).EQ.'XAMPLE') THEN
C         Do one of the examples, use the parameters set in xample.f.
C         Read no other .ctr file, prompt for no more parameters.
          FXample = .TRUE.
          CALL XAMPLE(IVerbose,NpFlNm,KHiNpFlNm,
     &                FNpFlFrm,FNotCon,FStretch,DltStar,
     &                BndStr,FFlatSwap,OpFlNm,KHiOpFlNm,
     &                CharOutType,CharOutForm,
     &                FDIVerbose,FDFStretch,FDDltStar,FDBndStr,
     &                FDFFlatSwap,FDFNotCon)
          FileEx = .TRUE.
          FNoCtr = .TRUE.
          FileEnd = .TRUE.
C
        ELSE
          INQUIRE (FILE=String(KPos:81-KPos),EXIST=FileEx)
          FNoCtr = .FALSE.
        END IF
C
        IF (FileEx.AND..NOT.FNoCtr.AND..NOT.FXample) THEN
          OPEN (NtCtrl,File=String,STATUS='OLD',FORM='FORMATTED')
          REWIND (NtCtrl)
C
        ELSE IF (.NOT.FNoCtr.AND..NOT.FXample) THEN
          WRITE (*,'(/A,3(/7X,A))')
     &    '    Please give the name of the .CTR file or type',
     &    'HELPME (in capitals!) to recieve information,',
     &    'XAMPLE (capitals!) to run an example,',
     &    'ALLPAR (caps!) or ''?'' to be prompted for all.'
     &    //' applicable parameters.'
          READ (*,'(A)') String
          KPos = 1
          DO WHILE (String(KPos:KPos).EQ.' '.AND.KPos.LE.255)
            KPos = KPos+1
          END DO
C
        END IF
C
      END DO
C
      IF (.NOT.FNoCtr.AND..NOT.FXample) THEN
C       Open the .ctr file.
        OPEN (NtCtrl,File=String,STATUS='OLD',FORM='FORMATTED')
        REWIND (NtCtrl)
      END IF
C
C     Read the parameters.
      FRead = .TRUE.
      DO WHILE ((.NOT.FileEnd.OR.FRead).AND..NOT.FXample)
        FRead = .FALSE.
C
C       Try to read from .ctr file.
        IF (.NOT.FNoCtr) CALL READKEY (NtCtrl,CharKey,FileEnd)
        IF (FileEnd) FNoCtr = .TRUE.
C
        IF (CharKey.EQ.'IGNORE') THEN
C
        ELSE IF (CharKey.EQ.'VERBOS'.OR.
     &           FNoCtr.AND.FAskAll.AND.FNoIVerbose) THEN
C         Read the verbosity.
          MCharTxt = 1
          CharTxt(1) = '       What level of program output do you'
     &                 //' want [0..5]?'
          CALL READINTEGER (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                      FDIVerbose,IVerbose)
C          IVerbose = MIN(5,MAX(0,IVerbose))
          FNoIVerbose = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'ALLPAR') THEN
C         Read whether all parameters shall be prompted.
          MCharTxt = 1
          CharTxt(1) = '       Do you want all parameters '
     &                 //'prompted [y,n]?'
          Answer = " "
          CALL READCHAR(NtCtrl,CharOptYN,MCharOptYN,CharTxt,MCharTxt,
     &                  FNoCtr,FNotReq,FDFAskAll,Answer)
          IF (Answer.EQ.'Y'.OR.Answer.EQ.'y'.OR.
     &        Answer.EQ.'t'.OR.Answer.EQ.'T') THEN
            FAskAll = .TRUE.
          ELSE 
            FAskAll = .FALSE.
          END IF
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'INFILE'.OR.
     &           FNoCtr.AND.FAskAll.AND.FNoNpFlNm) THEN
C         Read the input file name.
          MCharTxt = 1
          CharTxt(1) = '       Please give the name of the .pts'
     &                 //' file for input.'
          CALL READSTRING(NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                    FDNpFlNm,String,KLo,KHi)
          IF (KLo.LT.81) THEN
            NpFlNm = String(KLo:KHi)
            KHiNpFlNm = KHi-KLo+1
          END IF
          FNoNpFlNm = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'INFORM'.OR.
     &           FNoCtr.AND.FAskAll.AND.FNoFNpFlFrm) THEN
C         Read the input file format.
          MCharTxt = 1
          CharTxt(1) = '       Is the input file formatted [y,t,n,f]?'
          Answer = " "
          CALL READCHAR(NtCtrl,CharOptYN,MCharOptYN,CharTxt,MCharTxt,
     &                  FNoCtr,FNotReq,FDFNpFlFrm,Answer)
          IF (Answer.EQ.'Y'.OR.Answer.EQ.'y'.OR.
     &        Answer.EQ.'t'.OR.Answer.EQ.'T') THEN
            FNpFlFrm = .TRUE.
          ELSE 
            FNpFlFrm = .FALSE.
          END IF
          FNoFNpFlFrm = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'NODEUS'.OR.
     &           FNoCtr.AND.FAskAll.AND.FNoFIntNde) THEN
C         Read whether all steps shall be prompted.
          MCharTxt = 1
          CharTxt(1) = '       Shall the given interior nodes be'
     &                 //' used [y,t,n,f]?'
          Answer = " "
          CALL READCHAR(NtCtrl,CharOptYN,MCharOptYN,CharTxt,MCharTxt,
     &                  FNoCtr,FNotReq,FDFIntNde,Answer)
          IF (Answer.EQ.'Y'.OR.Answer.EQ.'y'.OR.
     &        Answer.EQ.'t'.OR.Answer.EQ.'T') THEN
            FIntNde = .TRUE.
          ELSE 
            FIntNde = .FALSE.
          END IF
          FNoFIntNde = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'NODECO'.OR.
     &           FNoCtr.AND.FAskAll.AND.FNoFConNde) THEN
C         Read whether all steps shall be prompted.
          MCharTxt = 1
          CharTxt(1) = '       Shall interior nodes be constructed'
     &                 //' [y,t,n,f]?'
          Answer = " "
          CALL READCHAR(NtCtrl,CharOptYN,MCharOptYN,CharTxt,MCharTxt,
     &                  FNoCtr,FNotReq,FDFConNde,Answer)
          IF (Answer.EQ.'Y'.OR.Answer.EQ.'y'.OR.
     &        Answer.EQ.'t'.OR.Answer.EQ.'T') THEN
            FConNde = .TRUE.
          ELSE 
            FConNde = .FALSE.
          END IF
          FNoFConNde = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'ASKROW'.OR.
     &           FConNde.AND.FNoCtr.AND.FAskAll.AND.FNoFAskRow) THEN
          MCharTxt = 1
          CharTxt(1) = '       Shall extra rows of constructed nodes'
     &                 //' be prompted [y,t,n,f]?'
          Answer = " "
          CALL READCHAR(NtCtrl,CharOptYN,MCharOptYN,CharTxt,MCharTxt,
     &                  FNoCtr,FNotReq,FDFAskRow,Answer)
          IF (Answer.EQ.'Y'.OR.Answer.EQ.'y'.OR.
     &        Answer.EQ.'t'.OR.Answer.EQ.'T') THEN
            FAskRow = .TRUE.
          ELSE 
            FAskRow = .FALSE.
          END IF
          FNoFAskRow = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'ANTICO'.OR.
     &           (FConNde.OR.CharOutType.EQ.'b').AND.
     &           FNoCtr.AND.FAskAll.AND.FNoFNotCon) THEN
          MCharTxt = 1
          CharTxt(1) = '       Shall the anti connectivity'
     &                 //' information be respected [y,t,n,f]?'
          Answer = " "
          CALL READCHAR(NtCtrl,CharOptYN,MCharOptYN,CharTxt,MCharTxt,
     &                  FNoCtr,FNotReq,FDFNotCon,Answer)
          IF (Answer.EQ.'Y'.OR.Answer.EQ.'y'.OR.
     &        Answer.EQ.'t'.OR.Answer.EQ.'T') THEN
            FNotCon = .TRUE.
          ELSE 
            FNotCon = .FALSE.
          END IF
          FNoFNotCon = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'ALLDIS'.OR.
     &           (FConNde.OR.CharOutType.EQ.'b').AND.
     &           FNoCtr.AND.FAskAll.AND.FNoFAllDisc) THEN
          MCharTxt = 1
          CharTxt(1) = '       Disconnect all non-consecutive'
     &                 //' frontal faces [y,t,n,f]?'
          Answer = " "
          CALL READCHAR(NtCtrl,CharOptYN,MCharOptYN,CharTxt,MCharTxt,
     &                  FNoCtr,FNotReq,FDFAllDisc,Answer)
          IF (Answer.EQ.'Y'.OR.Answer.EQ.'y'.OR.
     &        Answer.EQ.'t'.OR.Answer.EQ.'T') THEN
            FAllDisc = .TRUE.
          ELSE 
            FAllDisc = .FALSE.
          END IF
          FNoFAllDisc = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'SPCRAT'.OR.
     &           FConNde.AND.FNoCtr.AND.FAskAll.AND.FNoSpcGrdRt) THEN
          MCharTxt = 1
          CharTxt(1) = '       Give the ratio in gradients at the'
     &                 //' min. spacing vs the max. spacing.'
          CALL READREAL (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                   FDSpcGrdRt,RealAnswer)
          IF (.NOT.FDSpcGrdRt) SpcGrdRt = DBLE(RealAnswer)
          FNoSpcGrdRt = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'DTOLER'.OR.
     &           FConNde.AND.FNoCtr.AND.FAskAll.AND.FNoDTol) THEN
C         Read a distance tolerance DTol.
          MCharTxt = 1
          CharTxt(1) = '       Give a distance tolerance DTol'
          CALL READREAL (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                   FDDTol,DTol)
          FNoDTol = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'QTOLER'.OR.
     &           FConNde.AND.FNoCtr.AND.FAskAll.AND.FNoQTol) THEN
C         Read a sidelength tolerance QTol.
          MCharTxt = 1
          CharTxt(1) = '       Give a distance tolerance QTol'
          CALL READREAL (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                   FDQTol,QTol)
          FNoQTol = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'STRETC'.OR.
     &           FConNde.AND.FNoCtr.AND.FAskAll.AND.FNoFStretch) THEN
          MCharTxt = 1
          CharTxt(1) = '       Shall a stretched layer be built'
     &                 //' [y,t,n,f]?'
          Answer = " "
          CALL READCHAR(NtCtrl,CharOptYN,MCharOptYN,CharTxt,MCharTxt,
     &                  FNoCtr,FNotReq,FDFStretch,Answer)
          IF (Answer.EQ.'Y'.OR.Answer.EQ.'y'.OR.
     &        Answer.EQ.'t'.OR.Answer.EQ.'T') THEN
            FStretch = .TRUE.
          ELSE 
            FStretch = .FALSE.
          END IF
          FNoFStretch = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'BTOLER'.OR.FConNde.AND.
     &           FNoCtr.AND.FStretch.AND.FAskAll.AND.FNoBTol) THEN
C         Read a background distance tolerance BTol.
          MCharTxt = 1
          CharTxt(1) = '       Give a distance tolerance BTol'
          CALL READREAL (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                   FDBTol,BTol)
          FNoBTol = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'DELTAS'.OR.
     &           FConNde.AND.FNoCtr.AND.FStretch.AND.FNoDltStar) THEN
C         Read a background distance tolerance BTol.
          MCharTxt = 1
          CharTxt(1) = '       Give a thickness of the stretched'
     &                 //' layer.'
          CALL READREAL (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                   FDDltStar,DltStar)
          FNoDltStar = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'MAXASP'.OR.
     &           FConNde.AND.FNoCtr.AND.FStretch.AND.FNoBndStr) THEN
C         Read a background distance tolerance BTol.
          MCharTxt = 1
          CharTxt(1) = '       Give a maximum aspect ratio in the '
     &                 //'stretched layer.'
          CALL READREAL (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                   FDBndStr,BndStr)
          FNoBndStr = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'MVISRO'.OR.FStretch.AND.
     &           FConNde.AND.FNoCtr.AND.FAskAll.AND.FNoMVisRow) THEN
C         Read a maximum number of viscous rows to be constructed.
          MCharTxt = 1
          CharTxt(1) = '       How many viscous rows shall'//
     &                 ' be constructed?'
          CALL READINTEGER (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                      FDMVisRow,MVisRow)
          FNoMVisRow = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'ISMOOT'.OR.FStretch.AND.
     &           FConNde.AND.FNoCtr.AND.FAskAll.AND.FNoISmooth) THEN
C         Read the value for smoothing outer viscous cells [0,2].
          MCharTxt = 1
          CharTxt(1) = '       Smoothing indicator [0..2] for outer'
     &                 //' viscous cells?'
          CALL READINTEGER (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                      FDISmooth,ISmooth)
          ISmooth = MIN(2,MAX(0,ISmooth))
          FNoISmooth = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'MISORO'.OR.
     &           FConNde.AND.FNoCtr.AND.FAskAll.AND.FNoMIsoRow) THEN
C         Read a maximum number of isotropic rows to be constructed.
          MCharTxt = 1
          CharTxt(1) = '       How many isotropic rows shall be'//
     &                 ' constructed?'
          CALL READINTEGER (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                      FDMIsoRow,MIsoRow)
          FNoMIsoRow = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'LAPLAC'.OR.
     &           FConNde.AND.FNoCtr.AND.FAskAll.AND.FNoFLplc) THEN
          MCharTxt = 1
          CharTxt(1) = '       Shall a Laplacian filter'
     &                 //' be applied [y,t,n,f]?'
          Answer = " "
          CALL READCHAR(NtCtrl,CharOptYN,MCharOptYN,CharTxt,MCharTxt,
     &                  FNoCtr,FNotReq,FDFLplc,Answer)
          IF (Answer.EQ.'Y'.OR.Answer.EQ.'y'.OR.
     &        Answer.EQ.'t'.OR.Answer.EQ.'T') THEN
            FLplc = .TRUE.
          ELSE 
            FLplc = .FALSE.
          END IF
          FNoFLplc = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'LPBETA'.OR.FLplc.AND.
     &           FConNde.AND.FNoCtr.AND.FAskAll.AND.FNoBeta) THEN
C         Read a Richter factor Beta.
          MCharTxt = 1
          CharTxt(1) = '       Give a weighting effect for'
     &                 //' the Laplacian, Beta = [0,1].'
          CALL READREAL (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                   FDBeta,Beta)
          FNoBeta = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'RELFAC'.OR.FLplc.AND.
     &           FConNde.AND.FNoCtr.AND.FAskAll.AND.FNoRelFac) THEN
C         Read a relaxation factor for the Laplacian filter.
          MCharTxt = 1
          CharTxt(1) = '       Give a relaxation factor for'
     &                 //' the Laplacian filter  [0,1].'
          CALL READREAL (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                   FDRelFac,RelFac)
          FNoRelFac = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'RSWEEP'.OR.FLplc.AND.
     &           FConNde.AND.FNoCtr.AND.FAskAll.AND.FNoMSweep) THEN
C         Read a maximum number of viscous rows to be constructed.
          MCharTxt = 1
          CharTxt(1) = '       How many viscous rows shall'//
     &                 ' be constructed?'
          CALL READINTEGER (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                      FDMSweep,MSweep)
          FNoMSweep = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'MLEVEL'.OR.
     &           FConNde.AND.FNoCtr.AND.FAskAll.AND.FNoMLevel) THEN
C         Read a number of grid levels for MG.
          MCharTxt = 1
          CharTxt(1) = '       How many grid levels shall be'//
     &                 ' constructed?'
          CALL READINTEGER (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                      FDMLevel,MLevel)
          FNoMLevel = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'FLATSW'.OR.
     &           FNoCtr.AND.FAskAll.AND.FNoFFlatSwap) THEN
          MCharTxt = 1
          CharTxt(1) = '       Swap for smallest maximum '
     &                 //'angles [y,t,n,f]?'
          Answer = " "
          CALL READCHAR(NtCtrl,CharOptYN,MCharOptYN,CharTxt,MCharTxt,
     &                  FNoCtr,FNotReq,FDFFlatSwap,Answer)
          IF (Answer.EQ.'Y'.OR.Answer.EQ.'y'.OR.
     &        Answer.EQ.'t'.OR.Answer.EQ.'T') THEN
            FFlatSwap = .TRUE.
          ELSE 
            FFlatSwap = .FALSE.
          END IF
          FNoFFlatSwap = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'ANGMAX'.OR.
     &           FFlatSwap.AND.FNoCtr.AND.FAskAll.AND.FNoAnglMax) THEN
C         Read a limiting maximum angle to start swapping.
          MCharTxt = 1
          CharTxt(1) = '       Give a limiting maximum angle for '
     &                 //'the swap.'
          CALL READREAL (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                   FDAnglMax,AnglMax)
          FNoAnglMax = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'MCYCSW'.OR.
     &           FFlatSwap.AND.FNoCtr.AND.FAskAll.AND.FNoMCyclSwap) THEN
C         Read a maximum number of viscous rows to be constructed.
          MCharTxt = 1
          CharTxt(1) = '       How many sweeps for the maximum angle'
     &                 //' shall be executed?'
          CALL READINTEGER (NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                      FDMCyclSwap,MCyclSwap)
          FNoMCyclSwap = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'OUTFIL'.OR.
     &           FNoCtr.AND.FAskAll.AND.FNoOpFlNm) THEN
C         Read the output file name.
          MCharTxt = 1
          CharTxt(1) = '       Please give the name of the output'
     &                 //' file.'
          CALL READSTRING(NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                    FDOpFlNm,String,KLo,KHi)
          IF (KLo.LT.81) THEN
            OpFlNm = String(KLo:KHi)
            KHiOpFlNm = KHi-KLo+1
          END IF
          FNoOpFlNm = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'OUTTYP'.OR.
     &           FNoCtr.AND.FAskAll.AND.FNoCharOutType) THEN
C         Read the output file type.
          MCharOpt = 10
          CharOpt(1) = 't'
          CharOpt(2) = 'q'
          CharOpt(3) = 'h'
          CharOpt(4) = 'b'
          CharOpt(5) = 'T'
          CharOpt(6) = 'Q'
          CharOpt(7) = 'H'
          CharOpt(8) = 'B'
          CharOpt(9) = 'i'
          CharOpt(10) = 'I'
          MCharTxt = 5
          CharTxt(1) = '       Output: the triangulation         [t],'
          CharTxt(2) = '               a quadratic triangulation [q],'
          CharTxt(3) = '               the grid with convex hull [h],'
          CharTxt(4) = '               the background grid       [b],'
          CharTxt(5) = '               the initial triangulation [i]?'
          Answer = " "
          CALL READCHAR(NtCtrl,CharOpt,MCharOpt,CharTxt,MCharTxt,
     &                  FNoCtr,FNotReq,FDCharOutType,Answer)
          IF (Answer.EQ.'T'.OR.Answer.EQ.'t') THEN
            CharOutType = 't'
          ELSE IF (Answer.EQ.'Q'.OR.Answer.EQ.'q') THEN
            CharOutType = 'q'
          ELSE IF (Answer.EQ.'H'.OR.Answer.EQ.'h') THEN
            CharOutType = 'h'
          ELSE IF (Answer.EQ.'B'.OR.Answer.EQ.'b') THEN
            CharOutType = 'b'
          ELSE IF (Answer.EQ.'i'.OR.Answer.EQ.'I') THEN
            CharOutType = 'i'
          END IF
          FNoCharOutType = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'OUTFOR'.OR.
     &           FNoCtr.AND.FAskAll.AND.FNoCharOutForm) THEN
C         Read the output file format.
          MCharOpt = 8
          CharOpt(1) = 'g'
          CharOpt(2) = 'o'
          CharOpt(3) = 'd'
          CharOpt(4) = 'u'
          CharOpt(5) = 'G'
          CharOpt(6) = 'O'
          CharOpt(7) = 'D'
          CharOpt(8) = 'U'
          MCharTxt = 4
          CharTxt(1) = '       Output format: .dpl (dplot) [d],'
          CharTxt(2) = '                      .gri (vkb)   [g],'
          CharTxt(3) = '                      .out (vkb)   [o],'
          CharTxt(4) = '                      .ucd (AvS)   [u],'
          Answer = " "
          CALL READCHAR(NtCtrl,CharOpt,MCharOpt,CharTxt,MCharTxt,
     &                  FNoCtr,FNotReq,FDCharOutForm,Answer)
          IF (Answer.EQ.'D'.OR.Answer.EQ.'d') THEN
            CharOutForm = 'd'
          ELSE IF (Answer.EQ.'g'.OR.Answer.EQ.'G') THEN
            CharOutForm = 'g'
          ELSE IF (Answer.EQ.'o'.OR.Answer.EQ.'O') THEN
            CharOutForm = 'o'
          ELSE IF (Answer.EQ.'u'.OR.Answer.EQ.'U') THEN
            CharOutForm = 'u'
          END IF
          FNoCharOutForm = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'TITLE1'.OR.
     &           (CharOutForm.EQ.'g'.OR.CharOutForm.EQ.'o').AND.
     &           FAskAll.AND.FNoCtr.AND.FNoTitle1) THEN
          MCharTxt = 1
          CharTxt(1) = '       Please give a first title.'
          CALL READSTRING(NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                    FDTitle1,String,KLo,KHi)
          Title1 = String(1:MIN(80,KHi))
          KHiTitle1 = KHi
          FNoTitle1 = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'TITLE2'.OR.
     &           CharOutForm.EQ.'o'.AND.
     &           FAskAll.AND.FNoCtr.AND.FNoTitle2) THEN
          MCharTxt = 1
          CharTxt(1) = '       Please give a second title.'
          CALL READSTRING(NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                    FDTitle2,String,KLo,KHi)
          Title2 = String(1:MIN(80,KHi))
          KHiTitle2 = KHi
          FNoTitle2 = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'TITLE3'.OR.
     &           CharOutForm.EQ.'o'.AND.
     &           FAskAll.AND.FNoCtr.AND.FNoTitle3) THEN
          MCharTxt = 1
          CharTxt(1) = '       Please give a third title.'
          CALL READSTRING(NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                    FDTitle3,String,KLo,KHi)
          Title3 = String(1:MIN(80,KHi))
          KHiTitle3 = KHi
          FNoTitle3 = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'TITLE4'.OR.
     &           CharOutForm.EQ.'o'.AND.
     &           FAskAll.AND.FNoCtr.AND.FNoTitle4) THEN
          MCharTxt = 1
          CharTxt(1) = '       Please give a fourth title.'
          CALL READSTRING(NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                    FDTitle4,String,KLo,KHi)
          Title4 = String(1:MIN(80,KHi))
          KHiTitle4 = KHi
          FNoTitle4 = .FALSE.
          FRead = .TRUE.
C            
        ELSE IF (CharKey.EQ.'DOLOGF'.OR.
     &           FNoCtr.AND.FAskAll.AND.FNoFLogFile) THEN
C         Read whether a logfile is desired.
          MCharTxt = 1
          CharTxt(1) = '       Shall a logfile be written [y,t,n,f]?'
          Answer = " "
          CALL READCHAR(NtCtrl,CharOptYN,MCharOptYN,CharTxt,MCharTxt,
     &                  FNoCtr,FNotReq,FDFLogFile,Answer)
          IF (Answer.EQ.'Y'.OR.Answer.EQ.'y'.OR.
     &        Answer.EQ.'t'.OR.Answer.EQ.'T') THEN
            FLogFile = .TRUE.
          ELSE 
            FLogFile = .FALSE.
          END IF
          FNoFLogFile = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'LOGFIL'.OR.
     &           FNoCtr.AND.FNoLogFlNm.AND.FLogFile.AND.FAskAll) THEN
C         Read the log file name.
          MCharTxt = 1
          CharTxt(1) = '       Please give the name of the .log file '
     &                 //'for log output.'
          CALL READSTRING(NtCtrl,CharTxt,MCharTxt,FNoCtr,FNotReq,
     &                    FDLogFlNm,String,KLo,KHi)
          IF (KLo.LT.81) THEN
            LogFlNm = String(KLo:KHi)
            KHiLogFlNm = KHi-KLo+1
          END IF
          FNoLogFlNm = .FALSE.
          FRead = .TRUE.
C
        ELSE IF (CharKey.EQ.'ENDDAT') THEN
C         Stop reading .ctr file.
          FileEnd = .TRUE.
          FNoCtr = .TRUE.
C
        ELSE IF (.NOT.FileEnd) THEN
C         ?
          WRITE (*,'(/3A/)') ' WARNING: Unknown keyword:',
     &                       Charkey,' ignored.'
C
        END IF
C
      END DO 
C
      IF (IVerbose.GT.0.AND..NOT.FBanner) THEN
        WRITE (*,5) Code,Version,Date
        FBanner = .TRUE.
      END IF
C
      IF (FLogFile) THEN
        OPEN (NtLog,File=LogFlNm,STATUS='UNKNOWN',FORM='FORMATTED')
        REWIND NtLog
        WRITE (NtLog,5) Code,Version,Date
      END IF
C
C
      RETURN
      END
      
