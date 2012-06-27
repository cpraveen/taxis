      SUBROUTINE GETCTRL (NtCtrl,NtLog,IVerbose,NpFlNm,KHiNpFlNm,
     &                    FNpFlFrm,FIntNde,FConNde,FAskRow,FNotCon,
     &                    SpcGrdRt,DTol,QTol,FStretch,BTol,DltStar,
     &                    BndStr,MVisRow,ISmooth,MIsoRow,FFlatSwap,
     &                    AnglMax,MCyclSwap,OpFlNm,KHiOpFlNm,
     &                    CharOutType,CharOutForm,
     &                    Title1,KHiTitle1,Title2,KHiTitle2,
     &                    Title3,KHiTitle3,Title4,KHiTitle4,
     &                    FLogFile,Code,Version,Date,FAllDisc,MLevel,
     &                    FLplc,Beta,RelFac,MSweep)
C
C Last update:
C 12July94,3:30; intro all pertaining to LPLC.
C 6May94,19:00; intro MLEVEL, remove required file names.
C 1Dec93,16:00; intro FAllDisc.
C 8Nov93,15:25; move banner with version etc from delaundo.f here.
C 9Feb93,23:52; towards 4.0.
C
C Read the control parameters necessary for Delaundo from stdin or
C a .ctr file. Welcome to DOMINO software.
C
C Input:
C NtCtrl:     Logical unit of the .ctr file.
C NtLog:      Logical unit of the log file.
C
C Output:
C IVerbose:   Verbosity.
C NpFlNm:     Name of the .pts input file.
C KHiNpFlNm:  Length of the input file name.
C FNpFlFrm:   .T. if the input file is of formatted type.
C FIntNde:    .T. if given interior nodes are to be used.
C FConNde:    .T. if interior nodes are to be constructed.
C FAskRow:    .T. if rows exceeding Max[Vis/Iso]Row are to be prompted.
C FNotCon:    .T. if the anti-connectivity-information is to be used.
C SpcGrdRt:   Ratio of the spacing gradient at the point of finest
C             spacing vs. the gradient at the point of coarsest spc.
C DTol:       Fraction of the background spacing to be required for
C             nodes in the foreground grid.
C QTol:       Fraction of the maximum sidelength to be required for
C             'nice' triangles.
C FStretch:   .T. if stretched layers are to be built.
C BTol:       Fraction of the background spacing to be required for
C             nodes in the background grid.
C DltStar:    Thickness of the viscous layer.
C BndStr:     Maximum aspect ratio in the viscous layer.
C MVisRow:    Number of viscous rows to be produced.
C ISmooth:    Number of outer viscous rows to be opened for retriangul-
C             ation.
C MIsoRow:    Number of isotropic rows to be constructed.
C FFlatSwap:  .T. if swapping to minimize maximum angles is to be done.
C AnglMax:    Maximum tolerable angle.
C MCyclSwap:  Number of sweeps for the swapping algorithm.
C OpFlNm:     Name of the output file.
C KHiOpFlNm:  Length of the name of the output file.
C CharOutType:Type of the output.
C CharOutForm:Format of the output file.
C Title1-4:   Titles in VKB output files.
C KHiTitle1-4:Length of titles in VKB output files.
C FLogFile:   .T. if a log-file is to be written.
C FAllDisc:   .T. if all non-consecutive frontal faces are to be 
C             disconnected.
C MLevel:     Number of desired grid levels.
C FLplc:      .T. if a weighted Laplacian filter is to be applied.
C Beta:       Factor affecting the weighting of nodes with few or
C             many connections: [0,1].
C RelFac:     Relaxation factor for the filter, [0,1]?
C MSweep:     Number of relaxation sweeps.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
      PARAMETER (MaxCharTxt=20)
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      CHARACTER Version*5,Code*20,SysTime*24,CTime*24,Date*20,
     &          NpFlNm*255,OpFlNm*255,CharOutType*1,CharOutForm*1,
     &          LogFlNm*80,Title1*80,Title2*80,Title3*80,Title4*80
      INTEGER STime,Time
      EXTERNAL TIME,CTIME
      FNotReq = .FALSE.
C
C     Get the current system time.
      STime = TIME()
      SysTime = CTIME(STime)
C
C     Set default parameters.
      CALL DEFCTRL (IVerbose,NpFlNm,KHiNpFlNm,FNpFlFrm,
     & FLogFile,LogFlNm,KHiLogFlNm,FIntNde,FConNde,FAskRow,
     & FNotCon,FAllDisc,SpcGrdRt,DTol,QTol,BTol,MIsoRow,ISmooth,
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
C     Read the parameters.
      CALL READCTRL (NtCtrl,NtLog,Code,Version,Date,IVerbose,
     & NpFlNm,KHiNpFlNm,FNpFlFrm,FLogFile,LogFlNm,KHiLogFlNm,
     & FIntNde,FConNde,FAskRow,FNotCon,FAllDisc,
     & SpcGrdRt,DTol,QTol,BTol,MIsoRow,ISmooth,
     & FStretch,MVisRow,BndStr,DltStar,FLplc,Beta,RelFac,MSweep,MLevel,
     & FFlatSwap,AnglMax,MCyclSwap,
     & OpFlNm,KHiOpFlNm,CharOutType,CharOutForm,
     & Title1,KHiTitle1,Title2,KHiTitle2,
     & Title3,KHiTitle3,Title4,KHiTitle4,
     & FDIVerbose,FDNpFlNm,FDFNpFlFrm,FDFLogFile,FDLogFlNm,
     & FDFIntNde,FDFConNde,FDFAskRow,FDFNotCon,FDFAllDisc,FDSpcGrdRt,
     & FDDTol,FDQTol,FDBTol,FDMIsoRow,FDISmooth,
     & FDFStretch,FDMVisRow,FDBndStr,FDDltStar,
     & FDFLplc,FDBeta,FDRelFac,FDMSweep,FDMLevel,
     & FDFFlatSwap,FDAnglMax,FDMCyclSwap,
     & FDOpFlNm,FDCharOutType,FDCharOutForm,
     & FDTitle1,FDTitle2,FDTitle3,FDTitle4)
C
C     Plausibility tests.
      CALL PLAUSCTRL (NtLog,IVerbose,FLogFile,
     &                FIntNde,FConNde,SpcGrdRt,DTol,QTol,BTol,
     &                FStretch,DltStar,BndStr,ISmooth,FFlatSwap,
     &                CharOutType,CharOutForm,
     &                Beta,RelFac,MLevel,
     &                FDFIntNde,FDFConNde,FDSpcGrdRt,
     &                FDISmooth,FDFFlatSwap,FDRelFac,FDMLevel)
C
C     Screen Info.
      NtI = 6
      CALL INFOCTRL (NtCtrl,NtLog,IVerbose,NpFlNm,KHiNpFlNm,
     & FNpFlFrm,FIntNde,FConNde,FAskRow,FNotCon,
     & SpcGrdRt,DTol,QTol,FStretch,BTol,DltStar,
     & BndStr,MVisRow,ISmooth,MIsoRow,FFlatSwap,
     & AnglMax,MCyclSwap,OpFlNm,KHiOpFlNm,
     & CharOutType,CharOutForm,
     & Title1,KHiTitle1,Title2,KHiTitle2,
     & Title3,KHiTitle3,Title4,KHiTitle4,
     & FLogFile,LogFlNm,KHiLogFlNm,FAllDisc,MLevel,
     & FLplc,Beta,RelFac,MSweep,
     & FDIVerbose,FDNpFlNm,FDFNpFlFrm,
     & FDFIntNde,FDFConNde,FDFAskRow,FDFNotCon,
     & FDSpcGrdRt,FDDTol,FDQTol,FDFStretch,FDBTol,
     & FDDltStar,FDBndStr,FDMVisRow,FDISmooth,
     & FDMIsoRow,FDFFlatSwap,FDAnglMax,FDMCyclSwap,
     & FDOpFlNm,FDCharOutType,FDCharOutForm,
     & FDTitle1,FDTitle2,FDTitle3,FDTitle4,
     & FDFLogFile,FDFAllDisc,FDMLevel,
     & FDFLplc,FDBeta,FDRelFac,FDMSweep,
     & NtI)
C
C     Logfile.
      IF (FLogFile) THEN
        NtI = NtLog
        CALL INFOCTRL (NtCtrl,NtLog,IVerbose,NpFlNm,KHiNpFlNm,
     &                 FNpFlFrm,FIntNde,FConNde,FAskRow,FNotCon,
     &                 SpcGrdRt,DTol,QTol,FStretch,BTol,DltStar,
     &                 BndStr,MVisRow,ISmooth,MIsoRow,FFlatSwap,
     &                 AnglMax,MCyclSwap,OpFlNm,KHiOpFlNm,
     &                 CharOutType,CharOutForm,
     &                 Title1,KHiTitle1,Title2,KHiTitle2,
     &                 Title3,KHiTitle3,Title4,KHiTitle4,
     &                 FLogFile,LogFlNm,KHiLogFlNm,FAllDisc,MLevel,
     &                 FLplc,Beta,RelFac,MSweep,
     &                 FDIVerbose,FDNpFlNm,FDFNpFlFrm,
     &                 FDFIntNde,FDFConNde,FDFAskRow,FDFNotCon,
     &                 FDSpcGrdRt,FDDTol,FDQTol,FDFStretch,FDBTol,
     &                 FDDltStar,FDBndStr,FDMVisRow,FDISmooth,
     &                 FDMIsoRow,FDFFlatSwap,FDAnglMax,FDMCyclSwap,
     &                 FDOpFlNm,FDCharOutType,FDCharOutForm,
     &                 FDTitle1,FDTitle2,FDTitle3,FDTitle4,
     &                 FDFLogFile,FDFAllDisc,FDMLevel,
     &                 FDFLplc,FDBeta,FDRelFac,FDMSweep,
     &                 NtI)
      END IF
C        
      RETURN
      END
