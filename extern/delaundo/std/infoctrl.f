      SUBROUTINE INFOCTRL (NtCtrl,NtLog,IVerbose,NpFlNm,KHiNpFlNm,
     &                     FNpFlFrm,FIntNde,FConNde,FAskRow,FNotCon,
     &                     SpcGrdRt,DTol,QTol,FStretch,BTol,DltStar,
     &                     BndStr,MVisRow,ISmooth,MIsoRow,FFlatSwap,
     &                     AnglMax,MCyclSwap,OpFlNm,KHiOpFlNm,
     &                     CharOutType,CharOutForm,
     &                     Title1,KHiTitle1,Title2,KHiTitle2,
     &                     Title3,KHiTitle3,Title4,KHiTitle4,
     &                     FLogFile,LogFlNm,KHiLogFlNm,FAllDisc,MLevel,
     &                     FLplc,Beta,RelFac,MSweep,
     &                     FDIVerbose,FDNpFlNm,FDFNpFlFrm,
     &                     FDFIntNde,FDFConNde,FDFAskRow,FDFNotCon,
     &                     FDSpcGrdRt,FDDTol,FDQTol,FDFStretch,FDBTol,
     &                     FDDltStar,FDBndStr,FDMVisRow,FDISmooth,
     &                     FDMIsoRow,FDFFlatSwap,FDAnglMax,FDMCyclSwap,
     &                     FDOpFlNm,FDCharOutType,FDCharOutForm,
     &                     FDTitle1,FDTitle2,FDTitle3,FDTitle4,
     &                     FDFLogFile,FDFAllDisc,FDMLevel,
     &                     FDFLplc,FDBeta,FDRelFac,FDMSweep,
     &                     NtI)
C
C Last update:
C 18May96; say OUTFILe when you mean it. 
C 9Jan96: fix bug in character indices of Titles.
C 12July94,18:20; cut out of READCTRL.
C
C Throw up info about the selected parameters. Welcome to Domino
C software.
C
C I/O: see READCTRL.
C NtI:   Logical unit to write to.
C FD...: .T. indicates that the default value is applied for
C        the parameter ...
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
      PARAMETER (MaxCharTxt=20)
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      CHARACTER NpFlNm*80,OpFlNm*80,CharOutType*1,CharOutForm*1,
     &          LogFlNm*80,Title1*80,Title2*80,Title3*80,Title4*80
C
C     Push out input information depending on IVerbose.
      IF (IVerbose.GT.0) WRITE (NtI,'(/A)') 
     & '    The following parameters will be used:'
      IF (.NOT.FDIVerbose.AND.IVerbose.GE.2) THEN
        WRITE (NtI,'(6X,A,/T8,I1,A)') ' VERBOSe:',IVerbose,', User.'
      ELSE IF (IVerbose.GE.5.AND.FDIVerbose) THEN
        WRITE (NtI,'(6X,A,/T8,I1,A)') ' VERBOSe:',IVerbose,', Default.'
      END IF
C
      IF (.NOT.FDNpFlNm.AND.IVerbose.GE.1) THEN
        WRITE (NtI,'(6X,A,/T8,A)') ' INFILE:',NpFlNm(1:KHiNpFlNm)
      ELSE IF (IVerbose.GE.2.AND.FDNpFlNm) THEN
        WRITE (NtI,'(6X,A,/T8,2A)') ' INFILE:',NpFlNm(1:KHiNpFlNm),
     &                            ', Default.'
      END IF
C
      IF (.NOT.FDFIntNde.AND.IVerbose.GE.2) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' NODEUSe:',FIntNde,', User.'
      ELSE IF (IVerbose.GE.5.AND.FDFIntNde) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' NODEUSe:',FIntNde,', Default.'
      END IF
      IF (.NOT.FDFConNde.AND.IVerbose.GE.2) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' NODECOnstr.:',FConNde,', User.'
      ELSE IF (IVerbose.GE.5.AND.FDFConNde) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)')
     &        ' NODECOnstr.:',FConNde,', Default.'
      END IF
      IF (.NOT.FDFAskRow.AND.IVerbose.GE.2.AND.FConNde) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' ASKROW:',FAskRow,', User.'
      ELSE IF (IVerbose.GE.5.AND.FConNde.AND.FDFAskRow) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' ASKROW:',FAksRow,', Default.'
      END IF
      IF (.NOT.FDFNotCon.AND.IVerbose.GE.2.AND.
     &    (FConNde.OR.CharOutType.EQ.'b')) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' ANTICOnnect.:',FNotCon,', User.'
      ELSE IF (IVerbose.GE.5.AND.FConNde.AND.FDFNotCon) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' ANTICOnnect.:',FNotCon,
     &                           ', Default.'
      END IF
      IF (.NOT.FDFAllDisc.AND.IVerbose.GE.2.AND.
     &    (FConNde.OR.CharOutType.EQ.'b')) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' ALLDISconnect.:',
     &                              FAllDisc,', User.'
      ELSE IF (IVerbose.GE.5.AND.FConNde.AND.FDFAllDisc) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' ALLDISconnect.:',FAllDisc,
     &                           ', Default.'
      END IF
C
      IF (.NOT.FDSpcGrdRt.AND.IVerbose.GE.2.AND.FConNde) THEN
        WRITE (NtI,'(6X,A,/T8,F6.2,A)') ' SPCRATio:',SpcGrdRt,', User.'
      ELSE IF (IVerbose.GE.5.AND.FConNde.AND.FDSpcGrdRt) THEN
        WRITE (NtI,'(6X,A,/T8,F6.2,A)')
     &        ' SPCRATio:',SpcGrdRt,', Default.'
      END IF
C
      IF (.NOT.FDDTol.AND.IVerbose.GE.2.AND.FConNde) THEN
        WRITE (NtI,'(6X,A,/T8,F6.4,A)') ' DTOLERance:',DTol,', User.'
      ELSE IF (IVerbose.GE.5.AND.FConNde) THEN
        WRITE (NtI,'(6X,A,/T8,F6.4,A)') ' DTOLERance:',DTol,', Default.'
      END IF
      IF (.NOT.FDQTol.AND.IVerbose.GE.2.AND.FConNde) THEN
        WRITE (NtI,'(6X,A,/T8,F6.4,A)') ' QTOLERance:',QTol,', User.'
      ELSE IF (IVerbose.GE.5.AND.FConNde.AND.FDQTol) THEN
        WRITE (NtI,'(6X,A,/T8,F6.4,A)') ' QTOLERance:',QTol,', Default.'
      END IF
C
      IF (FConNde.OR.CharOutType.EQ.'b') FSInfo = .TRUE.
      IF (FStretch.AND.IVerbose.GE.1.AND.FSInfo) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' STRETChing:',FStretch,', User.'
      ELSE IF (.NOT.FDFStretch.AND.IVerbose.GE.2.AND.FSInfo) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' STRETChing:',FStretch,', User.'
      ELSE IF (IVerbose.GE.5.AND.FSInfo.AND.FDFStretch) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)')
     &        ' STRETChing:',FStretch,', Default.'
      END IF
      IF (FStretch.AND..NOT.FDBTol.AND.IVerbose.GE.2.AND.FSInfo) THEN
        WRITE (NtI,'(6X,A,/T8,F6.4,A)') ' BTOLERance:',BTol,', User.'
      ELSE IF (FStretch.AND.FSInfo.AND.IVerbose.GE.5
     &         .AND.FDBTol) THEN
        WRITE (NtI,'(6X,A,/T8,F6.4,A)') ' BTOLERance:',BTol,', Default.'
      END IF
      IF (FStretch.AND.FSInfo.AND.IVerbose.GE.1) THEN
        WRITE (NtI,'(6X,A,/T8,F6.4,A)') ' DELTAStar:',DltStar
        WRITE (NtI,'(6X,A,/T8,F6.0,A)') ' MAXASPect ratio:',BndStr
      END IF
      IF (FStretch.AND.FConNde.AND.IVerbose.GE.2.AND..NOT.FDMVisRow)
     &  THEN
        WRITE (NtI,'(6X,A,/T8,I6,A)') ' MVISROw:',MVisRow,', User.'
      ELSE IF (FStretch.AND.FConNde.AND.
     &         IVerbose.GE.1.AND.FDMVisRow) THEN
        WRITE (NtI,'(6X,A,/T8,I6,A)') ' MVISROw:',MVisRow,', Default.'
      END IF
      IF (FStretch.AND.FConNde.AND.IVerbose.GE.2.AND..NOT.FDISmooth)
     &  THEN
        WRITE (NtI,'(6X,A,/T8,I3,A)') ' ISMOOTh:',ISmooth,' User'
      ELSE IF (FStretch.AND.FSInfo.AND.
     &         IVerbose.GE.1.AND.FDISmooth) THEN
        WRITE (NtI,'(6X,A,/T8,I3,A)') ' ISMOOTh:',ISmooth,' Default'
      END IF
C
      IF (.NOT.FDMIsoRow.AND.IVerbose.GE.2.AND.FConNde) THEN
        WRITE (NtI,'(6X,A,/T8,I6,A)') ' MISOROw:',MIsoRow,', User.'
      ELSE IF (IVerbose.GE.5.AND.FConNde.AND.FDMIsoRow) THEN
        WRITE (NtI,'(6X,A,/T8,I6,A)') ' MISOROw:',MIsoRow,', Default.'
      END IF
C
      IF (.NOT.FDFlplc.AND.IVerbose.GE.2) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' LAPLACian:',FLplc,', User.'
      ELSE IF (IVerbose.GE.5.AND.FConNde.AND.FDMIsoRow) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' LAPLACian:',FLplc,', Default.'
      END IF
C
      IF (.NOT.FDBeta.AND.IVerbose.GE.2.AND.FLplc) THEN
        WRITE (NtI,'(6X,A,/T8,F6.4,A)') ' LPBETA:',Beta,', User.'
      ELSE IF (IVerbose.GE.5.AND.FConNde.AND.FDMIsoRow) THEN
        WRITE (NtI,'(6X,A,/T8,F6.4,A)') ' LPBETA:',Beta,', Default.'
      END IF
C
      IF (.NOT.FDRelFac.AND.IVerbose.GE.2.AND.FLplc) THEN
        WRITE (NtI,'(6X,A,/T8,F6.4,A)') ' RELFACtor:',RelFac,', User.'
      ELSE IF (IVerbose.GE.5.AND.FConNde.AND.FDMIsoRow) THEN
        WRITE (NtI,'(6X,A,/T8,F6.4,A)') ' RELFACtor:',RelFac,
     &                                  ', Default.'
      END IF
C
      IF (.NOT.FDMSweep.AND.IVerbose.GE.2.AND.FLplc) THEN
        WRITE (NtI,'(6X,A,/T8,I6,A)') ' RSWEEPs:',MSweep,', User.'
      ELSE IF (IVerbose.GE.5.AND.FConNde.AND.FDMIsoRow) THEN
        WRITE (NtI,'(6X,A,/T8,I6,A)') ' RSWEEPs:',MSweep,', Default.'
      END IF
C
      IF (.NOT.FDMLevel.AND.IVerbose.GE.2.AND.FConNde) THEN
        WRITE (NtI,'(6X,A,/T8,I6,A)') ' MLEVELs:',MLevel,', User.'
      ELSE IF (IVerbose.GE.5.AND.FConNde.AND.FDMIsoRow) THEN
        WRITE (NtI,'(6X,A,/T8,I6,A)') ' MLEVELs:',MLevel,', Default.'
      END IF
C
      IF (.NOT.FDFFlatSwap.AND.IVerbose.GE.2) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' FLATSWap:',FFlatSwap,', User.'
      ELSE IF (IVerbose.GE.5.AND.FDFFlatSwap) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)')
     &        ' FLATSWap:',FFlatSwap,', Default.'
      END IF
      IF (FFlatSwap.AND.IVerbose.GE.2.AND..NOT.FDAnglMax) THEN
        WRITE (NtI,'(6X,A,/T8,F6.1,A)') ' ANGMAX:',AnglMax
      ELSE IF (FFlatSwap.AND.IVerbose.GE.5.AND.FDAnglMax) THEN
        WRITE (NtI,'(6X,A,/T8,F6.1,A)') ' ANGMAX:',AnglMax,', Default.'
      END IF
      IF (FFlatSwap.AND.IVerbose.GE.2.AND..NOT.FDMCyclSwap) THEN
        WRITE (NtI,'(6X,A,/T8,I5,A)') ' MCYCSWap:',MCyclSwap,' User'
      ELSE IF (FFlatSwap.AND.IVerbose.GE.5.AND.FDMCyclSwap) THEN
        WRITE (NtI,'(6X,A,/T8,I5,A)')
     &        ' MCYCSWap:',MCyclSwap,', Default.'
      END IF
C
      IF (.NOT.FDOpFlNm.AND.IVerbose.GE.1) THEN
        WRITE (NtI,'(6X,A,/T8,A)') ' OUTFILe:',OpFlNm(1:KHiOpFlNm)
      ELSE IF (IVerbose.GE.2.AND.FDOpFlNm) THEN
        WRITE (NtI,'(6X,A,/T8,2A)') ' OUTFILe:',OpFlNm(1:KHiOpFlNm),
     &                            ', Default.'
      END IF
      IF (.NOT.FDCharOutType.AND.IVerbose.GE.1) THEN
        WRITE (NtI,'(6X,A,/T8,A)') ' OUTTYPe:',CharOutType
      ELSE IF (IVerbose.GE.2.AND.FDOpFlNm) THEN
        WRITE (NtI,'(6X,A,/T8,2A)') ' OUTTYPe:',CharOutType,
     &                            ', Default.'
      END IF
      IF (.NOT.FDCharOutForm.AND.IVerbose.GE.1) THEN
        WRITE (NtI,'(6X,A,/T8,A)') ' OUTTYPe:',CharOutForm
      ELSE IF (IVerbose.GE.2.AND.FDOpFlNm) THEN
        WRITE (NtI,'(6X,A,/T8,2A)') ' OUTTYPe:',CharOutForm,
     &                            ', Default.'
      END IF
C
      IF (CharOutType.EQ.'g'.OR.CharOutType.EQ.'o') THEN
        IF (.NOT.FDTitle1.AND.IVerbose.GE.2) THEN
          WRITE (NtI,'(6X,A,/A)') ' TITLE1:',Title1(1:KHiTitle1)
        END IF
      END IF
      IF (CharOutType.EQ.'o'.AND.IVerbose.GE.2) THEN
        IF (.NOT.FDTitle2) THEN
          WRITE (NtI,'(6X,A,/A)') ' TITLE2:',Title2(1:KHiTitle2)
        END IF
        IF (.NOT.FDTitle3) THEN
          WRITE (NtI,'(6X,A,/A)') ' TITLE3:',Title3(1:KHiTitle3)
        END IF
        IF (.NOT.FDTitle4) THEN
          WRITE (NtI,'(6X,A,/A)') ' TITLE4:',Title4(1:KHiTitle4)
        END IF
      END IF
C
      IF (.NOT.FDFLogFile.AND.IVerbose.GE.2) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)') ' DOLOGFile:',FLogFile,', User.'
      ELSE IF (IVerbose.GE.5.AND.FDFLogFile) THEN
        WRITE (NtI,'(6X,A,/T8,L1,A)')
     &        ' DOLOGFile:',FLogFile,', Default.'
      END IF
      IF (FLogFile.AND.IVerbose.GE.1) THEN
        WRITE (NtI,'(6X,A,/T8,A)') ' LOGFILe:',LogFlNm(1:KHiLogFlNm)
      END IF
C
      IF (IVerbose.GE.1)
     &WRITE (NtI,'(6X,A)') ' ENDDAT: End of the list of control '
     &                    //'parameters.'
C
C
      RETURN
      END
      
