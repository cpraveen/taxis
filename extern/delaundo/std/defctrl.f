      SUBROUTINE DEFCTRL (IVerbose,NpFlNm,KHiNpFlNm,FNpFlFrm,
     & FLogFile,LogFlNm,KHiLogFlNm,FIntNde,FConNde,FAskRow,
     & FNotCon,FAllDisc,SpcGrdRt,DTol,QTol,BTol,MIsoRow,ISmooth,
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
C Last update:
C 24Sep96; properly initialize KHiLogFlNm.
C 9Jan96; clean up.
C 12July94,18:10; cut out of READCTRL.
C
C Set default values for all parameters defined by READCTRL as
C far as applicable. Note that some of these defaults are reset in
C plausctrl.f. For the sake of simplicity, there is no communication
C between the two routines about their default settings other than
C your favorite editor. Welcome to Domino software.
C
C I/O: see READCTRL.
C
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      CHARACTER NpFlNm*80,OpFlNm*80,CharOutType*1,CharOutForm*1,
     &          LogFlNm*80,Title1*80,Title2*80,Title3*80,Title4*80
C
C     Set default values for all parameters:
C     0<="Verbosity"<5?, the higher IVerbose, the more info on screen.
      FDIVerbose = .TRUE.
      IVerbose = 3
C     Inputfile name.
      FDNpFlNm = .TRUE.
      NpFlNm = 'delaundo.pts'
      KHiNpFlNm = 13
C     Inputfile format.
      FDFnpFlFrm = .TRUE.
      FNpFlFrm = .TRUE.
C     FLogFile: when .true., a log-file will be written.
      FDFLogFile = .TRUE.
      FLogFile = .TRUE.
C     Name of the log file to be written.
      LogFlNm = 'delaundo.log'
      KHiLogFlNm = 13
C     FIntNde = [.t.,.f.], when .true., given interior point cloud used.
      FDFIntNde = .TRUE.
      FIntNde = .FALSE.
C     FConNde = [.t.,.f.], when .true., interior nodes constructed.
      FDFConNde = .FALSE.
      FConNde = .TRUE.
C     FAskRow = [.t.,.f.], when .true., all parameters asked. 
      FDFAskRow = .TRUE.
      FAskRow = .FALSE.
C     FNotCon = [.t.,.f.], when .true., anti connectivity enforced.
      FDFNotCon = .TRUE.
      FNotCon = .FALSE.
C     FAllDisc = [.t.,.f.], when .true., all frontal non-consecutive
C     faces will be disconnected.
      FDFAllDisc = .TRUE.
      FAllDisc = .FALSE.
C     SpcGrdRt: ratio of the spacing gradients at the minimum vs
C     the gradient at the maximum spacing site. 
      FDSpcGrdRt = .TRUE.
      SpcGrdRt = 1.
C     DTol: Distance tolerance for nodes in the foreground grid.
      FDDTol = .TRUE.
      DTol = .65
C     QTol: Sidelength tolerance for acceptable triangles.
      FDQTol = .TRUE.
      QTol = .65
C     FStretch: when .true., viscous meshing will be done.
      FDFStretch = .TRUE.
      FStretch = .FALSE.
C     BTol: Distance tolerance for additional nodes in the background
C     grid.
      FDBTol = .TRUE.
      BTol = 2.
C     DltStar: Thickness of the viscous layer.
      FDDltStar = .TRUE.
      DltStar = .1
C     BndStr: Maximum aspect ratio of cells in the viscous layer.
      FDBndStr = .TRUE.
      BndStr = 10.
C     MVisRow: the maximum number of nodes to be built.
      FDMVisRow = .TRUE.
      MVisRow = 30000
C     ISmooth: Smoothing indicator for outer viscous cells.
      FDISmooth = .TRUE.
      ISmooth = 0
C     MIsoRow: the maximum number of nodes to be built.
      FDMIsoRow = .TRUE.
      MIsoRow = 30000
C     FLplc: Apply Laplacian?
      FDFLplc = .TRUE.
      FLplc = .FALSE.
C     Beta: How much weighting?
      FDBeta = .TRUE.
      Beta = .5
C     RelFac: Relaxation factor.
      FDRelFac = .TRUE.
      RelFac = .75
C     MSweep: Number of relaxations.
      FDMSweep = .TRUE.
      MSweep = 10   
C     MLevel: the number of grid levels of successively coarser meshes.
      FDMLevel = .TRUE.
      MLevel = 1
C     FFlatSwap: when .true., do a Min-Max swap over the grid.
      FDFFlatSwap = .TRUE.
      FFlatSwap = .FALSE.
C     AnglMax: an limiting angle to start angular swapping.
      FDAnglMax = .TRUE.
      AnglMax = 120.
C     MCycleSwap: Number of cycles for the swap.
      FDMCyclSwap = .TRUE.
      MCyclSwap = 10
C     Outputfile name.
      FDOpFlNm = .TRUE.
      OpFlNm = 'delaundo.dpl'
      KHiOpFlNm = 13
C     Output type.
      FDCharOutType = .TRUE.
      CharOutType = 't'
C     Output format.
      FDCharOutForm = .TRUE.
      CharOutForm = 'd'
C     Titles: only applied to vkb's .gri and .out formats.
      FDTitle1 = .TRUE.
      Title1 = ' '
      KHiTitle1 = 0
      FDTitle2 = .TRUE.
      Title2 = ' '
      KHiTitle2 = 0
      FDTitle3 = .TRUE.
      Title3 = ' '
      KHiTitle3 = 0
      FDTitle4 = .TRUE.
      Title4 = ' '
      KHiTitle4 = 0
C
      RETURN
      END
      
