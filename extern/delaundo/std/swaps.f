      SUBROUTINE SWAPS (MVvr,NFrmNde,NghVvr,XNode,YNode,MCyclSwap,
     &                  AnglMax,FlsOutVv,FVvrChk,MStck,LsNVvr,LsKNgh,
     &                  IVerbose,NtLog,FLogFile,ISwap)
C
C Last update:
C 
C
C Take an existing grid and swap the diagonals for MinMax or MaxMin
C angle. Welcome to DOMINO software.
C
C Input:
C MVvr:    Number of cells.
C NFrmNde: Forming nodes of all cells.
C NghVvr:  Neighboring Vvrs of all Vvrs.
C X/YNode: Coordinates of the nodes.
C MCyclSwap:  Maximum number of sweep cycles.
C AnglMax: Maximum tolerable cell angle in degrees for MinMax.
C FlsOutVv: Flag vector for triangles outside the domain.
C FVvrChk: Workarray of at least 3 times MVvr.
C MStck:   Dimension for LsNVvr, LsKNgh.
C LsNVvr:  Workspace of at least MStck for the swapstack.
C LsKNgh:  Workspace of at least MStck for the swapstack.
C IVerbose: 
C NtLog: 
C FLogFile: 
C ISwap:   0: swap MaxMin, ie. Delaunay,
C          1: swap MinMax.
C 
C Output:
C NFrmNde: Updated forming nodes.
C NghVvr: Updated neighbors.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1 (F)
      DOUBLE PRECISION XNode(*),YNode(*)
      DIMENSION NFrmNde(3,*),NghVvr(3,*),LsNVvr(*),LsKNgh(*),
     &          FVvrChk(3,*),FlsOutVv(*)
C
      IF (ISwap.EQ.1) THEN
        IF (FLogFile) WRITE (NtLog,11) AnglMax
        IF (IVerbose.GE.3) WRITE (*,11) AnglMax
 11     FORMAT (/3X,' Swapping diagonals to reduce angles to less',
     &          ' than',F5.0,' degrees.')
C       Cosine of the maximum tolerable angle.
        CosMinSwap = COS(AnglMax/180.*3.14159)
C
      ELSE IF (ISwap.EQ.0) THEN
        IF (FLogFile) WRITE (NtLog,12) 
        IF (IVerbose.GE.3) WRITE (*,12) 
 12     FORMAT (/3X,' Swapping diagonals for Delaunay.')
C
      ELSE
        IF (FLogFile) WRITE (NtLog,13) ISwap
        IF (IVerbose.GE.3) WRITE (*,13) ISwap
 13     FORMAT (/' FATAL in SWAPS: Unknown value for ISwap:',I7)
        STOP
C
      END IF
C
C     Loop over all cells.
      FoundSwap = .TRUE.
      NCycle = 0
      DO WHILE (FoundSwap.AND.NCycle.LT.MCyclSwap)
        FoundSwap = .FALSE.
        NCycle = NCycle+1
        MSwap = 0
C
C       Loop over all cells as a starter for a thread.
        DO NVvr = 1,MVvr
C         Initialize parameters.
          NVvr1 = NVvr
          MLsVvr = 0
          IF (.NOT.FlsOutVv(NVvr)) THEN
C           This is an internal cell. Put all pairings with physical
C           and internal neighbors into the stack.
            IF (.NOT.(NghVvr(1,NVvr1).EQ.-10.OR.FVvrChk(1,NVvr1).OR. 
     &                FlsOutVv(NghVvr(1,NVvr1)))) 
     &        CALL PUSHSTACK (NVvr1,1,LsNVvr,LsKNgh,MLsVvr,MStck)
            IF (.NOT.(NghVvr(2,NVvr1).EQ.-10.OR.FVvrChk(2,NVvr1).OR. 
     &                FlsOutVv(NghVvr(2,NVvr1)))) 
     &        CALL PUSHSTACK (NVvr1,2,LsNVvr,LsKNgh,MLsVvr,MStck)
            IF (.NOT.(NghVvr(3,NVvr1).EQ.-10.OR.FVvrChk(3,NVvr1).OR. 
     &                FlsOutVv(NghVvr(3,NVvr1)))) 
     &        CALL PUSHSTACK (NVvr1,3,LsNVvr,LsKNgh,MLsVvr,MStck)
          END IF
C           
          DO WHILE (MLsVvr.GT.0)
C           Take a pair of triangles out of the stack.
            CALL PULLSTACK (LsNVvr,LsKNgh,MLsVvr,NghVvr,
     &                      NVvr1,KNgh1,NVvr2,KNgh2,Found)
C
            IF (Found) THEN
C
C             Find out whether the quadrilateral is convex.
              N1 = NFrmNde(KNgh1,NVvr1)
              N2 = NFrmNde(JCYCL(KNgh1+1),NVvr1)
              N3 = NFrmNde(JCYCL(KNgh1+2),NVvr1)
              N4 = NFrmNde(KNgh2,NVvr2)
              DX21 = XNode(N2)-XNode(N1)
              DY21 = YNode(N2)-YNode(N1)
              DX41 = XNode(N4)-XNode(N1)
              DY41 = YNode(N4)-YNode(N1)
              DX31 = XNode(N3)-XNode(N1)
              DY31 = YNode(N3)-YNode(N1)
              CrPrd214 = DX21*DY41-DX41*DY21
              CrPrd413 = DX41*DY31-DX31*DY41
              FSwap = .FALSE.
C
              IF (CrPrd214.GT.1.E-7.AND.CrPrd413.GT.1.E-7) THEN
C               It is convex. Try to swap for minimum maximum angle.
                CALL COSANGL(N1,N2,N3,N4,XNode,YNode,
     &                       Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,
     &                       Cos7,Cos8,Cos9,Cos10,Cos11,Cos12)

                IF (ISwap.EQ.1.) THEN
C                 MinMax.
                  CosMinOl = MIN(Cos1,Cos2,Cos3,Cos4,Cos5,Cos6)
                  CosMinNw = MIN(Cos7,Cos8,Cos9,Cos10,Cos11,Cos12)
                  IF (CosMinNw.GT.CosMinOl.AND.
     &                CosMinOl.LT.CosMinSwap) FSwap = .TRUE.
                ELSE 
C                 MaxMin.
                  CosMaxOl = MAX(Cos1,Cos2,Cos3,Cos4,Cos5,Cos6)
                  CosMaxNw = MAX(Cos7,Cos8,Cos9,Cos10,Cos11,Cos12)
                  IF (CosMaxNw.LT.CosMaxOl) FSwap = .TRUE.
C
                END IF
C
                IF (FSwap) THEN
C                 Swap this diagonal from 2-3 to 1-4.
                  CALL SWAPDIAG (NVvr1,KNgh1,NVvr2,KNgh2,
     &                           NFrmNde,NghVvr,FlsOutVv,FVvrChk,
     &                           FoundSwap,MSwap,
     &                           IVerbose,FLogFile,NtLog)
C
C                 Put all four neighbors to this quadrilateral in the
C                 stack.
                  Ngh = NghVvr(KNgh1,NVvr1)
                  IF (Ngh.NE.-10.AND..NOT.FlsOutVv(Ngh)) THEN
                    CALL PUSHSTACK (NVvr1,KNgh1,
     &                              LsNVvr,LsKNgh,MLsVvr,MStck)
                    FVvrChk(KNgh1,NVvr1) = .FALSE.
                  END IF
                  Ngh = NghVvr(JCYCL(KNgh1+1),NVvr1)
                  IF (Ngh.NE.-10.AND..NOT.FlsOutVv(Ngh)) THEN
                    CALL PUSHSTACK (NVvr1,JCYCL(KNgh1+1),
     &                              LsNVvr,LsKNgh,MLsVvr,MStck)
                    FVvrChk(JCYCL(KNgh1+1),NVvr1) = .FALSE.
                  END IF
                  Ngh = NghVvr(KNgh2,NVvr2)
                  IF (Ngh.NE.-10.AND..NOT.FlsOutVv(Ngh)) THEN
                    CALL PUSHSTACK (NVvr2,KNgh2,
     &                              LsNVvr,LsKNgh,MLsVvr,MStck)
                    FVvrChk(KNgh2,NVvr2) = .FALSE.
                  END IF
                  Ngh = NghVvr(JCYCL(KNgh2+1),NVvr2)
                  IF (Ngh.NE.-10.AND..NOT.FlsOutVv(Ngh)) THEN
                    CALL PUSHSTACK (NVvr2,JCYCL(KNgh2+1),
     &                              LsNVvr,LsKNgh,MLsVvr,MStck)
                    FVvrChk(JCYCL(KNgh2+1),NVvr2) = .FALSE.
                  END IF
C
                ELSE
C                 Mark this pair as checked.
                  FVvrChk(KNgh1,NVvr1) = .TRUE.
                  FVvrChk(KNgh2,NVvr2) = .TRUE.
C
                END IF
C
              END IF
C
            END IF
C
          END DO
C
        END DO
C
C       Status.
        IF (NCycle.EQ.1.AND.IVerbose.GE.3) THEN
          Write (*,'(6X,2(I7,A))') MSwap,' swaps in',NCycle,'st sweep.'
        ELSE IF (NCycle.EQ.2.AND.IVerbose.GE.3) THEN
          Write (*,'(6X,2(I7,A))') MSwap,' swaps in',NCycle,'nd sweep.'
        ELSE IF (NCycle.EQ.3.AND.IVerbose.GE.3) THEN
          Write (*,'(6X,2(I7,A))') MSwap,' swaps in',NCycle,'rd sweep.'
        ELSE  IF (IVerbose.GE.3) THEN
          Write (*,'(6X,2(I7,A))') MSwap,' swaps in',NCycle,'th sweep.'
        END IF
        IF (NCycle.EQ.1.AND.FLogFile) THEN
          Write (NtLog,'(6X,2(I7,A))') 
     &           MSwap,' swaps in',NCycle,'st sweep.'
        ELSE IF (NCycle.EQ.2.AND.FLogFile) THEN
          Write (NtLog,'(6X,2(I7,A))') 
     &           MSwap,' swaps in',NCycle,'nd sweep.'
        ELSE IF (NCycle.EQ.3.AND.FLogFile) THEN
          Write (NtLog,'(6X,2(I7,A))') 
     &           MSwap,' swaps in',NCycle,'rd sweep.'
        ELSE  IF (FLogFile) THEN
          Write (NtLog,'(6X,2(I7,A))') 
     &           MSwap,' swaps in',NCycle,'th sweep.'
        END IF
C
      END DO
C
      RETURN
      END
C

