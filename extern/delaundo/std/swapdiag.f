      SUBROUTINE SWAPDIAG (NVvr1,KNgh1,NVvr2,KNgh2,
     &                     NFrmNde,NghVvr,FlsOutVv,FVvrChk,
     &                     FoundSwap,MSwap,
     &                     IVerbose,FLogFile,NtLog)
C
C Last update:
C 
C 
C Swap a diagonal in a quadrilateral formed by two triangles. Welcome
C to DOMINO software.
C
C Input:
C NVvr1/2: Numbers of the two cells that share the diagonal to swap.
C NFrmNde: Forming nodes of all cells.
C NghVvr:  Neighboring Vvrs of all Vvrs.
C FlsOutVv: Flag vector for triangles outside the domain.
C FVvrChk: Workarray of at least 3 times MVvr.
C IVerbose:
C FLogFile:
C NtLog:
C 
C Output:
C NFrmNde: Updated forming nodes.
C NghVvr:  Updated neighbors.
C FVvrChk: Workarray of at least 3 times MVvr.
C FoundSwap: .T. on exit.
C MSwap:   Incremented by one on exit.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1 (F)
      DIMENSION NFrmNde(3,*),NghVvr(3,*),FVvrChk(3,*),FlsOutVv(*)
C
C     Swap this diagonal from 2-3 to 1-4.
      FoundSwap = .TRUE.
      MSwap = MSwap+1
C
C     Take a note of the KNgh1-1st neighbor to nVvr1 
C     before it is overwritten.
      NKgh1m1 = NghVvr(JCYCL(KNgh1-1),NVvr1)
C
C     Do the swap, baby, do the swap-swap, hey, hey, yeah,..
      FVvrChk(JCYCL(KNgh1+1),NVvr1) = .TRUE.
      NFrmNde(JCYCL(KNgh1+1),NVvr1) = NFrmNde(KNgh2,NVvr2)
      NghVvr(JCYCL(KNgh1-1),NVvr1) = NVvr2
      NghVvr(KNgh1,NVvr1) = NghVvr(JCYCL(KNgh2-1),NVvr2)
      FVvrChk(JCYCL(KNgh2+1),NVvr2) = .TRUE.
      NFrmNde(JCYCL(KNgh2+1),NVvr2) = NFrmNde(KNgh1,NVvr1)
      NghVvr(JCYCL(KNgh2-1),NVvr2) = NVvr1
      NghVvr(KNgh2,NVvr2) = NKgh1m1
C
C     Update the neighboring information in the cells 
C     surrounding the quadrilateral.
C     Find back the position of NVvr2 in NNgh1.
      NNgh1 = NghVvr(KNgh1,NVvr1)
      IF (NNgh1.EQ.-10) THEN
C       This neighbor is a boundary.
        FoundNgh = .TRUE.
      ELSE
        FoundNgh = .FALSE.
        KNghNgh = 0
      END IF
      DO WHILE (.NOT.FoundNgh)
        IF (KNghNgh.LT.3) THEN
          KNghNgh = KNghNgh+1
          IF (NghVvr(KNghNgh,NNgh1).EQ.NVvr2) THEN
C           This the entry to update.
            FoundNgh = .TRUE.
            NghVvr(KNghNgh,NNgh1) = NVvr1
          END IF
        ELSE
          IF (FLogFile) WRITE (NtLog,11)
          IF (IVerbose.GE.3) WRITE (*,11)
          STOP
 11       FORMAT (/' FATAL in SWAPDIAG: OOPS7!')
        END IF
      END DO
C     Find back the position of NVvr1 in NNgh2.
      NNgh2 = NKgh1m1
      IF (NNgh2.EQ.-10) THEN
C       This neighbor is a boundary.
        FoundNgh = .TRUE.
      ELSE
        FoundNgh = .FALSE.
        KNghNgh = 0
      END IF
      DO WHILE (.NOT.FoundNgh)
        IF (KNghNgh.LT.3) THEN
          KNghNgh = KNghNgh+1
          IF (NghVvr(KNghNgh,NNgh2).EQ.NVvr1) THEN
C           This the entry to update.
            FoundNgh = .TRUE.
            NghVvr(KNghNgh,NNgh2) = NVvr2
          END IF
        ELSE
          IF (FLogFile) WRITE (NtLog,12)
          IF (IVerbose.GE.3) WRITE (*,12)
          STOP
 12       FORMAT (/' FATAL in SWAPDIAG: OOPS8!')
        END IF
      END DO
C
      RETURN
      END
