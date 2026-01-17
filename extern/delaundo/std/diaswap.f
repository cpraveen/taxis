      SUBROUTINE DIASWAP (MCell,NFrmNde,NghCell,XNode,YNode,Q,
     &                    IPar7,ISwap,MCycle,DiaMax2,CosAnglMax)
C
C Last update:
C 9Apr92, 15:20. (upgrade to MinMax, Delaunay, constrained k swap.)
C 1Apr92, 18:10. (swap only up to certain lengths of diagonals.)
C 24Mar92, 17:07. (new PULLSTACK.)
C 24Mar92, 15:33. (fix bug with NCell/NCell1.)
C 27Feb92, 23:11.
C 
C Take an existing grid and swap the diagonals according to different
C criteria. Welcome to DOMINO software.
C
C Input:
C MCell:   Number of cells.
C NFrmNde: Forming nodes of all cells.
C NghCell: Neighboring cells of all cells.
C X/YNode: Coordinates of the nodes.
C Q:       Scalar varialbe at the nodes.
C IPar7:   IPar(7) in SPLIT, different test cases. See there.
C ISwap:   1 if swapping for minimum |k| = |n.s|.
C          2 if swapping for maximum minimum angle.
C          3 if swapping for minimum maximum angle.
C MCycle:  Maximum number of sweep cycles.
C DiaMax2: Maximum tolerable squared length of the diagonal.
C CosAnglMax: Cosine of the maximum tolerable cell angle.
C 
C Output:
C NFrmNde: Updated forming nodes.
C NghCell: Updated neighbors.
C MaxCyc:  Performed number of sweep cycles.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT LOGICAL*1 (F)
      PARAMETER (NODESPEREL=6,MStck=20,MaxVert=30000)
      DIMENSION NFrmNde(NODESPEREL,*),NghCell(NODESPEREL,*),XNode(*),
     &          YNode(*),Q(4,*),LsNCell(MStck),LsKNgh(MStck),
     &          FCellChk(Nodesperel,2*MaxVert)
C
C     Loop over all cells.
      FoundSwap = .TRUE.
      NCycle = 0
      DO WHILE (FoundSwap.AND.NCycle.LT.MCycle)
        FoundSwap = .FALSE.
        NCycle = NCycle+1
        MSwap = 0
C
C       Loop over all cells as a starter for a thread.
        DO NCell = 1,MCell
C         Initialize parameters.
          NCell1 = NCell
          MLsCell = 0
C         Put all pairings with physical neighbors into the stack.
          IF (.NOT.(NghCell(1,NCell1).EQ.-10.OR.FCellChk(1,NCell1))) 
     &      CALL PUSHSTACK (NCell1,1,LsNCell,LsKNgh,MLsCell,MStck)
          IF (.NOT.(NghCell(2,NCell1).EQ.-10.OR.FCellChk(2,NCell1))) 
     &      CALL PUSHSTACK (NCell1,2,LsNCell,LsKNgh,MLsCell,MStck)
          IF (.NOT.(NghCell(3,NCell1).EQ.-10.OR.FCellChk(3,NCell1))) 
     &      CALL PUSHSTACK (NCell1,3,LsNCell,LsKNgh,MLsCell,MStck)
C           
          DO WHILE (MLsCell.GT.0)
C           Take a pair of triangles out of the stack.
            CALL PULLSTACK (LsNCell,LsKNgh,MLsCell,NghCell,
     &                      NCell1,KNgh1,NCell2,KNgh2,Found)
C
            IF (Found) THEN
C
C             Find out whether the quadrilateral is convex.
              N1 = NFrmNde(KNgh1,NCell1)
              N2 = NFrmNde(JCYCL(KNgh1+1),NCell1)
              N3 = NFrmNde(JCYCL(KNgh1+2),NCell1)
              N4 = NFrmNde(KNgh2,NCell2)
              DX21 = XNode(N2)-XNode(N1)
              DY21 = YNode(N2)-YNode(N1)
              DX41 = XNode(N4)-XNode(N1)
              DY41 = YNode(N4)-YNode(N1)
              DX31 = XNode(N3)-XNode(N1)
              DY31 = YNode(N3)-YNode(N1)
              CrPrd214 = DX21*DY41-DX41*DY21
              CrPrd413 = DX41*DY31-DX31*DY41
              FSwapThis = .FALSE.
C
              IF (CrPrd214.GT.1.E-7.AND.CrPrd413.GT.1.E-7) THEN
C               It is convex. 
C
                IF (ISwap.EQ.1.OR.ISwap.EQ.3) THEN
C                 Swap for min |k|. 
C
C                 Length of the new diagonal 41.
                  D41Sq = DX41**2+DY41**2
                  IF (D41Sq.LT.DiaMax2) THEN
C                   New diagonal is not too long. Test for alignment.
C
C                   Calculate a simple average position, the midpoint
C                   between both midpoints of the two diagonals 2-3,1-4.
                    XAvg = .25*(XNode(N1)+XNode(N2)+XNode(N3)+XNode(N4))
                    YAvg = .25*(YNode(N1)+YNode(N2)+YNode(N3)+YNode(N4))
                    QAvg = .25*(Q(1,N1)+Q(1,N2)+Q(1,N3)+Q(1,N4))
C                   Calculate scalar advection speed.
                    CALL ADVCTSCL (XAvg,YAvg,QAvg,IPar7,A,B)
C                   Calculate k's.
                    C23 = ABS(A*(YNode(N3)-YNode(N2))+
     &                      B*(XNode(N2)-XNode(N3)))
                    C14 = ABS(A*(YNode(N4)-YNode(N1))+
     &                      B*(XNode(N1)-XNode(N4)))
C
                    IF (C14.LT.C23) THEN
C                     The new diagonal is better aligned.
C
                      IF (ISwap.EQ.1) THEN
C                       Just minimize k's
                        FSwapThis = .TRUE.
C
                      ELSE
C                       Check for maximum angles.
                        CALL COSANGL(N1,N2,N3,N4,XNode,YNode,
     &                               Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,
     &                               Cos7,Cos8,Cos9,Cos10,Cos11,Cos12)
                        CosMinOl = MIN(Cos1,Cos2,Cos3,Cos4,Cos5,Cos6)
                        CosMinNw = MIN(Cos7,Cos8,Cos9,Cos10,Cos11,Cos12)
C                       Allow a swap only if the maximum angle in the
C                       new swapped configuration is smaller than 
C                       AnglMax or if it is smaller than the maximum
C                       angle in the old unswapped configuration.
                        IF (CosMinNw.GT.CosAnglMax.OR.
     &                      CosMinNw.GT.CosMinOl) FSwapThis = .TRUE.
C
                      END IF
C
                    END IF
C
                  END IF
C
                ELSE IF (ISwap.EQ.2) THEN
C                 Swap for minimum maximum angle.
                  CALL COSANGL(N1,N2,N3,N4,XNode,YNode,
     &                         Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,
     &                         Cos7,Cos8,Cos9,Cos10,Cos11,Cos12)
                  CosMinOl = MIN(Cos1,Cos2,Cos3,Cos4,Cos5,Cos6)
                  CosMinNw = MIN(Cos7,Cos8,Cos9,Cos10,Cos11,Cos12)
                  IF (CosMinNw.GT.CosMinOl) FSwapThis = .TRUE.
C
                ELSE IF (ISwap.EQ.4) THEN
C                 Swap for maximum minimum angle, Delaunay.
                  CALL COSANGL(N1,N2,N3,N4,XNode,YNode,
     &                         Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,
     &                         Cos7,Cos8,Cos9,Cos10,Cos11,Cos12)
                  CosMaxOl = MAX(Cos1,Cos2,Cos3,Cos4,Cos5,Cos6)
                  CosMaxNw = MAX(Cos7,Cos8,Cos9,Cos10,Cos11,Cos12)
                  IF (CosMaxNw.LT.CosMaxOl) FSwapThis = .TRUE.
C
                END IF
C
                IF (FSwapThis) THEN
C                 Swap this diagonal from 2-3 to 1-4.
                  FoundSwap = .TRUE.
                  MSwap = MSwap+1
C                 Take a note of the KNgh1-1st neighbor to ncell1 
C                 before it is overwritten.
                  NKgh1m1 = NghCell(JCYCL(KNgh1-1),NCell1)
C                 Do the swap, baby, do the swap-swap, hey, hey, yeah,..
                  FCellChk(JCYCL(KNgh1+1),NCell1) = .TRUE.
                  NFrmNde(JCYCL(KNgh1+1),NCell1) = NFrmNde(KNgh2,NCell2)
                  NghCell(JCYCL(KNgh1-1),NCell1) = NCell2
                  NghCell(KNgh1,NCell1) = NghCell(JCYCL(KNgh2-1),NCell2)
                  FCellChk(JCYCL(KNgh2+1),NCell2) = .TRUE.
                  NFrmNde(JCYCL(KNgh2+1),NCell2) = NFrmNde(KNgh1,NCell1)
                  NghCell(JCYCL(KNgh2-1),NCell2) = NCell1
                  NghCell(KNgh2,NCell2) = NKgh1m1
C
C                 Update the neighboring information in the cells 
C                 surrounding the quadrilateral.
C                 Find back the position of NCell2 in NNgh1.
                  NNgh1 = NghCell(KNgh1,NCell1)
                  IF (NNgh1.EQ.-10) THEN
C                   This neighbor is a boundary.
                    FoundNgh = .TRUE.
                  ELSE
                    FoundNgh = .FALSE.
                    KNghNgh = 0
                  END IF
                  DO WHILE (.NOT.FoundNgh)
                    IF (KNghNgh.LT.3) THEN
                      KNghNgh = KNghNgh+1
                      IF (NghCell(KNghNgh,NNgh1).EQ.NCell2) THEN
C                       This the entry to update.
                        FoundNgh = .TRUE.
                        NghCell(KNghNgh,NNgh1) = NCell1
                      END IF
                    ELSE
                      WRITE (*,*) ' OOPS7!'
                      STOP
                    END IF
                  END DO
C                 Find back the position of NCell1 in NNgh2.
                  NNgh2 = NKgh1m1
                  IF (NNgh2.EQ.-10) THEN
C                   This neighbor is a boundary.
                    FoundNgh = .TRUE.
                  ELSE
                    FoundNgh = .FALSE.
                    KNghNgh = 0
                  END IF
                  DO WHILE (.NOT.FoundNgh)
                    IF (KNghNgh.LT.3) THEN
                      KNghNgh = KNghNgh+1
                      IF (NghCell(KNghNgh,NNgh2).EQ.NCell1) THEN
C                       This the entry to update.
                        FoundNgh = .TRUE.
                        NghCell(KNghNgh,NNgh2) = NCell2
                      END IF
                    ELSE
                      WRITE (*,*) ' OOPS8!'
                      STOP
                    END IF
                  END DO
C
C                 Put all four neighbors to this quadrilateral in the
C                 stack.
                  IF (NghCell(KNgh1,NCell1).NE.-10) THEN
                    CALL PUSHSTACK (NCell1,KNgh1,
     &                              LsNCell,LsKNgh,MLsCell,MStck)
                    FCellChk(KNgh1,NCell1) = .FALSE.
                  END IF
                  IF (NghCell(JCYCL(KNgh1+1),NCell1).NE.-10) THEN
                    CALL PUSHSTACK (NCell1,JCYCL(KNgh1+1),
     &                            LsNCell,LsKNgh,MLsCell,MStck)
                    FCellChk(JCYCL(KNgh1+1),NCell1) = .FALSE.
                  END IF
                  IF (NghCell(KNgh2,NCell2).NE.-10) THEN
                    CALL PUSHSTACK (NCell2,KNgh2,
     &                            LsNCell,LsKNgh,MLsCell,MStck)
                    FCellChk(KNgh2,NCell2) = .FALSE.
                  END IF
                  IF (NghCell(JCYCL(KNgh2+1),NCell2).NE.-10) THEN
                    CALL PUSHSTACK (NCell2,JCYCL(KNgh2+1),
     &                            LsNCell,LsKNgh,MLsCell,MStck)
                    FCellChk(JCYCL(KNgh2+1),NCell2) = .FALSE.
                  END IF
C
                ELSE
C                 Mark this pair as checked.
                  FCellChk(KNgh1,NCell1) = .TRUE.
                  FCellChk(KNgh2,NCell2) = .TRUE.
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
        IF (NCycle.EQ.1) THEN
          Write (*,'(2(I7,A))') MSwap,' swaps in',NCycle,'st sweep.'
        ELSE IF (NCycle.EQ.2) THEN
          Write (*,'(2(I7,A))') MSwap,' swaps in',NCycle,'nd sweep.'
        ELSE IF (NCycle.EQ.3) THEN
          Write (*,'(2(I7,A))') MSwap,' swaps in',NCycle,'rd sweep.'
        ELSE 
          Write (*,'(2(I7,A))') MSwap,' swaps in',NCycle,'th sweep.'
        END IF
C
      END DO
C
      RETURN
      END
C

