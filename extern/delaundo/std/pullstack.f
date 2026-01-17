      SUBROUTINE PULLSTACK (LsNCell,LsKNgh,MLsCell,NghCell,
     &                      NCell1,KNgh1,NCell2,KNgh2,Found)
C
C Last update:
C 3Dec92,20:30;derived from PULLSTACK for MANIP with INTEGER*2.
C 
C Pull a pair of cells for a possible swap out of the stack.
C Welcome to DOMINO software.
C
C Input:
C LsNCell: Stack of cells.
C LsKNgh:  Stack of neighbors.
C MLsCell: Height of the stack
C MStck:   Maximum height of the stack
C NghCell: Neighboring cells of all cells.
C
C Output:
C NCell1:  Number of the cell to pull at the bottom of the stack.
C KNgh1:   Postion of the neighbor whith which a quad shall be formed.
C NCell2:  Number of the neighbor.
C KNgh2:   Position of the neighbor in the cell.
C LsNCell: Updated stack of cells.
C LsKNgh:  Updated stack of neighbors.
C MLsCell: Updated height of the stack.
C Found:   .TRUE. if a pair could be extracted from the stack.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      DIMENSION LsNCell(*),LsKNgh(*),NghCell(3,*)
C
      Found = .FALSE.
      DO WHILE (.NOT.Found.AND.MLsCell.GT.0)
C
C       Pull the new entry at the bottom.
        NCell1 = LsNCell(1)
        KNgh1 = LsKNgh(1)
C
C       Pull the list down.
        DO Ntry = 2,MLsCell
          LsNCell(Ntry-1) = LsNCell(Ntry)
          LsKNgh(Ntry-1) = LsKNgh(Ntry)
        END DO
        MLsCell = MAX(MLsCell-1,0)
C
C       Find back the position of NCell1 in NCell2.
        NCell2 = NghCell(KNgh1,NCell1)
        IF (NCell2.NE.-10) THEN
          KNghNgh = 0
          DO WHILE (.NOT.Found.AND.KNghNgh.LT.3)
            KNghNgh = KNghNgh+1
            IF (NghCell(KNghNgh,NCell2).EQ.NCell1) THEN
              Found = .TRUE.
              KNgh2 = KNghNgh
            END IF
          END DO
        END IF
C
      END DO
C
      RETURN
      END 

