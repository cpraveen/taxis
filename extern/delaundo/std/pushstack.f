      SUBROUTINE PUSHSTACK (NCell,KNgh,LsNCell,LsKNgh,MLsCell,MStck)
C
C Last update:
C 3Dec92,20:30; derived from PUSHSTACK for MANIP with INTEGER*2.
C 
C Put a cell with a designated face into the bottom of the stack.
C Welcome to DOMINO software.
C
C Input:
C NCell:   Number of the cell to put at the bottom of the stack.
C KNgh:    Number of the neighbor whith which a quad shall be formed.
C LsNCell: Stack of cells.
C LsKNgh:  Stack of neighbors.
C MLsCell: Height of the stack
C MStck:   Maximum height of the stack
C
C Output:
C LsNCell: Updated stack of cells.
C LsKNgh:  Updated stack of neighbors.
C MLsCell: Updated height of the stack.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      DIMENSION LsNCell(*),LsKNgh(*)
C
C     New height of the stack.
      MLsCell = MIN(MLsCell+1,MStck-1)
C
C     Push the list up
      DO Ntry = MLsCell,2,-1
        LsNCell(Ntry) = LsNCell(Ntry-1)
        LsKNgh(Ntry) = LsKNgh(Ntry-1)
      END DO
C
C     Put the new entry at the bottom.
      LsNCell(1) = NCell
      LsKNgh(1) = KNgh
C
      RETURN
      END 

