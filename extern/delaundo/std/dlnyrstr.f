      SUBROUTINE DLNYRSTR (MVvr,NFrmNde,NghVvr,XVvr,YVvr,RadVvr,
     &                     XNode,YNode,MCyclSwap,
     &                     FlsOutVv,FVvrChk,MStck,LsNVvr,LsKNgh,
     &                     IVerbose,NtLog,FLogFile)
C
C Last update:
C 
C
C Restore a given grid to Delaunay, i.e. enforce the MaxMin condition.
C on the angles. Welcome to DOMINO software.
C
C Input:
C MVvr:    Number of cells.
C NFrmNde: Forming nodes of all cells.
C NghVvr:  Neighboring Vvrs of all Vvrs.
C X/Y/RadVvr:
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
C 
C Output:
C NFrmNde: Updated forming nodes.
C NghVvr: Updated neighbors.
C X/Y/RadVvr:
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1 (F)
      DOUBLE PRECISION XNode(*),YNode(*),XVvr(*),YVvr(*),RadVvr(*)
      DIMENSION NFrmNde(3,*),NghVvr(3,*),LsNVvr(*),LsKNgh(*),
     &          FVvrChk(3,*),FlsOutVv(*)
C
C     Swap all non-Delaunay diagonals.
      AnglMax = 0.
      ISwap = 0
      CALL SWAPS (MVvr,NFrmNde,NghVvr,XNode,YNode,MCyclSwap,
     &            AnglMax,FlsOutVv,FVvrChk,MStck,LsNVvr,LsKNgh,
     &            IVerbose,NtLog,FLogFile,ISwap)
C
      DO NVvr = 5,MVvr
        CALL CALCVVR (NFrmNde(1,NVvr),XVvr(NVvr),YVvr(NVvr),
     &                RadVvr(NVvr),FLinDeg)
      END DO
C
      RETURN
      END
