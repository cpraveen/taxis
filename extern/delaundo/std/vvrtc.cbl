C     Last update:
C     29Jan92, 17:35. (include RadTol.)
C     5Jul91, 19:58. (Rearranging.)
C
C     Common block for DELAUNDO containing the triangulation and
C     the defined thickness of the circumcircles. Welcome to DOMINO
C     software.
C
      COMMON /VVRTC/ XVvr(2*MaxNode),YVvr(2*MaxNode),
     &               RadVvr(2*MaxNode),RadTol,RadTolPl,RadTolMi,
     &               MVvr,NFrmNde(3,2*MaxNode),NghVvr(3,2*MaxNode)
