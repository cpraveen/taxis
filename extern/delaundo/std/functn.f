      FUNCTION JCYCL (I)
C
C     //////////////////////////////////////////////////////////////
C
C     This function brings the value of I back into the interval
C     [1,3] in a cyclic way.
C
      IMPLICIT INTEGER(I-P)
C
      IM    = MOD(I,3)
      IS = ISIGN(1,(IM-1))
      JCYCL = IM + 3*((1-IS)/2)
C  
      RETURN
      END
C
C     //////////////////////////////////////////////////////////////
C
      FUNCTION JVVR (Node1,Node2)
C
C     //////////////////////////////////////////////////////////////
C
C     Computes the neighbouring Voronoi vertex at the edge
C     formed by Node1 and Node2.
C
      IMPLICIT INTEGER(I-P)
C
C     Switch for case (1,3).
      JDelta = ABS(Node1-Node2)
      JMax = MAX(Node1,Node2)+1
      JMaxCycl = JCYCL(JMax)
C
      JVVR = (2-JDelta)*JMaxCycl-2+JDelta*2
C
      RETURN
      END
C     //////////////////////////////////////////////////////////////
C
      FUNCTION ICYCL (I,NCYC)
C
C     //////////////////////////////////////////////////////////////
C
C     This function brings the value of I back into the interval
C     [1,NCYC] in a cyclic way.
C
C     For instance, if NCYC = 5
C
C      -7 -> 3
C      -6 -> 4
C      -5 -> 5
C      -4 -> 1
C      -3 -> 2
C      -2 -> 3
C      -1 -> 4 
C       0 -> 5
C       1 -> 1
C       2 -> 2
C       3 -> 3
C       4 -> 4
C       5 -> 5
C       6 -> 1
C       7 -> 2
C       8 -> 3
C       9 -> 4
C
C     ..............................................................
C
      IM    = MOD(I,NCYC)
      ICYCL = IM + NCYC*((1-ISIGN(1,(IM-1)))/2)
C  
      RETURN
      END
      
