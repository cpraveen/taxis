C     Last update:
C     5May92, 16:30. (extend NvNmBn to accomodate IllConn and Stretch.)
C     15Jul91, 15:21. (Introducing AntiConnectivity information.)
C     6Jul91, 15:22. (Introducing IBndType and MaxBndNde.)
C     5Jul91, 19:55. (Eliminating NghNde and rearranging arrays.)
C
C     Common block for DELAUNDO containing information about the nodes
C     and boundaries. Welcome to DOMINO software.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      PARAMETER (MaxNode=50000,MaxBound=25,MaxBndNde=MaxNode/10,
     &           MaxVisBound = 10*MaxBound,MaxLsFlg=2*MaxBndNde)

      PARAMETER (MStck=100,MaxFrontEl=MaxBndNde,RadTolRel=1.D-11)

      COMMON /NODES/ XNode(MaxNode),YNode(MaxNode),
     &               MBnd,NmeBnd(MaxBound),NvNmBn(-1:MaxBound+2),
     &               NmNghFNd(MaxBound),NmNghLNd(MaxBound),
     &               IBndType(MaxVisBound),MNtNde,MNde,
     &               NdxLBnNd(0:MaxVisBound),LmtNde(MaxBound),MLmt,
     &               IFront(MaxVisBound)
