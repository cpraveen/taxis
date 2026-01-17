      SUBROUTINE HULDPL (MVvr,NFrmNde,NghVvr,BkgSpc,BkgStr,FBkgStr,
     &                   Mnde,XNode,YNode,
     &                   R,FBkgrd,NtWrt)
C
C     Last update:
C     18May96; remove nodes.cbl
C     17Feb92, 15:17. (plot background stretching parameters.)
C     2Feb92, 0:20. (write only boundary segments 1:4.)
C     6Jul91, 16:55. (Passing grid as argument and plotting BkgSpc.)
C
C     Writes the convex hull or the background grid in .DPL format.
C     Welcome to DOMINO software.
C
C     Input:
C     MVVr:      Number of Voronoi vertices.
C     NFrmNde:   Array of the three forming nodes of each Vvr.
C     NghVVr:    Array of the there neighboring VVr to each Vvr.
C     BkgSpc:    Background spacing to be plotted at the vertices.
C     BkgStr:    Background stretching to be plotted at the vertices.
C     FBkgStr:   .T. if the corresponding background cell is stretched.
C     MNde:      Number of grid vertices.
C     X/YNode:   Vertex coordinates.
C     R:         Work array of at least MNde.
C     NtWrt:     Logical name of the output unit.
C     FBkg:      .TRUE. when background values to be plotted.
C     /NODES/:   Coordinates of the nodes, Index of boundaries, etc. 
C                See main program DELAUNDO for details.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION NghVvr(3,*),NFrmNde(3,*),BkgSpc(*),BkgStr(*),FBkgStr(*),
     &          XNode(*),YNode(*),R(*)
C
      IF (FBkgrd) THEN
C       Distribute the cellwise FBkgStr to the nodes.
        DO NNde = 1,MNde
          R(NNde) = 0.1
        END DO
        DO NVvr = 1,MVvr
          IF (FBkgStr(NVvr)) THEN
            R(NFrmNde(1,NVvr)) = 1.
            R(NFrmNde(2,NVvr)) = 1.
            R(NFrmNde(3,NVvr)) = 1.
          END IF
        END DO
      END IF
C
C     Write to .DPL file:
C     Header.
      WRITE (NTWRT,'(A22)') 'unstructured grid data'
C
C     Number of interior cells,.
      WRITE (NtWrt,'(I5,2I2)') MVvr

C     Outer boundary of setup hull. Note that for whatever
C     reason, VKBPlot requires all your cells to have a positive
C     volume. The following won't work with that.
      DO NVvr = 1,4
        WRITE (NtWrt,'(I1,1X,2I6,2(2X,I6))') 2,NFrmNde(2,NVvr),
     &                                    NFrmNde(3,NVvr),
     &                                    NghVvr(1,NVvr),NVvr
      END DO

      DO NVvr = 5,MVvr
        WRITE (NtWrt,'(I1,1X,3(3I6,2X))') 3,NFrmNde(1,NVvr),
     &                                    NFrmNde(2,NVvr),
     &                                    NFrmNde(3,NVvr),
     &                                    NghVvr(1,NVvr),
     &                                    NghVvr(2,NVvr),
     &                                    NghVvr(3,NVvr),NVvr
      END DO
C
C     Number of nodes ignoring the 4 nodes of the setup hull.
      WRITE (NtWrt,'(I6)') MNde
C     Conditions at infinity.
      WRITE (NtWrt,'(4F3.0)') 1.,1.,1.,1.
C
C     Loop over nodes.
      IF (FBkgrd) THEN
C       Plot the background values.
        DO NNde = 1,4
          WRITE (NtWrt,'(2E15.7,3F3.0,F7.0)') XNode(NNde),YNode(NNde),
     &                                        1.,1.,1.,FLOAT(NNde)
        END DO
        DO NNde = 5,MNde
          Bsp = BkgSpc(NNde)
          U = BkgStr(NNde)
          WRITE (NtWrt,'(5E15.7,F7.0)') XNode(NNde),YNode(NNde),
     &                                       Bsp,Bsp*U,Bsp*R(NNde),
     &                                       FLOAT(NNde)
        END DO
C
      ELSE
C       Plot the full foreground grid.
        DO NNde = 1,MNde
          WRITE (NtWrt,'(2E15.7,3F3.0,F7.0)') XNode(NNde),YNode(NNde),
     &                                        1.,1.,1.,FLOAT(NNde)
        END DO
C
      END IF
C
C     Bodies/Elements,
      WRITE (NtWrt,'(I3)') 1
C     Bodyfaces.
      WRITE (NtWrt,'(I3)') 4
      WRITE (NtWrt,'(2I7)') 1,4        
      WRITE (NtWrt,'(2I7)') 2,1        
      WRITE (NtWrt,'(2I7)') 3,2        
      WRITE (NtWrt,'(2I7)') 4,3        
C
      WRITE (NtWrt,'(2I3)') 0
C
      RETURN
      END
