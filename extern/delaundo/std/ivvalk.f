      SUBROUTINE IVVALK (X,Y,NVvr,NNdePvt,
     &                   XVvr,YVvr,NFrmNde,NghVvr,MVvr,
     &                   FOutCnv,ILevel,NghLevelMax)
C
C Last update:
C 21Nov96; fix STUPID, really STUPID, typo in cross-product evaluation
C          of the convex-hull check. Arggghhh!
C 26Aug96; normalize the crossproducts, fix bug in crossproduct on
C          the edge of the convex hull.
C 21May96; place upper bounds, trace the pivot, return with a zero
C          NNdePvt from endless loops to call BOXWALK.
C 7Mar95; extend the walk around the vertices of the convex hull.
C 29Apr94,14:33;collect NghLevelMax.
C
C Find the Voronoi vertex closest to the target by walking on the 
C Voronoi diagram and the highest grid level found in all neighboring
C grid vertices connected to NNdePvt, including NNdePvt.
C
C Input:
C X:        X coordinate of the point to check for.
C Y:        Y coordinate of the point to check for.
C NVvr:     Mesh cell to start checking.
C X/YVVr:   Vectors of x/y-coordinates of the Voronoi vertices.
C NFrmNde:  Array of forming nodes of the triangulation.
C NghVvr:   Array of neighbouring cells of the triangulation.
C MVvr:     Maximum number of cells to walk over.
C ILevel:   Vector of grid levels for all nodes.
C
C Output:
C NVvr:     Voronoi vertex closest to the point XY. 0 on failure.
C NNdePvt:  Vertex closest to the point XY.
C FOutCnv:  Flag indicating that search path left the convex hull
C           when set to true.
C NghLevelMax: Highest grid level found in all vertices connected to
C           NNdePvt.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      DIMENSION NFrmNde(3,*),NghVvr(3,*),CrPrd(5),ILevel(*)
      DOUBLE PRECISION X,Y,XVvr(*),YVvr(*)
      DOUBLE PRECISION DXTrgt,DYTrgt,CrPrd,DXNgh,DYNgh,XPrd,
     &                 Zero,ZeroFrac,DXNxt,DYNxt,DTrgt,DNgh,DNxt
      PARAMETER (Zero=1.D-12)
C     PARAMETER (FPrintWarnings=.FALSE.)
C
C     Make sure NVvr is still valid.
      NVvr = MIN(NVvr,MVvr)
      FOutCnv = .FALSE.
      MTest = 0
C
C     Make sure NVvr is inside the domain.
      IF (NFrmNde(1,NVvr).LE.0.OR.NFrmNde(2,NVvr).LE.0.OR.
     &    NFrmNde(3,NVvr).LE.0) THEN
        FOutCnv = .TRUE.
        RETURN
      END IF
C
C     Loop as long as the Dirichlet tile is not found.
      Found = .FALSE.
      KSwitch = 0
      DO WHILE (.NOT.Found)
C
C       This is a switch from one pivoting node to another.
C       Find the sector, ie. the Dirchlet tile with which to
C       proceed.
C       Calculate the three vectors along the Voronoi diagram
C       and normalize.
        DXTrgt = X-XVvr(NVvr)
        DYTrgt = Y-YVvr(NVvr)
        DTrgt = 1.D0/SQRT( DXTrgt*DXTrgt + DYTrgt*DYTrgt )
        DO KNgh = 1,3
          NNgh = NghVvr(KNgh,NVvr)
          DXNgh = XVvr(NNgh)-XVvr(NVvr)
          DYNgh = YVvr(NNgh)-YVvr(NVvr)
          DNgh = DXNgh*DXNgh + DYNgh*DYNgh
          IF ( DNgh*DTrgt .LT. Zero ) THEN
            CrPrd(KNgh) = 0.
          ELSE
            DNgh = 1.D0/SQRT( DNgh )
            CrPrd(KNgh) = DTrgt*DNgh*( DXTrgt*DYNgh-DYTrgt*DXNgh )
          END IF
        END DO
        CrPrd(4) = CrPrd(1)
        CrPrd(5) = CrPrd(2)
C
C       Find the sector that contains DXYTrgt. This is the one where
C       the crossproduct changes sign from negative to positive.
C       Reset the maximum grid level of all nodes connected to 
C       NNdePvt.
        FoundSwitch = .FALSE.
        DO KNgh = 1,3
          IF (CrPrd(KNgh).LT.Zero.AND.CrPrd(KNgh+1).GE.0.) THEN
            FoundSwitch = .TRUE.
            NNdePvt = NFrmNde(JCYCL(KNgh+2),NVvr)
            NghLevelMax = ILevel(NNdePvt)
            NVvrNxt = NghVvr(KNgh,NVvr)
            NVvrTrgt = NVvr
            NNdeEdge = NFrmNde(JCYCL(KNgh+1),NVvr)
            NghLevelMax = MAX(NghLevelMax,ILevel(NNdeEdge))

            MTest = MTest+1
            IF (MTest.GT.MVvr) THEN
C              IF ( FPrintWarnings ) THEN
C                WRITE (*,'(2A/A,2E14.6)') 
C     &               ' WARNING: IVVALK caught in an endless loop in',
C     &               ' search for',' X,Y=',X,Y
C              END IF
              NNdePvt = 0
              RETURN
            END IF

          END IF
        END DO

        IF ( .NOT. FoundSwitch ) THEN
C         There was no switch in signs of the crossproduct. This
C         may occur on non-delaunay triangulations. Revert to
C         BOXWALK.
C          IF ( FPrintWarnings ) THEN
C            WRITE (*,'(A,I7/A,2E14.6)')
C     &           ' WARNING: no x-product switch in cell',NVvr,
C     &           '          for',x,y
C          END IF
C         Pick the next neighbor
          KSwitch = KSwitch+1
          IF ( KSwitch .EQ. 4 ) THEN
C           Tried all neighbors, BOXWALK.
            NNdePvt = 0
            RETURN
          END IF
          
          KNgh = KSwitch
          NNdePvt = NFrmNde(JCYCL(KNgh+2),NVvr)
          NghLevelMax = ILevel(NNdePvt)
          NVvrNxt = NghVvr(KNgh,NVvr)
          NVvrTrgt = NVvr
          NNdeEdge = NFrmNde(JCYCL(KNgh+1),NVvr)
          NghLevelMax = MAX(NghLevelMax,ILevel(NNdeEdge))
          
        END IF
C
C       Walk counterclockwise around NNdePvt as long as the target XY
C       is to the left of the vector to the next VVr.
        XPrd = -1.D25
        MChk = 0
        DO WHILE (XPrd.LT.Zero.AND.NVvrTrgt.NE.NVvrNxt)
          MChk = MChk+1
          IF (MChk.GT.MVvr) THEN
C            IF ( FPrintWarnings ) THEN
C              WRITE (*,'(2A/A,2E14.6)/A,I7') 
C     &             ' WARNING: IVVALK caught in an endless loop',
C     &             '        in search for',' X,Y=',X,Y,
C     &             '        around node',NNdePvt
C            END IF
            NNdePvt = 0
            RETURN
          END IF

          IF (NVvrNxt.GT.MVvr) THEN
C            WRITE (*,'(2(A,I7))')
C     &           ' FATAL: Current cell number',NVvrNxt,
C     &           ' larger than the total number',MVvr
C            STOP
            NNdePvt = 0
            RETURN
            
          ELSE IF ( NVvrNxt .LT. 1 ) THEN
C            WRITE (*,'(A)')
C     &            ' FATAL: IVVALK ran into a negative cell number.'
C            STOP
            NNdePvt = 0
            RETURN
            
          ELSE
            NVvr = NVvrNxt
          ENDIF
          
          IF (NVvr.GT.4) THEN
            FoundPvt = .FALSE.
                            
            DXTrgt = X-XVvr(NVvr)
            DYTrgt = Y-YVvr(NVvr)
C           The target can very well coincide with a circumcenter.
            DTrgt = DXTrgt*DXTrgt + DYTrgt*DYTrgt
            IF ( DTrgt .EQ. 0 ) THEN
              DTrgt = 1.
            ELSE
              DTrgt = 1.D0/SQRT( DTrgt )
            END IF

            DO KNgh = 1,3
              IF (NFrmNde(KNgh,NVvr).EQ.NNdePvt) THEN
                FoundPvt = .TRUE.
                NVvrNxt = NghVvr(JCYCL(KNgh+1),NVvr)
                NNdeEdge = NFrmNde(JCYCL(KNgh+2),NVvr)
                NghLevelMax = MAX(NghLevelMax,ILevel(NNdeEdge))

                DXNgh = XVvr(NVvrNxt)-XVvr(NVvr)
                DYNgh = YVvr(NVvrNxt)-YVvr(NVvr)
C               On regular meshes, the circumcenters can coincide.
                DNgh = DXNgh*DXNgh + DYNgh*DYNgh
                IF ( DNgh*DTrgt .LT. Zero ) THEN
                  XPrd = 0.
                ELSE
                  DNgh = 1.D0/SQRT( DNgh )
                  XPrd = DTrgt*DNgh*( DXTrgt*DYNgh-DYTrgt*DXNgh )
                END IF
              END IF
            END DO

            IF ( .NOT. FoundPvt ) THEN
C              IF ( FPrintWarnings ) THEN
C                WRITE (*,'(A,I7,A)') ' FATAL: Lost the pivot node',
C     &               NNdePvt,' in ivvalk''s inner loop.'
C                write (*,'(A,I7,A)') '    nvvr:',nvvr,' nde/ngh:'
C                write (*,'(4X,2I7)')
C     &               ( nfrmnde(knde,nvvr),nghvvr(knde,nvvr),
C     &                 knde=1,3)
C              END IF
              NNdePvt = 0
              RETURN
C              STOP
            END IF
            
          ELSE
C           NVvr is a boundary edge of the convex hull. Try to
C           wrap NVvr around to the next edge clockwise and check whether
C           the sector between the two eges contains the target.
            NVvrNxt = ICYCL( NVvr+1, 4 )

C           Neighboring node on the convex hull clockwise, that is
C           along cell NVvrNxt. Keep in mind that XYVvr(..4) stores
C           the nodal coordinates.
            NNdeNxt = ICYCL( NNdePvt+1, 4 )
            DXNxt = XVvr(NNdeNxt) - XVvr(NNdePvt)
            DYNxt = YVvr(NNdeNxt) - YVvr(NNdePvt)
            DNxt = 1.D0/SQRT( DXNxt*DXNxt + DYNxt*DYNxt )

C           Vector from NNdePvt to the Target.
            DXTrgt = X - XVvr(NNdePvt)
            DYTrgt = Y - YVvr(NNdePvt)
C           This is the only vector that can become zero.
            DTrgt = DXTrgt*DXTrgt + DYTrgt*DYTrgt
C           Crossproducts.
            IF ( DTrgt .EQ. 0.0 ) THEN
              XPrd = 0.
            ELSE
              DTrgt = 1.D0/SQRT( DTrgt )
              XPrd = DNxt*DTrgt*( DXTrgt*DYNxt - DYTrgt*DXNxt )
            END IF

            IF ( XPrd .GT. 0. ) THEN
C             Target is not in the convex hull.
              FOutCnv = .TRUE.
              RETURN

            ELSE
C             Go on with the interior neighbor to NVvrNxt.
              NVvrNxt = NghVvr(1,NVvrNxt)
              
            END IF
            
          END IF
        END DO
C
C       Check whether the loop has been completed. In that case,
C       all cross-products were negative and the target is inside
C       the Dirichlet tile of NNdePvt. Accept the ones that are on
C       an edge in the Dirichlet tesselation as well, since they are
C       equally close to either node on either side of the edge.
        IF (XPrd.LT.Zero) Found = .TRUE.
C
      END DO
C
      RETURN
      END




