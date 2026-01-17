      SUBROUTINE FNDIAREA (FlsOutVv,NBFrmNde,NdxLBnNd,IBndType,IFront,
     &                     IArea,FNoArea)
C
C Last update:
C 
C
C Determine whether background triangles for viscous meshing are
C connectingproperly, highlight the first violater or assign the
C relation IArea pointing to a certain element for each triangle.
C Welcome to DOMINO software.
C
C Input:
C FlsOutVv: .T. if the triangle is outside the domain.
C NBFrmNde: Vector of forming nodes of the triangle.
C NdxLBnNd: Vector of pointers to last nodes on boundaries.
C IBndType: Vector of boundary types.
C IFront:   Vector of the associated element of each boundary.
C
C Output:
C IArea:    If found, the number of the associated element.
C FNoArea:  .T. if the triangle illconnects.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION NBFrmNde(3),NdxLBnNd(0:*),IBndType(*),IFront(*)
C
      IF (FlsOutVv) THEN
C       This triangle is outside the domain. Don't care.
        IArea = 0
C
      ELSE IF (IArea.LT.0) THEN
C       This triangle has not been dealt with so far. Find the nodes and
C       their boundary numbers.
        IArea = 0
C
        NB1 = NBFrmNde(1)
        NrBnd1 = 1
        DO WHILE (NdxLBnNd(NrBnd1).LT.NB1)
          NrBnd1 = NrBnd1+1
        END DO
        IF (IBndType(NrBnd1).EQ.1.OR.IBndType(NrBnd1).EQ.4) THEN
          FFront1 = .TRUE.
        ELSE
          FFront1 = .FALSE.
        END IF
C    
        NB2 = NBFrmNde(2)
        NrBnd2 = 1
        DO WHILE (NdxLBnNd(NrBnd2).LT.NB2)
          NrBnd2 = NrBnd2+1
        END DO
        IF (IBndType(NrBnd2).EQ.1.OR.IBndType(NrBnd2).EQ.4) THEN
          FFront2 = .TRUE.
        ELSE
          FFront2 = .FALSE.
        END IF
C
        NB3 = NBFrmNde(3)
        NrBnd3 = 1
        DO WHILE (NdxLBnNd(NrBnd3).LT.NB3)
          NrBnd3 = NrBnd3+1
        END DO
        IF (IBndType(NrBnd3).EQ.1.OR.IBndType(NrBnd3).EQ.4) THEN
          FFront3 = .TRUE.
        ELSE
          FFront3 = .FALSE.
        END IF
C
C       Check whether this background triangle connects properly.
        FNoArea = .FALSE.
        IF (FFront1.AND.FFront2.AND.FFront3) THEN
C         Three boundary nodes connect to a flat triangle along
C         the frontal wall that disturbs the interpolation. Crack it.
          FNoArea = .TRUE.
C
C         The two boundary nodes must be on the same element and
C         the third node must be on the outer edge of the viscous layer.
C
        ELSE IF (FFront1.AND.FFront2) THEN
          IF (IFront(NrBnd1).EQ.IFront(NrBnd2).AND.
     &        IBndType(NrBnd3).EQ.10) THEN
            IArea = IFront(NRBnd1)
          ELSE IF (IFront(NrBnd1).EQ.IFront(NrBnd2).AND.
     &             IFront(NrBnd1).EQ.IFront(NrBnd3)) THEN
            IArea = IFront(NRBnd1)
          ELSE
            FNoArea = .TRUE.
          END IF
C
        ELSE IF (FFront2.AND.FFront3) THEN
          IF (IFront(NrBnd2).EQ.IFront(NrBnd3).AND.
     &        IBndType(NrBnd1).EQ.10) THEN
            IArea = IFront(NRBnd2)
          ELSE IF (IFront(NrBnd2).EQ.IFront(NrBnd3).AND.
     &             IFront(NrBnd2).EQ.IFront(NrBnd1)) THEN
            IArea = IFront(NRBnd2)
          ELSE
            FNoArea = .TRUE.
          END IF
C
        ELSE IF (FFront3.AND.FFront1) THEN
          IF (IFront(NrBnd3).EQ.IFront(NrBnd1).AND.
     &        IBndType(NrBnd2).EQ.10) THEN
            IArea = IFront(NRBnd3)
          ELSE IF (IFront(NrBnd3).EQ.IFront(NrBnd1).AND.
     &             IFront(NrBnd3).EQ.IFront(NrBnd2)) THEN
            IArea = IFront(NRBnd3)
          ELSE
            FNoArea = .TRUE.
          END IF
C
C         The two non-frontal nodes must be on the outer edge of
C         the viscous layer.
C
        ELSE IF (FFront1) THEN
          IF ((IBndType(NrBnd2).EQ.10.OR.
     &         IFront(NrBnd2).EQ.IFront(NrBnd1)).AND.
     &        (IBndType(NrBnd3).EQ.10.OR.
     &         IFront(NrBnd3).EQ.IFront(NrBnd1))) THEN
            IArea = IFront(NrBnd1)
          ELSE
            FNoArea = .TRUE.
          END IF
C
        ELSE IF (FFront2) THEN
          IF ((IBndType(NrBnd3).EQ.10.OR.
     &         IFront(NrBnd3).EQ.IFront(NrBnd2)).AND.
     &        (IBndType(NrBnd1).EQ.10.OR.
     &         IFront(NrBnd1).EQ.IFront(NrBnd2))) THEN
            IArea = IFront(NrBnd2)
          ELSE
            FNoArea = .TRUE.
          END IF
C
        ELSE IF (FFront3) THEN
          IF ((IBndType(NrBnd1).EQ.10.OR.
     &         IFront(NrBnd1).EQ.IFront(NrBnd3)).AND.
     &        (IBndType(NrBnd2).EQ.10.OR.
     &         IFront(NrBnd2).EQ.IFront(NrBnd3))) THEN
            IArea = IFront(NrBnd3)
          ELSE
            FNoArea = .TRUE.
          END IF
C
        END IF
      END IF
C
C     Erase IArea if this triangle connects badly.
      IF (FNoArea) IArea = -1
C
      RETURN
      END
 
