C
      
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      SUBROUTINE CAV2DPL ( MFlgVvr, LsFlgVvr, NFrmNde, XNode, YNode )

      IMPLICIT NONE

      INTEGER MFlgVvr, LsFlgVvr(*), NFrmNde(3,*), KVvr, KVx, NVvr, NNde
      DOUBLE PRECISION XNode(*), YNode(*)

      OPEN ( 11, FILE='cavity.dpl' )
      REWIND 11

      WRITE (11,'(A)') 'unstruct'
      WRITE (11,*) MFlgVvr

      DO KVvr = 1,MFlgVvr
        WRITE (11,'(4I4,I8)')
     &        3,3*(KVvr-1)+1,3*(KVvr-1)+2,3*(KVvr-1)+3,LsFlgVvr(KVvr)
      END DO

      WRITE (11,*) 3*MFlgVvr
      WRITE (11,'(A)') '1. 1. 0. 29.'
      DO KVvr = 1,MFlgVvr
        NVvr = LsFlgVvr(KVvr)
        DO KVx = 1,3
          NNde = NFrmNde(KVx,NVvr)
          WRITE (11,'(2E14.7,F9.0,A)')
     &          XNode(NNde),YNode(NNde),FLOAT(NNde),' 1. 0. 29.'
        END DO
      END DO

      WRITE (11,*) 0
      WRITE (11,*) 0

      CLOSE (11)

      RETURN
      END
      
