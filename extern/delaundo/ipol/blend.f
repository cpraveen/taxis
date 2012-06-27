      subroutine blend(N,iB,iC,nX,nY,nT,xx,yy,tt)

c     ..................................................................
c     
c     N:     number of points given
c     iB:    blending type:  -1 Linear Blending,
c                             0 Quadratic Blending, C0 Continuity
c                             1 Qubic Blending,     C1 Continuity
c                             2 Quintic Blending,   C2 Continuity
c     iC:    open/closed curve (1=closed curve)
c     nX:    arrays of given X's
c     nY:    arrays of given Y's
c     nT:    arrays of given T's
c     xx:    returned x value
c     yy:    returned y value
c     tt:    inputed t value
c     
c     Note: Include first to last node no matter what iC is.
c     ..................................................................

      integer N,iB,iC,i,j
      real*8 nX(*),nY(*),nT(*),xx,yy,tt
      real*8 xh(4),yh(4),th(4),lp1,lp2,lp3,lq2,lq3,lq4,tmin,tmax,tb
      real*8 t1,t2,t3,t4,t12,t13,t23,t24,t34,xp,yp,xq,yq
      real*8 P5,zero,one,two,three

      P5=0.5d0
      zero=0.d0
      one=1.d0
      two=2.d0
      three=3.d0
      
      if (N.le.3) then
         write (*,*) ' FATAL: Not enough points given in blend',N
         stop
      end if

c     find out whether nT is given.
      IF (nT(N)-nT(1).EQ.0.) THEN
        nT(1) = 0.
        DO iN = 2,N
          nT(iN) = nT(iN-1)+
     &             SQRT((nX(iN)-nX(iN-1))**2+(nY(iN)-nY(iN-1))**2)
        END DO
        IF (IC.EQ.1) THEN
          nT(N+1) = SQRT((nX(1)-nX(N))**2+(nY(1)-nY(N))**2)
        END IF
      END IF

c     find tmin,tmax
      tmin=min(nT(1),nT(N))
      tmax=max(nT(1),nT(N))
      if (tt.lt.tmin.or.tt.gt.tmax) then
         write (*,'(A,3(E12.5,A))')
     &       ' FATAL: t is not in correct range: ',
     &       tmin,' <',tt,' <',tmax,'.'
         stop
      end if

c     find span i
      i=0
      j=0
      do while (i.eq.0)
         j=j+1
         if (j.eq.N) then
            write (*,*) ' FATAL: t not in range to blend curve',
     &          iC,j,N,tt
            stop
         end if
         if ((tt.le.nT(j).and.tt.ge.nT(j+1)).or.
     +        (tt.ge.nT(j).and.tt.le.nT(j+1))) then
            i=j
         end if
      end do

c     linear fit
      if (iB.eq.-1) then
         xx=nX(i)+(nX(i+1)-nX(i))*
     +        ((tt-nT(i))/(nT(i+1)-nT(i)))
         yy=nY(i)+(nY(i+1)-nY(i))*
     +        ((tt-nT(i))/(nT(i+1)-nT(i)))
         return
      end if

c     closed curve
      if (iC.eq.1) then

c     first span
         if (i.eq.1) then
            xh(1)=nX(N-1)
            yh(1)=nY(N-1)
            th(1)=nT(1)-(nT(N)-nT(N-1))
            
            xh(2)=nX(1)
            yh(2)=nY(1)
            th(2)=nT(1)
            
            xh(3)=nX(2)
            yh(3)=nY(2)
            th(3)=nT(2)
            
            xh(4)=nX(3)
            yh(4)=nY(3)
            th(4)=nT(3)

c     last span
         elseif (i.eq.N-1) then
            xh(1)=nX(N-2)
            yh(1)=nY(N-2)
            th(1)=nT(N-2)
            
            xh(2)=nX(N-1)
            yh(2)=nY(N-1)
            th(2)=nT(N-1)
            
            xh(3)=nX(N)
            yh(3)=nY(N)
            th(3)=nT(N)
            
            xh(4)=nX(2)
            yh(4)=nY(2)
            th(4)=nT(N)+(nT(2)-nT(1))

c     middle spans
         else
            xh(1)=nX(i-1)
            yh(1)=nY(i-1)
            th(1)=nT(i-1)
            
            xh(2)=nX(i)
            yh(2)=nY(i)
            th(2)=nT(i)
            
            xh(3)=nX(i+1)
            yh(3)=nY(i+1)
            th(3)=nT(i+1)
            
            xh(4)=nX(i+2)
            yh(4)=nY(i+2)
            th(4)=nT(i+2)
         end if

c     open curve
      else

c     first span
         if (i.eq.1) then
            t1=tt-nT(1)
            t2=tt-nT(2)
            t3=tt-nT(3)

            t12=nT(1)-nT(2)
            t13=nT(1)-nT(3)
            t23=nT(2)-nT(3)

            lp1= t2*t3/(t12*t13)
            lp2=-t1*t3/(t12*t23)
            lp3= t1*t2/(t13*t23)

            xx=lp1*nX(1)+lp2*nX(2)+lp3*nX(3)
            yy=lp1*nY(1)+lp2*nY(2)+lp3*nY(3)
            return

c     last span
         elseif (i.eq.N-1) then
            t1=tt-nT(N-2)
            t2=tt-nT(N-1)
            t3=tt-nT(N)

            t12=nT(N-2)-nT(N-1)
            t13=nT(N-2)-nT(N)
            t23=nT(N-1)-nT(N)

            lp1= t2*t3/(t12*t13)
            lp2=-t1*t3/(t12*t23)
            lp3= t1*t2/(t13*t23)

            xx=lp1*nX(N-2)+lp2*nX(N-1)+lp3*nX(N)
            yy=lp1*nY(N-2)+lp2*nY(N-1)+lp3*nY(N)
            return

c     middle spans
         else
            xh(1)=nX(i-1)
            yh(1)=nY(i-1)
            th(1)=nT(i-1)
            
            xh(2)=nX(i)
            yh(2)=nY(i)
            th(2)=nT(i)
            
            xh(3)=nX(i+1)
            yh(3)=nY(i+1)
            th(3)=nT(i+1)
            
            xh(4)=nX(i+2)
            yh(4)=nY(i+2)
            th(4)=nT(i+2)
         end if
      end if

      t1=tt-th(1)
      t2=tt-th(2)
      t3=tt-th(3)
      t4=tt-th(4)

      t12=th(1)-th(2)
      t13=th(1)-th(3)
      t23=th(2)-th(3)
      t24=th(2)-th(4)
      t34=th(3)-th(4)

c     find parabola P
      lp1= t2*t3/(t12*t13)
      lp2=-t1*t3/(t12*t23)
      lp3= t1*t2/(t13*t23)
      xp=lp1*xh(1)+lp2*xh(2)+lp3*xh(3)
      yp=lp1*yh(1)+lp2*yh(2)+lp3*yh(3)

c     find parabola Q
      lq2= t3*t4/(t23*t24)
      lq3=-t2*t4/(t23*t34)
      lq4= t2*t3/(t24*t34)
      xq=lq2*xh(2)+lq3*xh(3)+lq4*xh(4)
      yq=lq2*yh(2)+lq3*yh(3)+lq4*yh(4)

c     quadtradic C0 blending
      if (iB.eq.0) then
         xx=P5*(xp+xq)
         yy=P5*(yp+yq)

c     cubic C1 blending
      elseif (iB.eq.1) then
         tb=t2/t23
         xx=(one+tb)*xp-tb*xq
         yy=(one+tb)*yp-tb*yq

c     quintic C2 blending
      elseif (iB.eq.2) then
         tb=t2/t23
         tb=-one*(tb*tb*(three+two*tb))
         xx=(one+tb)*xp-tb*xq
         yy=(one+tb)*yp-tb*yq

      else
         write (*,*) ' FATAL: Wrong value for blending',iB
         stop
      end if

      return
      end
