      program am2mesh
c -- because iargc is undeclare in  f2c
c --
c      implicit none 
c if you have any trouble with iargc()
c     replace  iargc() by 0
c and remove the call :  call getarg(1,fa)
c    -- this is just to get unix argument 
c ----------
      save 
      integer nbvx,nbtx,nbsdx
      parameter (nbvx=300000,nbtx=2*nbvx,nbsdx=1024)
      integer nu(3,nbtx),refv(nbvx),reft(0:nbtx)
      integer sd(3,nbsdx)
      integer nua(3,nbtx)
      integer kkk(nbvx)
      integer ma(4,3*nbtx)
      integer pile(nbtx)
      integer ipile(nbtx)
      integer mark(0:nbtx)
      integer nbv,nbt,nbe,nbsd
      integer i,j,k,nbsg,i1,i2,i3,j3,i4,j4,ma3,ma4,ir,it,sens,iedge,ie
      integer nbge
      integer  len_trim
      real c(2,nbvx)
      character*256 fam,fg,fm,fa
c      print *,' nb argument = ', iargc()
      if (iargc().lt.1) then
         print *, ' file sans suffix am_fmt'
         read (*,'(a)') fa
      else
         call getarg(1,fa)
      endif
      fam=fa(1:len_trim(fa))//'.am_fmt'
      fg=fa(1:len_trim(fa))//'_g.msh'
      fm=fa(1:len_trim(fa))//'_0.msh'
      if (iargc().ge.2)  call getarg(2,fg)
      if (iargc().ge.3)  call getarg(3,fm)
      open(1,file=fam,status='old',err=2)
      print *,' lecture de fichier ', fam(1:len_trim(fam))
      read (1,*) nbv,nbt
      print *,' nbv = ',nbv,' nbt = ',nbt
      if( nbv.gt.nbvx) stop 'trop de sommets'
      if( nbt.gt.nbtx) stop 'trop de triangles'
      
      read (1,*) ((nu(i,j),i=1,3),j=1,nbt)
      read (1,*) ((c(i,j),i=1,2),j=1,nbv)
      read (1,*) (reft(i),i=1,nbt)
      read (1,*) (refv(i),i=1,nbv)
      close(1)
      goto 10
 2    fam=fa(1:len_trim(fa))//'.am'
      open(1,file=fam,status='old',form='unformatted',err=3)
      print *,'lecture de fichier ', fam(1:len_trim(fam)),' unformatted'
      read (1) nbv,nbt
      print *,' nbv = ',nbv,' nbt = ',nbt
      if( nbv.gt.nbvx) stop 'trop de sommets'
      if( nbt.gt.nbtx) stop 'trop de triangles'
      
      read (1) ((nu(i,j),i=1,3),j=1,nbt),
     +     ((c(i,j),i=1,2),j=1,nbv),
     +     (reft(i),i=1,nbt),
     +     (refv(i),i=1,nbv)
      close(1)
      goto 10
 3    fam=fa(1:len_trim(fa))//'.amdba'
      open(1,file=fam,status='old')
      print *,' lecture de fichier ', fam(1:len_trim(fam))
      read (1,*) nbv,nbt
      print *,' nbv = ',nbv,' nbt = ',nbt
      if( nbv.gt.nbvx) stop 'trop de sommets'
      if( nbt.gt.nbtx) stop 'trop de triangles'
      

      read (1,*) (k,(c(i,k),i=1,2),refv(k),j=1,nbv)
      read (1,*) (k,(nu(i,k),i=1,3),reft(k),j=1,nbt)
      close(1)
      
 10   call caret1 (nu,nbt,nbv,ma,nbe,kkk)
      print *,' nb de trou = ', nbe-nbt-nbv+1
      ir=0
      do i=1,nbt
         ir=min(ir,reft(i))
      enddo
c--- pour etre sur de reft(0) n'est pas une ref de triangle
      reft(0)=ir-1
      
      do i=1,nbt
         do j=1,3
            nua(j,i)=0
         enddo
      enddo
      nbge=0
      do i=1,nbv
         kkk(i)=0
      enddo
C     generation  des traingles adjacents
      do i=1,nbe
         i4 =0 
         ma3=abs(ma(3,i))
         i3=(ma3-1)/3
         j3=ma3-3*i3
         if(ma(4,i).ne.0) then
            ma4=abs(ma(4,i))
            i4=(ma4-1)/3
            j4=ma4-3*i4
            nua(j4,i4)=i3
            nua(j3,i3)=i4
         endif
      enddo
c----   recherche des sous domaine 
      do i = 1,nbt
         mark(i)=0
      enddo
      mark(0)=-1
      nbsd = 0
      do j = 1,nbt
         i=0
         i3 = 0
         if(mark(j).eq.0) then
            nbsd = nbsd +1 
            sd(1,nbsd)=j
            ir=reft(j)
            sd(2,nbsd)=ir
            sd(3,nbsd)=0
            i=1
            pile(i)=j
            ipile(i)=3
 500        if(i.gt.0) then
 600           if(ipile(i).gt.0) then
                  it=nua(ipile(i),pile(i))
                  ipile(i)=ipile(i)-1
                  if(ir.eq.reft(it)) then
                     if(mark(it).eq.0) then
                        
                        mark(it)=nbsd
                        i3 = i3 +1
                        i=i+1
                        pile(i)=it
                        ipile(i)=3
                     endif
                  else
c--   on a un bord du sous domaine on sauve le nu du triangle 
                     sd(3,nbsd)=pile(i)*3+ipile(i)
c                     print *,pile(i),ipile(i)+1,ir
                  endif
                  goto 600
               else
                  i=i-1
                  goto 500
               endif
            endif
            print *,' nb de triangle du sous dom ',nbsd
     +           ,' de ref ',ir,' est = ',i3
         endif
      enddo
      print *,' Nombre de sous domaine = ',nbsd
      nbge=0
      do i=1,nbe
         mark(i)=0
         i4 =0 
         ma3=abs(ma(3,i))
         i3=(ma3-1)/3
         j3=ma3-3*i3
         nua(j3,i3)=sign(i,ma(3,i))
         if(ma(4,i).ne.0) then
            ma4=abs(ma(4,i))
            i4=(ma4-1)/3
            j4=ma4-3*i4
            nua(j4,i4)=sign(i,ma(4,i))
         endif
         if(reft(i3).ne.reft(i4)) then
            nbge=nbge+1
            kkk(ma(1,i))=1
            kkk(ma(2,i))=1
            mark(i)=nbge
         endif
      enddo

      nbsg =0
      do i = 1,nbv
         if(kkk(i).ne.0) then
            nbsg=nbsg+1
            kkk(i)=nbsg
         endif
      enddo
         
      open(2,file=fg(1:len_trim(fg)))
      print  *,' ecriture de la geometrie dans ',fg(1:len_trim(fg))
      write(2,*) 'MeshVersionFormatted 0'
      write (2,*)
      write(2,*) 'Dimension'
      write (2,*) 2 
      write(2,*) 
      
      write(2,*) 'AngleOfCornerBound'
      write(2,*) 30
      write(2,*)
      
      write (2,*) 'Vertices'
      write (2,*) nbsg
      do i=1,nbv
         if(kkk(i).ne.0) then
            write(2,*) c(1,i),c(2,i),refv(i)
         endif
      enddo
      write(2,*) 
      write(2,*) 'Edges'
      write(2,*) nbge
      do i=1,nbe
         i4=0
         ma3=abs(ma(3,i))
         i3=(ma3-1)/3
         j3=ma3-3*i3
         if(ma(4,i).ne.0) then
            ma4=abs(ma(4,i))
            i4=(ma4-1)/3
            j4=ma4-3*i4
         endif
         if(reft(i3).ne.reft(i4)) then 
            if(ma(3,i).gt.0)  then
               i1 = ma(1,i)
               i2 = ma(2,i)
            else
               i1 = ma(2,i)
               i2 = ma(1,i)
            endif
            write (2,*) kkk(i1),kkk(i2),min(refv(i1),refv(i2))
            endif

      enddo
      write(2,*)
      write(2,*) "SubDomain"
      write(2,*) nbsd
      do i =1,nbsd
         it=sd(3,i)/3
         j=sd(3,i)-3*it+1
         ie = abs(nua(j,it))
         i1 = ma(1,ie)
         i2 = ma(2,ie)
         if(sd(3,i).eq.0) stop 'Big Bug'
         if(sd(2,i).ne.reft(it)) stop 'BigBug 2'
         print *,j,it,sd(3,i),sd(2,i),c(2,nu(1,it))
     +        ,nu(1,it),nu(2,it),nu(3,it),' -- ',i1,i2
     +        ,'  nua=',nua(j,it), mark(abs(nua(j,it)))
         print *,'           ',i1,c(1,i1),c(2,i1),refv(i1)
         print *,'           ',i2,c(1,i2),c(2,i2),refv(i2)


         iedge = mark(abs(nua(j,it)))
         sens = sign(1,nua(j,it)*ma(3,ie))
         write (2,*) 2,iedge,sens,sd(2,i)
      enddo

      write(2,*)
    
      write(2,*) 'End'
     
      close(2)
      
      open(3,file=fm(1:len_trim(fm)))
      print  *,' ecriture de maillage  dans ',fm(1:len_trim(fm))
      write(3,*) 'MeshVersionFormatted 0'
      write (3,*)
      write(3,*) 'Dimension'
      write (3,*) 2 
      write (3,*)

      write (3,*) 'Identifier'
      write (3,'(a,a,a,a,a)') '"amfmt to mesh G=',fg(1:len_trim(fg))
     +     ,' copy of ',fam(1:len_trim(fam)),' "'
      write (3,*)
      

      write (3,*) 'Geometry'
      write (3,*) '"',fg(1:len_trim(fg)),'"'
      write(3,*)

 
      write(3,*) 'Vertices'
      write(3,*) nbv
      do i=1,nbv
         write(3,*) c(1,i),c(2,i),refv(i)
      enddo

      write(3,*)

      write (3,*) 'Triangles'
      write (3,*) nbt
      do i=1,nbt
         write(3,*) nu(1,i),nu(2,i),nu(3,i),reft(i)
      enddo
      write(3,*)
      write(3,*) 
      write(3,*) 'Edges'
      write(3,*) nbge
      do i=1,nbe
         i4=0
         ma3=abs(ma(3,i))
         i3=(ma3-1)/3
         j3=ma3-3*i3
         if(ma(4,i).ne.0) then
            ma4=abs(ma(4,i))
            i4=(ma4-1)/3
            j4=ma4-3*i4
         endif
         if(reft(i3).ne.reft(i4)) then 
            if(ma(3,i).gt.0)  then
               i1 = ma(1,i)
               i2 = ma(2,i)
            else
               i1 = ma(2,i)
               i2 = ma(1,i)
            endif
            write (3,*) i1,i2,min(refv(i1),refv(i2))
            endif

      enddo
      write(3,*)
      write(3,*) 'EdgeOnGeometricEdge'
      write(3,*)  nbge
      do i=1,nbge
         write(3,*) i,i
      enddo
      write(3,*)

      write(3,*) 'VertexOnGeometricEdge'
      write(3,*)  0
      write(3,*)


      write(3,*) 'VertexOnGeometricVertex'
      write (3,*) nbsg
      do i = 1,nbv
         if(kkk(i).ne.0) then
            write (3,*) i,kkk(i)
         endif
      enddo
      write(3,*)

      write(3,*) 'End'
      close(3)
      
      end

      integer function len_trim(c)
      character*(*) c
      integer i
      do i = len(c),1,-1
         if(c(i:i).ne.' ') then
            len_trim=i
            return
         endif
      enddo
      len_trim=0
      end

       subroutine rmspace(c)
      character*(*) c
      integer i,j
      j = 0
      do i = 1,len(c)
       if(c(i:i).ne.' ') then
        j = j +1
        c(j:j)=c(i:i)
       endif
      enddo
      if(j.lt.len(c)) c(j+1:)=' '
      end
      

C/UPDATE ADD NAME=CARETE,SSI=83011713
      subroutine caret1 (nu,nt,ns,ma,na,k)
      implicit none 
c--------------------------------------------------------------
c   but: construire les aretes d'une traingulation
c--------------------------------------------------------------
c in:
c       nu(1:3,1:nt) sommets des triangle
c       nt : nb de triangle
c       ns : nb de sommet
c       lma : nb de mot du tableau ma > 3*na 
c               et na = nt+ns-1 + nb trou dans l'ouvert 
c out:      
c     ma(1:3,1:na)   tableau des aretes du maillage
c                   (ma(1,i),ma(2,i))  2 sommets de l'arete i
c                    ma(3,i) = 0 => l'arete i est interne 
c                              1 => l'arete i est frontiere
c     na  :  nb d'arete                                
c
c  work
c     k(1:ns) tableau de travail 
c--------------------------------------------------------------
      integer nt,ns,na
      integer nu(3,nt),ma(4,*),k(ns)
      integer  ka(2,3)
      integer i,it,is,k1,k2,kx,ki,ip,ip1
      data ka/1,2,2,3,3,1/
      na=0
      do 10 i=1,ns
10      k(i)=0
      do 100 it=1,nt
       do 100 is=1,3
        k1=nu(ka(1,is),it)
        k2=nu(ka(2,is),it)
        kx=max(k1,k2)
        ki=min(k1,k2)
        ip=k(ki)
        ip1=0
 20     if(ip.ne.0)  then
           ip1=ip
           ip=ma(1,ip)
           if(ma(2,ip1).ne.kx) goto 20
           if(ma(4,ip1).ne.0) goto 10000
           ma(4,ip1)=it*3+is
           if(kx.ne.k2) ma(4,ip1)=-ma(4,ip1)
           goto 100
        endif
        na=na+1
        ma(1,na)=k(ki)
        k(ki)=na
        ma(2,na)=kx
        ma(3,na)=3*it+is
        if(kx.ne.k2) ma(3,na)= -ma(3,na)
        ma(4,na)=0
 100  continue

      do is=1,ns
         ip=k(is)
 210     if(ip.ne.0) then
            ip1=ip
            ip=ma(1,ip)
            ma(1,ip1)=is
            goto 210
         endif
      enddo
      return
10000 continue
      stop 'BiG error'
      end
