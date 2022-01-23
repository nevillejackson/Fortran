C     Last change:  KM   17 Nov 97   10:28 am
c===========================================================================
      SUBROUTINE DXgrd3 (xvec)
c===========================================================================

      use params
      use legendre
      use ages
      use numbers

      real(8), dimension(mxparm), intent(in) :: xvec
      REAL(8) :: aamin,aamax,a,xupp,xlow,step,aa,aaa,arange
      REAL(8),dimension (:),allocatable :: aage,astar
      REAL(8),dimension (maxnq,maxnq)   :: vv
      REAL(8),dimension (maxnqq)        :: xkk
      REAL(8),dimension (maxnq)         :: a1,a2
      character(len=25)                 :: UNKN,FORMA,FSTAND
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      iun15=15
      UNKN='UNKNOWN'
      FORMA='FORMATTED'
      FSTAND='DF15#DAT'
      CALL FCONCT(IUN15,FSTAND,FORMA,UNKN)

      nq=nosvec(3)
      write(*,*)'age range in data =',iiage(1),' to',iiage(nq)
      write(*,*)'generate grid for ? '
      write(*,9)' 1  ...  ages in the data '
      write(*,9)' 2  ...  specified range & step size (interpolate) '
      call optdef(ii,1,2,1)
      if(ii.eq.1)then
         mq=nq
         allocate(astar(mq),aage(mq),stat=ii)
         if(ii>0)stop 'dxgrd3 : alloc'
         do i=1,nq
         aage(i)=iiage(i)
         end do
         aamin=iiage(1)
         aamax=iiage(nq)
      else
         write(*,*)'minimum age ?'
         call rvalue(aamin,-1.d12,1.d12)
         if(aamin.lt.iiage(1))write(*,*)'warning : trying to ',
     *   'extrapolate outside range in data !'
         write(*,*)'maximum age ?'
         call rvalue(aamax,aamin,1.d12)
         if(aamax.gt.iiage(nq))write(*,*)'warning : trying to ',
     *   'extrapolate outside range in data !'
 1       write(*,*)'step size ?'
         arange=aamax-aamin
         call rvalue(step,0.d0,arange)
         mq=arange/step
         a=aamin+mq*step
         if(dabs(a-aamax).gt.1.d-5)then
           write(*,*)'step size and upper age limit incompatible'
           write(*,*)'upper limit set to =',a
           aamax=a
         end if
         allocate(astar(mq),aage(mq),stat=ii)
         if(ii>0)stop 'dxgrd3 : alloc'

         aage(1)=aamin
         do i=1,mq
         aage(i+1)=aamin+i*step
         end do
         mq=mq+1
      end if
         
c     set up vector of standardised ages
      xlow=-1.d0
      xupp=1.d0
      aa=(xupp-xlow)/(aamax-aamin)
      do i=1,mq
      astar(i)=xlow+aa*(aage(i)-aamin)
      end do

      write(*,*)'which covariance function ?'
      write(*,9)' 1  ...  additive-genetic '
      if(nosvec(15).eq.1)write(*,9)' 2  ...  maternal-genetic '
      if(nosvec(14).eq.1)write(*,9)' 4  ...  add. random effect '
      write(*,9)' 5  ...  error (permanent)'
c      write(*,9)' 6  ...  measurement error (temporary)'
      call option(jj,1,6)

c     pick out k-matrix
      ibegin=0
      do i=1,jj-1
      ibegin=ibegin+kfit(i)*(kfit(i)+1)/2
      end do
      kk=kfit(jj)
      ij=0
      do i=1,kk
      do j=i,kk
      ij=ij+1
      xkk(ij)=xvec(ij+ibegin)
      print *,i,j,xkk(ij)
      end do
      end do

c     covariance function
      vv=0.d0
      do ifit=1,kk
      do jfit=1,kk
      do i=1,kk
      do j=1,kk
      vv(i,j)=vv(i,j)+clgndr(i,ifit)*clgndr(j,jfit)*
     *                              xkk(ihmssf(ifit,jfit,kk))
      end do
      end do
      end do
      end do

      write(*,*)' '
      write(*,*)'Coefficients of covariance function'
      do i=1,kk
      write(*,*)i-1,(vv(i,j),j=1,kk)
      end do

c     calculate "generated" covariance matrix

      do i=1,mq
      aa=astar(i)
      aaa=1.d0
      do m=1,kk
      a1(m)=aaa
      aaa=aaa*aa
      end do
      do j=1,mq
      aa=astar(j)
      aaa=1.d0
      do m=1,kk
      a2(m)=aaa
      aaa=aaa*aa
      end do
      xx=0.d0
      do k=1,kk
      do m=1,kk
      xx=xx+a1(k)*a2(m)*vv(k,m)
      end do
      end do
      write(iun15,915)i,j,aage(i),aage(j),xx,xx
      end do
      write(iun15,10)
 10   format(/)
      end do
      write(iun15,10)
      
      write(*,*)'Output file is "DF15#DAT" '
    
      stop ' "DXGRID" '
9     FORMAT(8X,A)
 915  format(2i4,2f10.3,f20.4,g20.8)
      end subroutine dxgrd3
