c============================================================================
      SUBROUTINE Dxfix3 (lim0)
C============================================================================

      use units
      use levels
      use numbers
      use phimatrix
      use ages
      use means

!     arguments
      integer, intent(in)                    :: lim0

!     loacl variables
      integer, dimension(:),  allocatable    :: nnvec,iicol,nn,nnq
      real(8), dimension(:),  allocatable    :: xvec,row,yvec,y1,y2,yy1,
     &                                          yy2,y3,y4,yres
      real(8), dimension (:,:),allocatable   :: xcov
      integer, dimension (:,:), allocatable  :: ieq,nna
      integer                                :: ii,nneff,nnfr,iobs
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      iun71=71
      nneff=maxval(neff)
      nnfr=maxval(nfr)
      allocate (xcov(nnfr,mobs),yvec(mobs),nna(mobs,nmeta),
     &          ieq(nneff,mobs),nnq(mobs),
     *          iicol(mqhq),y1(nage(1)),y2(nage(1)),yy1(nage(1)),
     &          yy2(nage(1)),
     &          y3(nage(1)),y4(nage(1)),nn(nage(1)),yres(mobs),stat=ii)
      if(ii>0)stop 'dffix'

      iq=1

!     re-read lsq solutions for fixed effects
      REWIND(IUN13)
      READ(IUN13)LIM2
      allocate(nnvec(lim2),row(lim2),xvec(lim2),stat=ii)
      if(ii>0)stop 'dflsp3 : alloc'
      read(iun13)nnvec
      read(iun13)row
      read(iun13)xvec

C     INITIALIZE
      nn=0
      y1=0.d0
      y2=0.d0
      y3=1.d12
      y4=-1.d12
      yy1=0.d0
      yy2=0.d0
      REWIND(IUN52)

500   READ(IUN52,END=598)II,NOBS,NR,NNA(:NOBS,:),nnq(:nobs),IICOL(:NR),
     *   ( (IEQ(K,L) ,K=1,NEFF(NNQ(L)) ),L=1,NOBS),
     *   ( (XCOV(K,L), K=kftfix(nnq(l))+1,NFR(NNQ(L)) ),L=1,NOBS),
     &    YVEC(:NOBS)

      do iobs=1,nobs
      iage=nna(iobs,1)
      y1(iage)=y1(iage)+yvec(iobs)
      yy1(iage)=yy1(iage)+yvec(iobs)*yvec(iobs)
      nn(iage)=nn(iage)+1
      end do

      do iobs=1,nobs
      yres(iobs)=yvec(iobs)
      iq=nnq(iobs)
!     adjust for covariables
      if(kftfix(iq)>0)xcov(:kftfix(iq),iobs)=
     &                 phi(nna(iobs,irrmet(0,iq)),:kftfix(iq),0,iq)
      do i=1,nfr(iq)
      yres(iobs)=yres(iobs)-xcov(i,iobs)*xvec(nfrst(iq)+i)
      end do

!     adjust for fixed effects
      do i=1,nfix(iq)
      irow=ieq(i,iobs)-lim1
      if(irow.gt.0)yres(iobs)=yres(iobs)-xvec( iicol(irow) )
      end do
      end do !obs

      do iobs=1,nobs
      iage=nna(iobs,1)
      y2(iage)=y2(iage)+yres(iobs)
      yy2(iage)=yy2(iage)+yres(iobs)*yres(iobs)
      if(yres(iobs).lt.y3(iage))y3(iage)=yres(iobs)
      if(yres(iobs).gt.y4(iage))y4(iage)=yres(iobs)
      write(iun71,'(i6,2g16.6)')nna(iobs,1),yres(iobs),yvec(iobs)
      if(abs(yres(iobs)).gt.2*sdev(iq))write(*,*)' large res',iage,
     &   yres(iobs),yvec(iobs),xcov(:,iobs),xvec(lim0+1:lim1),
     &   xvec( (/ (iicol( ieq(i,iobs)-lim1),i=1,nfix(1) ) /) )
      end do
      go to 500

 598  write(*,*)'output file for residuals is fort.71'
      write(iun66,*)'Means & SD for records and residuals'
      do i=1,nage(1)
      if(nn(i).gt.1)then
        yy1(i)=sqrt( (yy1(i)-y1(i)*y1(i)/nn(i))/(nn(i)-1) )
        yy2(i)=sqrt( (yy2(i)-y2(i)*y2(i)/nn(i))/(nn(i)-1) )
        y1(i)=y1(i)/nn(i)
        y2(i)=y2(i)/nn(i)
      end if
      write(*,'(i4,2g16.6)')i,y1(i),y2(i)
      write(iun66,'(i4,2i5,7g12.5)')i,iiage(i,1),nn(i),
     *          ybar(iq)+y1(i), y1(i),yy1(i),y2(i),yy2(i),y3(i),y4(i)
      end do

      deallocate(nnvec,row,xvec,ieq,iicol,xcov,yres,yvec,y1,y2,y3,y4,
     *           nn,nnq,yy1,yy2,stat=ii)
      if(ii>0)print *,'dxfix3 : de-alloc'
      return

      END subroutine dxfix3
















