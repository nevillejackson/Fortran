      module eigs
        real(8), dimension(:), allocatable, save   :: eig
        real(8), dimension(:,:), allocatable, save :: vv
      end module eigs

!     ============
      PROGRAM DF17
!     ============

      use eigs

      integer ::
     &   np=0             ! no. of parameters
     &,  km=0             ! no. of k-matrices
     &,  me=0             ! no. of me variances
     &,  nrme=0           ! no. of regression coefficients m.e.
     &,  nrpe=0           ! ............................   p.e.
     &,  ncfp=0           ! no. of correlation function parameters
     &,  icorrf=-9        ! correlation function no. ( 0 to 6)

      character(len=6)                     :: ff
      character(len=30)                    :: fn
      character(len=60)                    :: bb,fm
      character(len=14),dimension(:), allocatable :: par
      character(len=50)                    :: cf

      integer, dimension(7)                :: kfit=0, kfteig=0, ksz=0
      integer, dimension(:), allocatable   :: kka,kkm,kkr,kkq,iiage,
     &                                        jjage
      integer                              :: nq,nqq,nage,kq
     &                                        kmind,ilgeps,ilgpe
      integer                              :: ska, skm,skr,skq,sme
      real(8), dimension(:), allocatable   :: xvec,eigg,astar,avec,ww
      real(8), dimension(:,:), allocatable :: phi,clgndr,xkk,rkk
      real(8)                              :: aamin,aamx,xlow=-1.d0,
     &                                        xupp=1.d0,aa,xx,eps
      real(8), dimension(:,:), allocatable :: siga,sigr,sigp,sigm,sigq,
     &                                        sige
      logical                              :: lex
!------------------------------------------------------------------------
      
      write(*,*)'------------------------------------------------------'
      write(*,*)'Program "DF17" - Operate on file "DF17#DAT" from DXMRR'
      write(*,*)'------------------------------------------------------'

!     INPUT FILE
      call open_df17

!     NUMBER OF TRAITS
      write(*,*)'No. of traits ?'
      nq=1
      call optdef(nq,1,9,1)
      
!     ANALYZE INPUT FILE
      call anal_df17

!     ALLOCATE ARRAYS
      call alloc_spaces

!     READ PARAMETER ESTIMATES
      rewind(1)
      do i=1,nparm
      read(1,'(g16.8,20x,a14)')xvec(i),par(i)
      end do
      close(1)
      
!     SET UP MATRIX OF LEGENDRE COEFFICIENTS
      call legendre(kq)

!     DETERMINE NO. OF AGES IN THE DATA
      call get_ages

!     GENETIC COVARIANCE FUNCTION
      ibegin=0
      cf='"1st" K-matrix : direct genetic (or equiv.) '
      call Kofunc(siga,ibegin,ska,kka)

!     SECOND GENETIC COV FUNCTION : SIG M
      cf='"2nd" K-matrix : maternal genetic         '
      call Kofunc(sigm,ibegin,skm,kkm)

!     PERMANENT ENV COV FUNCTION : SIG R
!     ... random regressions ...
      if(icorrf.eq.-9)then
         cf='"3rd" K-matrix : direct, permanent environmental '
         call Kofunc(sigr,ibegin,skr,kkr)
!     ... parametric form
      else
         cf='Parametric cov : direct, permanent environmental '
         call Corrfunc(sigr,ibegin,icorrf,nrpe,ncfp)
      end if

!     ADDITIONAL RANDOM EFFECT COV FUNCTION : SIG Q
      cf='"4th" K-matrix : maternal, permanent environmental '
      call Kofunc(sigq,ibegin,skq,kkq)

!     MEASUREMENT ERROR VARIANCES
      write(11,*)' '
      if(nrme.eq.0)then   ! ... no regression coefficients
         do ia=1,nage
         jbegin=ibegin+(jjage(ia)-1)*nqq
         do ij=1,nqq
         sige(ia,ij)=xvec(jbegin+ij)
         end do
         end do
         write(11,*)'Measurement error variance component(s) '
         do i=1,me
         write(11,'(i4,g14.6)')i,xvec(np-me+i)
         end do
      else
         call varfun_me   ! polynomial variance function
      end if

!     PHENOTYPIC COVARIANCES
      sigp=siga+sigm+sigr+sigq+sige

!     WRITE OUT COVARIANCES & GENETIC PARAMETERS
      call write_sdev

      contains

!     =====================
      subroutine open_df17
!     =====================

      inquire(file='DF17#DAT', exist=lex)
      if(lex)then
         write(*,*)'Found file "DF17#DAT" - use as input file '
         fn='DF17#DAT'
      else 
 401     write(*,*)'Input file ? (max. 30 char.s)'
         read(*,'(a)')fn
         if(fn(1:6).eq.'      ')then
            write(*,*)'Error : no file name given - try again !'
            go to 401
         end if
      end if
      open(1,file=fn,status='old',form='formatted')
      open(11,file='DF17.out',status='unknown',position='append')
      return
      end subroutine open_df17

!     ====================
      subroutine anal_df17
!     ====================

      character(len=14)                    :: pp
      
 550  read(1,'(a)',end=599)bb
      print *,bb
      if(bb(29:33).eq.'log L'.or.bb(24:28).eq.'log L')go to 589

!     ... try for parameter
      read(bb,'(g16.8,20x,a14)',err=589,end=589)xx,pp
      if(pp(1:4).eq.'KMAT')then
         np=np+1
         read(pp(9:14),'(2i3)')iq,jq
         if(iq.eq.1.and.jq.eq.1)then
            if(km>0)ksz(km)=kk
            km=km+1
         end if
         kk=iq
      else 
         if(kmind.eq.0)then  ! previous k-matrix finished
            ksz(km)=kk
            kmind=1
         end if

!        variance function for p.e. variance
         if(pp(1:10).eq.'REG.CF.-PE')then
            nrpe=nrpe+1
            np=np+1

         else if(pp(1:11).eq.'SIG C  0 -L')then
            np=np+1
            sigc00=xx
            read(pp,'(11x,i3)')ilgpe

!        correlation function
         else if(pp(1:5).eq.'CorF ')then
            ncfp=ncfp+1
            np=np+1
            read(pp(6:6),'(i1)')icorrf

!        variance function for measurement errors
         else if(pp(1:10).eq.'REG.CF.-ME')then
            nrme=nrme+1
            np=np+1

         else if(pp(1:11).eq.'SIG ME 0 -L')then
            me=me+1
            np=np+1
            read(pp,'(11x,i3)')ilgeps
         else if(pp(1:8).eq.'SIG ME  ')then
            me=me+1
            np=np+1
         end if
      end if
      go to 550

!     ... no more parameters
 589  read(bb,*)xlike                        ! read likelihood
      read(1,'(a)',end=599,err=599)bb
      print *,bb
      if(bb(1:12).eq.'order of fit')read(bb(13:60),*,err=599,end=599)
     &                                                           kfit
 560  read(1,'(a)',end=599,err=599)bb
      if(bb(1:12).eq.'no. eigenvalues')read(bb(13:60),*,err=599,
     &                                                 end=599)kfteig

 599  write(*,'(a,t35,a)') 'Input file used          = ',fn
      write(*,'(a,t35,i8)')'No. of traits specified  =',nq
      write(*,'(a,t35,i8)')'Total no. of parameters  =',np
      write(*,'(a,t35,i8)')'No. of "K-matrices"      =',km
      if(nrpe>0)then
         write(*,'(a,t35,i8)')'No. of regr. coeff.  PE  =',nrpe
         write(*,'(a,t35,i8)')'No. of corr.F. param.s   =',ncfp
         write(*,'(a,t35,i8)')'Correlation function no. =',icorrf
      end if
      write(*,'(a,t35,i8)')'No. of regr. coeff.  ME  =',nrme
      write(*,'(a,t35,i8)')'No. of m.error variances =',me
      write(*,'(a,t27,f16.3)')'Log likelihood           =',xlike
      write(11,'(a,t35,a)') 'Input file used          = ',fn
      write(11,'(a,t35,i8)')'No. of traits specified  =',nq
      write(11,'(a,t35,i8)')'Total no. of parameters  =',np
      write(11,'(a,t35,i8)')'No. of "K-matrices"      =',km
      if(nrpe>0)then
         write(11,'(a,t35,i8)')'No. of regr. coeff.  PE  =',nrpe
         write(11,'(a,t35,i8)')'No. of corr.F. param.s   =',ncfp
         write(11,'(a,t35,i8)')'Correlation function no. =',icorrf
      end if
      write(11,'(a,t35,i8)')'No. of regr. coeff.  ME  =',nrme
      write(11,'(a,t35,i8)')'No. of m.error variances =',me
      write(11,'(a,t27,f16.3)')'Log likelihood           =',xlike

      return
      end subroutine anal_df17

!     =======================
      subroutine alloc_spaces
!     =======================

      allocate (kka(nq),kkm(nq),kkr(nq),kkq(nq),stat=ii)
      if(ii>0)stop 'alloc'

      if(nq.eq.1)then
         kka(1)=kfit(1)
         kkm(1)=kfit(2)
         if(icorrf.eq.-9)then
            kkr(1)=kfit(4)
         else
            kkr=0
         end if
         kkq(1)=kfit(5)
         sme=me+nrme
      else
         kkm=0
         kkr=0
         kkq=0
         do iq=1,nq
         write(*,*)'For trait no. ',iq
         write(*,*)'order of fit for genetic cov function ?'
         call option(kka(iq),1,kfit(1))
         if(kfit(2)>0)then
            print *,'order of fit for maternal genetic cov function ?'
            call option(kkm(iq),1,kfit(2))
         end if
         if(kfit(4)>0)then
            print *,'order of fit for animal perm env cov function ?'
            call option(kkr(iq),1,kfit(4))
         end if
         if(kfit(5)>0)then
            print *,'order of fit for maternal perm env cov function ?'
            call option(kkq(iq),1,kfit(5))
         end if
         end do ! iq
         sme=me*nq*(nq+1)/2  !      no VF allowed yet for multiv anal
      end if
      ska=sum(kka)
      skm=sum(kkm)
      skr=sum(kkr)
      skq=sum(kkq)

      if(icorrf.eq.-9)then
         nparm=ska*(ska+1)/2+skr*(skr+1)/2+skm*(skm+1)/2+
     &                                               skq*(skq+1)/2+sme
      else
         nparm=ska*(ska+1)/2+skm*(skm+1)/2+ skq*(skq+1)/2+sme
     &          + nrpe+ncfp+1
      end if
      allocate(xvec(nparm),par(nparm),stat=ii)

      kq=ska
      if(skm>kq)kq=skm
      if(skr>kq)kq=skr
      if(skq>kq)kq=skq
      allocate(avec(kq),vv(kq,kq),phi(kq,kq),clgndr(kq,kq),eig(kq),
     &      eigg(kq), rkk(kq,kq),xkk(kq,kq),ww(kq*(kq+1)/2),stat=ii)
      if(ii>0)stop 'alloc'

      return
      end subroutine alloc_spaces

!     ===================
      subroutine get_ages
!     ===================

      integer :: jj

      nage=0
      inquire(file='DF20#DAT', exist=lex)

      if(lex)then
         write(*,*)'File "DF20#DAT" found - determine ages from it'
         open(1,file='DF20#DAT')
         do 
            read(1,*,iostat=jj)ii
            if(jj.ne.0)exit
            nage=nage+1
         end do
         write(*,*)'No. of ages =',nage   
         allocate(iiage(nage),jjage(nage),astar(nage),stat=ii)
         if(ii>0)stop 'alloc age'
         rewind(1)
         do i=1,nage
         if(me>1)then
            read(1,*)ii,jj,iiage(i)
         else
            read(1,*)ii,iiage(i)
         end if

         end do
         write(*,*)'Age range found =',iiage(1),iiage(nage)

      else
         write(*,*)'File "DF20#DAT" does not exist'
         write(*,*)'Minimum age in the data ?'
         call option(ia1,0,999999)
         write(*,*)'Maximum age in the data ?'
         call option(ia2,ia1,999999)
         write(*,*)'Step size ??'
         call option(nstep,1,99999)
         nage=1
         ia=ia1
         do while(ia.le.ia2)
            ia=ia+nstep
            nage=nage+1
         end do
         allocate(iiage(nage),jjage(nage),astar(nage),stat=ii)
         if(ii>0)stop 'alloc age'
         do i=1,nage
         iiage(i)=ia1+(i-1)*nstep
         end do
         if(iiage(nage)>ia2)iiage(nage)=ia2
      end if

!     map ages to appropriate sigeps
      if(me.eq.1 .and.nrme.eq.0)then       ! single m.e. variance
         jjage=1
      else if(me.eq.nage.or.nrme>0)then    ! variance function or individual me
         jjage= (/ (i,i=1,nage) /)
      else if(me>1)then
         if(lex)then
            rewind(1)
            do j=1,nage
            read(1,*)jjage(j),jj,k
            iiage(j)=k
            end do
            close(1)
         else
            write(*,*)'Give running no. of m.e. variance component :'
            do i=1,nage
            write(*,'(a,i6,a)')'Age =',iiage(i),'  ?'
            call option(jjage(i),1,me)
            end do
         end if
      end if
      
!     standardised ages
      astar(:nage)=iiage(:nage)
      aamin=astar(1)
      aamax=astar(nage)
      aa=(xupp-xlow)/(aamax-aamin)
      astar(:nage)=xlow+aa*(astar(:nage)-aamin)

!     allocate arrays depending on no. of ages
      nqq=nq*(nq+1)/2
      allocate(siga(nage,nqq),sigp(nage,nqq),sigm(nage,nqq),
     &         sigr(nage,nqq),sigq(nage,nqq),sige(nage,nqq),stat=ii)

      return
      end subroutine get_ages

c     ==========================================
      SUBROUTINE  KOFUNC (sig,ibegin,kkfit,kfit)
c     ==========================================
      use eigs

      integer, intent(in)                       :: kkfit
      integer, intent(inout)                    :: ibegin
      integer, intent(in), dimension(nq)        :: kfit
      real(8), dimension(nage,nqq), intent(out) :: sig

      if(kkfit<1)then
         sig=0.d0
         return
      end if
      write(11,'(/a)')cf

!     pick out coefficient matrix
      ij=0
      do i=1,kkfit
      do j=i,kkfit
      ij=ij+1
      xkk(i,j)=xvec(ibegin+ij)
      xkk(j,i)=xvec(ibegin+ij)
      ww(ij)=xvec(ibegin+ij)
      end do
      end do
      ibegin=ibegin+kkfit*(kkfit+1)/2

!     calculate correlations between RR coefficients
      do i=1,kkfit
      do j=i+1,kkfit
      rkk(i,j)=xkk(j,i)/sqrt(xkk(i,i)*xkk(j,j))
      end do
      end do

      write(fm,'(a)')'(a,i3,  g13.6,  f13.3)' 
      do i=1,kkfit
      write(*,'(a,i3,(6g12.5))')' K',i,xkk(i,:i)
      write(fm(7:8),'(i2)')i
      write(fm(15:16),'(i2)')kkfit-i
      write(11,fm)'  K',i,xkk(i,:i),rkk(i,i+1:kkfit)
      end do 

c     eigenvalue decomposition
      call EIGN(WW,1.D-6,KKFIT,300,1)
      do i=1,kkfit
      xx=-9999.99
      do j=1,kkfit
      if(eig(j)>xx)then
          xx=eig(j)
          jj=j
      end if
      end do
      eigg(i)=eig(jj)
      eig(jj)=-1.d8
      end do
      write(11,'(a,3x,(5g14.6))')'EIG',eigg(:kkfit)
 
      ij=0
      do iq=1,nq
      do jq=iq,nq
      ij=ij+1
!     work out cov function
      vv=0.d0
      do ifit=1,kfit(iq)
      ifit1=sum(kfit(:iq-1))+ifit
      do jfit=1,kfit(jq)
      jfit1=sum(kfit(:jq-1))+jfit
      xx=xkk(ifit1,jfit1)
      do i=1,kfit(iq)
      do j=1,kfit(jq)
      vv(i,j)=vv(i,j)+clgndr(i,ifit)*clgndr(j,jfit)*xx
      end do !j
      end do !i
      end do ! jfit
      end do ! ifit
      do i=1,kkfit
      write(11,'(a,i3,(5g14.6))')' CF',i,vv(i,:kkfit)
      end do 

!     calculate covariances for ages in the data
      avec(1)=1.d0
      do ia=1,nage
      do j=2,kq
      avec(j)=astar(ia)**(j-1)
      end do
      xx=0.d0
      do i=1,kfit(iq)
      do j=1,kfit(jq)
      xx=xx+avec(i)*avec(j)*vv(i,j)
      end do ! j
      end do ! i
      sig(ia,ij)=xx
      end do ! ia
      end do ! jq
      end do ! iq

      return
      end subroutine kofunc

!     =================================================
      subroutine Corrfunc (sig,ibegin,icorrf,nrpe,ncfp)
!     =================================================

      integer, intent(in)                       :: icorrf,nrpe,ncfp
      integer, intent(inout)                    :: ibegin
      real(8), dimension(nage,nqq), intent(out) :: sig

      sig=0.d0
      if(icorrf<0.or.icorrf>9)then
         write(*,*)'invalid correlation function'
         return
      end if

!     variance function for perm environmental effects
      ilg=mod(ilgpe,10)         ! 0=normal, 1=log-lin 1, 2=log-lin 2
      jlg=mod(ilgpe,100)/10     ! 0=variances, 1=sdev
      sigc=xvec(ibegin+nrpe+1)

      do ia=1,nage
      if(ilgpe<100)then         ! ordinary polynomials of standardised age
         xx=1.d0
         if(ilg.eq.2)xx=sigc
         aa=astar(ia)
         do j=1,nrpe
         xx=xx+xvec(ibegin+j)*(aa**j)
         end do
      else                    ! Legendre polynomials
         stop 'orth pol not done yet'
!         xx=phi(i,1,6,1)
!         if(ilg.eq.2)xx=xx*sigc
!         do j=1,nb
!         xx=xx+bvec(j)*phi(i,j+1,6,1)
!         end do
      end if

      if(ilg.eq.0)then
         sig(ia,1)=xx*sigc
      else if(ilg.eq.1)then
         sig(ia,1)=dexp(xx)*sigc
      else if(ilg.eq.2)then
         sig(ia,1)=dexp(xx)
      else if(ilg.eq.3)then
         sig(ia,1)=dexp(xx*sigc)
      end if
      if(jlg.eq.1)sig(ia,1)=sig(ia,1)**2   ! we've modelled SD rather than Var
      end do  ! ia

      
      write(11,*)'Function for perm. env. standard deviation'
      do i=1,nrpe
      write(11,'(a,i3,t30,g13.6)')'Regression coefficient',i,
     &                                          xvec(ibegin+i)
      end do
      write(11,'(a,t30,i10)')'Parameterisation for VF',ilgpe
      if(jlg.eq.1)then
      write(11,'(a,t30,g13.6)')'SDEV  at mean age ',sigc
      else
      write(11,'(a,t30,g13.6)')'Variance at mean age ',sigc
      end if

      ibegin=ibegin+nrpe+1+ncfp
      return
      end subroutine corrfunc

!     =====================
      subroutine varfun_me
!     =====================

      ilg=mod(ilgeps,10)         ! 0=normal, 1=log-lin 1, 2=log-lin 2
      jlg=mod(ilgeps,100)/10     ! 0=variances, 1=sdev

      sigeps=xvec(np)
      do ia=1,nage
      if(ilgeps<100)then         ! ordinary polynomials of standardised age
         eps=1.d0
         if(ilg.eq.2)eps=sigeps
         aa=astar(ia)
         do j=1,nrme
         eps=eps+xvec(ibegin+j)*(aa**j)
         end do
      else                    ! Legendre polynomials
         stop 'orth pol not done yet'
      end if

      if(ilg.eq.0)then
         sige(ia,1)=eps*sigeps
      else if(ilg.eq.1)then
         sige(ia,1)=dexp(eps)*sigeps
      else if(ilg.eq.2)then
         sige(ia,1)=dexp(eps)
      else if(ilg.eq.3)then
         sige(ia,1)=dexp(eps*sigeps)
      end if
      if(jlg.eq.1)sige(ia,1)=sige(ia,1)**2   ! modelled SD rather than Var
      end do  ! ia

      write(11,*)'Measurement error variance function'
      do i=1,nrme
      write(11,'(a,i3,t30,g13.6)')'Regression coefficient',i,
     &                                          xvec(np-nrme-me+i)
      end do
      write(11,'(a,t30,i10)')'Parameterisation for VF',ilgeps
      if(jlg.eq.1)then
         write(11,'(a,t30,g13.6)')'SDEV  at mean age ',sigeps
      else
         write(11,'(a,t30,g13.6)')'Variance at mean age ',sigeps
      end if

      return
      end subroutine varfun_me

!     =====================
      subroutine write_sdev
!     =====================

!     write out a file "sdevij" for each pair of traits i,j
      ij=0
      do iq=1,nq
      do jq=1,nq
      ij=ij+1

      write(ff,'(a,2i1)')'sdev',iq,jq
      open(4,file=ff,status='unknown',form='formatted')
      write(4,'(a,4i3,2(a,i4),a,f16.3)')'# k=',ska,skm,skr,skq,
     &                   ' p=',nparm,' m=',me,'  log L =',xlike
      if(me<2)write(4,'(a,g13.6)')'# sig_eps =',xvec(nparm)

!     variances & genetic parameters
      if(iq.eq.jq)then
         sdm=0.d0
         sdr=0.d0
         sdq=0.d0
         xmsq=0.d0
         rsq=0.d0
         csq=0.d0
         t=0.d0
         do i=1,nage
         ii=i
         hsq=siga(ii,ij)/sigp(ii,ij)
         sda=sqrt(siga(ii,ij))
         if(kkm(iq)>0)then
            sdm=sqrt(sigm(ii,ij))
            xmsq=sigm(ii,ij)/sigp(ii,ij)
         end if
         if(kkr(iq)>0.or.icorrf.ne.-9)then
           sdr=sqrt(sigr(ii,ij))
           rsq=sigr(ii,ij)/sigp(ii,ij)
           t=hsq+rsq
         end if
         if(kkq(iq)>0)then
           sdq=sqrt(sigq(ii,ij))
           qsq=sigq(ii,ij)/sigp(ii,ij)
         end if
        sd=sqrt(sigp(ii,ij))
        sde=sqrt( sige(ii,ij) )
        write(4,40)iiage(i),hsq,xmsq,rsq,qsq,t,sd,sda,sdm,sdr,sdq,sde
 40     format(i4,5f8.3,6f9.3)
        end do

!     covariances & correlations
      else
        ii=ihmii(iq,nq)
        jj=ihmii(jq,nq)
        rrm=0.d0
        rrr=0.d0
        rrq=0.d0
        do i=1,nage
        rra=siga(i,ij)/sqrt(siga(i,ii)*siga(i,jj))
        rre=sige(i,ij)/sqrt(sige(i,ii)*sige(i,jj))
        rrp=sigp(i,ij)/sqrt(sigp(i,ii)*sigp(i,jj))
        if(skm>0)rrm=sigm(i,ij)/sqrt(sigm(i,ii)*sigm(i,jj))
        if(skr>0.or.icorrf>-9)rrr=sigr(i,ij)/sqrt(sigr(i,ii)*sigr(i,jj))
        if(skq>0)rrq=sigq(i,ij)/sqrt(sigq(i,ii)*sigq(i,jj))
        write(4,41)iiage(i),rra,rrm,rrr,rrq,rre,rrp,siga(i,ij),
     &       sigm(i,ij),sigr(i,ij),sigq(i,ij),sige(i,ij),sigp(i,ij)
 41     format(i4,6f8.3,6f9.3)
        end do
      end if
      close(4)
      end do !jq
      end do !iq
      return
      end subroutine write_sdev

!     ========================
      subroutine legendre (nq)
!     ========================
 
      integer, intent(in) :: nq
      integer             :: iord, jj, m, kk
      real(8)             :: c1, c2,cc, binfac

      clgndr=0.d0
      do iord=0,nq-1
      c1=dfloat(2**iord)
      c2=iord+0.5d0
      cc=dsqrt(c2)/c1
      jj=iord/2
      do m=jj,0,-1
      i=1+iord-2*m
      j=2*(iord-m)
      kk=1
      if(m.gt.0)kk=(-1)**m
      clgndr(i,iord+1)=kk*cc*binfac(iord,m)*binfac(j,iord)
      end do
      end do

      return
      end subroutine legendre

      end program df17

!=============================================================================
      double precision function binfac (n,m)
!=============================================================================
      integer, intent(in) :: n,m
      real(8)             :: factrl,b1,b2,b3

      if(m.eq.0.or.m.eq.n)then
         binfac=1.d0
      else
         b1=factrl(n)
         n1=n-m
         b2=factrl(n1)
         b3=factrl(m)
         binfac=b1/(b2*b3)
      end if
      return
      end function binfac

c=============================================================================
      double precision function factrl (n)
c=============================================================================
      integer, intent(in) :: n
      real(8)             :: ff

      ff=1.d0
      do i=2,n
      ff=ff*i
      end do
      factrl=ff
      return
      end function factrl

C=======================================================================
      SUBROUTINE EIGn(A,EPS,N,MAX,ICODE)
C=======================================================================

      use eigs

      real(8), dimension(n*(n+1)/2), intent(inout) :: a
      real(8), intent(in)                          :: eps
      integer, intent(in)                          :: n, max
      integer, intent(inout)                       ::  icode

      integer :: nsweep, n1,nm1,nn,ij,i,j,ipp,ipq,iqq
      real(8) :: epsqu,ss,x,app,apq,aqq,theta,s,c,t,cc,cs,csa,
     *           aijp,aijq,vp,vq

      NSWEEP=0
      N1=N+1
      NM1=N-1
      EPSQU=EPS*EPS

      IF(ICODE.EQ.1)THEN
         vv=0.d0
         DO I=1,N
         VV(I,I)=1.D0
         end do
      END IF

C     CALCULATE SUM OF SQUARES OF OFF-DIAGONAL ELEMENTS

5     SS=0.D0
      NSWEEP=NSWEEP+1
      IJ=0
      DO I=1,NM1
      IJ=IJ+1
      DO J=I+1,N
      IJ=IJ+1
      X=A(IJ)
      SS=SS+X*X
      end do
      end do
      SS=2.D0*SS
      NN=N*(N+1)/2

C     TEST WHETHER SS ARE SUFFICIENTLY SMALL
      IF(SS.LT.EPSQU)THEN
         II=-N
         DO I=1,N
         II=II+N1
         EIG(I)=A(II)
         II=II-I
         end do
         RETURN
      END IF

C     MATRIX NOT DIAGONAL YET, NEED MORE TRANSFORMATIONS

      IPP=-N
      IPQ=0
      DO IP=1,NM1

      IPP=IPP+N1
      APP=A(IPP)
      IPQ=IPQ+1
      DO IQ=IP+1,N
      IPQ=IPQ+1
      APQ=A(IPQ)

      IF(APQ.NE.0)THEN

C     APPLY P-Q ROTATION TO MATRIX A

      IQQ=IHMII(IQ,N)
      AQQ=A(IQQ)
      THETA=0.5D0*(A(IQQ)-APP)/APQ
      IF(THETA.EQ.0)THEN
         T=1.D0
      ELSE
         X=DSQRT(1.D0+THETA*THETA)
         IF(THETA.LT.0)X=-X
         T=1.D0/(THETA+X)
      END IF
      C=1.D0/DSQRT(1.D0+T*T)
      S=C*T
      CC=C*C
      CS=C*S
      SS=S*S
      CSA=2.D0*CS*APQ

      A(IPP)=CC*APP-CSA+SS*AQQ
      A(IPQ)=CS*(APP-AQQ)+(CC-SS)*APQ
      A(IQQ)=SS*APP+CSA+CC*AQQ
      APP=A(IPP)

      DO  J=1,IP-1
      IJP=IHMSSF(IP,J,N)
      IJQ=IHMSSF(IQ,J,N)
      AIJP=A(IJP)
      AIJQ=A(IJQ)
      A(IJP)=C*AIJP-S*AIJQ
      A(IJQ)=S*AIJP+C*AIJQ
      end do

      IJP=IPP
      DO J=IP+1,IQ-1
      IJP=IJP+1
      IJQ=IHMSSF(IQ,J,N)
      AIJP=A(IJP)
      AIJQ=A(IJQ)
      A(IJP)=C*AIJP-S*AIJQ
      A(IJQ)=S*AIJP+C*AIJQ
      end do

      IJQ=IQQ
      DO J=IQ+1,N
      IJP=IHMSSF(IP,J,N)
      IJQ=IJQ+1
      AIJP=A(IJP)
      AIJQ=A(IJQ)
      A(IJP)=C*AIJP-S*AIJQ
      A(IJQ)=S*AIJP+C*AIJQ
      end do

C     ACCUMULATE TRANSFORMATIONS TO FORM MATRIX OF EIGENVECTORS

      IF(ICODE.EQ.1)THEN
         DO I=1,N
         VP=VV(I,IP)
         VQ=VV(I,IQ)
         VV(I,IP)=C*VP-S*VQ
         VV(I,IQ)=S*VP+C*VQ
         end do
      END IF

      END IF

      end do
      IPP=IPP-IP
      end do

      IF(NSWEEP.LT.MAX)GO TO 5

      ICODE=9
      PRINT *,'MAX. NO. OF SWEEPS REACHED    =',MAX
      PRINT *,'LAST SS OF OFF-DIAG. ELEMENTS =',SS
      PRINT *,'ACCURACY TOLERANCE GIVEN      =',EPSQU,EPS

      RETURN
      END subroutine eign

























