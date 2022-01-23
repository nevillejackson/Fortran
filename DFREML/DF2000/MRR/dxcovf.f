!===========================================================================
      SUBROUTINE      DXCOVF (xlike,ioprun)
!===========================================================================

c     purpose :       routine to write out results (CF estimates) to
c                     unit "66"
c-------------------------------------------------------------km--8/95------

      use parameters
      use names
      use units
      use ages
      use numbers
      use parmap
      use phimatrix
      use residuals

      real(8), intent(in)                    :: xlike
      integer, intent(in)                    :: ioprun
      real(8), dimension(:), allocatable     :: xkk,rr
      integer                                :: i,kkmx
      real(8)                                :: sigeps, sigpe, eee, xx

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      mfitmx=maxval(ksfit)
      kkmx=mfitmx*(mfitmx+1)/2
      allocate(xkk(kkmx),rr(mfitmx),stat=ii)
      if(ii>0)stop 'alloc dxcovf'

c     write out estimates to unit "17"
      call write_iun17

      WRITE(IUN66,908)'---------------------------------'
      WRITE(IUN66,*)  'ESTIMATES OF COVARIANCE FUNCTIONS'
      WRITE(IUN66,*)  '---------------------------------'
      WRITE(IUN66,*)' '
      if(ialog.eq.1)then
         write(iun66,*)'Ages have been transformed to log scale !!!'
         write(iun66,*)' '
      end if

c     additive genetic
      ibegin=0
      if(nanim>0)then
         call pick_kmat (1,ibegin)
         WRITE(IUN66,908)'ADDITIV-GENETIC (DIRECT) COVARIANCE MATRIX'
         call cofunc  (ksfit(1),1)
      end if

c     maternal genetic
      if(ioprn2.eq.1)then
         call pick_kmat (2,ibegin)
         WRITE(IUN66,908)'SECOND "ANIMAL" COVARIANCE MATRIX'
         call cofunc  (ksfit(2),2)
      end if

!     ------------------
!     add. random effect
!     ------------------

!     ... RR model
      if(ioprn1.eq.1 .and. irropt(4,1)<4 )then
         call pick_kmat (4,ibegin)
         WRITE(IUN66,908)'COVARIANCE MATRIX FOR PERM. ENV. EFFECT'
         call cofunc  (ksfit(4),4)

!     ... parametric correlation structure
      else if(irropt(4,1).eq.4)then
         write(iun66,908)'Covariance structure for perm. env. effect'
         write(iun66,*)'"Variance" function'
         if(kvfpe.ge.0)then
!           ... write out description
            call wr_varfun (ibegin, kvfpe, ilgpe, kfoupe, omegape )
!           ... evaluate variance function
            call check_varfun (
     &          xvec(ibegin+1:ibegin+kvfpe+2*kfoupe),
     &          pevec, xvec(ibegin+kvfpe+2*kfoupe+1),omegape,
     &          zero,big,fvalue,kvfpe,ilgpe,kfoupe,iout)
!           ... link function a la foulley et al  
         else
            write(iun66,*)'Use link function to model p.e. variances'
            write(iun66,*)'Parameter 1 =',xvec(ibegin+1)
            write(iun66,*)'Parameter 2 =',xvec(ibegin+2)
         end if

         write(iun66,*)'Correlation function',icorrf
         do i=1,ncpno
         write(iun66,*)'... Parameter no.',i,
     &                                xvec(ibegin+kfit(4,1)-ncpno+i)
         end do

         ibegin=ibegin+kfit(4,1)
      end if

      if(ioprn3.eq.1)then
         call pick_kmat (5,ibegin)
         WRITE(IUN66,908)'COVARIANCE MATRIX FOR ADD. RANDOM EFFECT'
         call cofunc  (ksfit(5),5)
      end if

!     ---------------------------
!     measurement error variances
!     ---------------------------

      WRITE(IUN66,908)'ERROR COVARIANCE MATRIX'

!     homogeneous (co)variances or step function (univ)
      if(kfit(7,1)>0)then
!         call cofunc  (ksfit(7),7)
          do i=1,kfit(7,1)
          xkk=0.d0
          xkk(:nq*(nq+1)/2 )=xvec(ibegin+1:ibegin+nq*(nq+1)/2)
          ibegin=ibegin+nq*(nq+1)/2
          write(iun66,'(i4,(t5,6f10.4))')i,xkk(:nq*(nq+1)/2)
          end do
          if(kfit(7,1).eq.1)then
             do j=1,nage(1)
             eevec(j,:nq*(nq+1)/2)=xkk(:nq*(nq+1)/2)
             end do
          else
             eevec=0.d0 ! yet to be implemented
          end if

!     variance function
      else if(kfit(7,1)<0 .or. kfoume>0)then
         write(iun66,*)'Have fitted variance function to model',
     &                 ' error variances'
         call wr_varfun (ibegin, -kfit(7,1), ilgeps, kfoume, omegame)

!        evaluate variance function
         call check_varfun (xvec(ibegin+1:nparm-1),eevec(:nage(1),1), 
     &          xvec(nparm), omegame, zero,big,fvalue,-kfit(7,1),
     &          ilgeps, kfoume, iout)

!        link function with p.e. variances
         if(kvfpe.eq.-2)then
            do i=1,nage(1)
            eee=sqrt(eevec(i,1))
            xx=exp( xvec(ibegin+1)+ xvec(ibegin+2)*log(eee))
            pevec(i)=xx*xx
            end do
         end if
      else
         write(iun66,*)'No measurement error variances have been fitted'
      end if

      if(kfit(7,1)<0.or.irropt(4,1).eq.4)then
         write(iun66,*)
         write(iun66,*)'Variance function(s) evaluated for ages in data'
         write(iun66,'(t19,2(4x,a8,4x))')'Var(ME) ','Var(PE) '
         sigpe=0.d0
         do i=1,nage (1)
         sigeps=eevec(i,1)
         if(irropt(4,1).eq.4)sigpe=pevec(i)
         write(iun66,'(i4,i6,f8.3,3g16.6)')i,iiage(i,1),astar(i),sigeps,
     &                                     sigpe
         end do
      end if
      deallocate(xkk,stat=ii)
      if(ii>0)stop 'dealloc dxcovf'

908   FORMAT(/1X,A)
      return

      contains

!     ================================
      subroutine pick_kmat (jj,ibegin)
!     ================================

      integer, intent(in)     :: jj
      integer, intent(inout)  :: ibegin
      integer                 :: kk

      xkk=0.d0
      kk=ksfit(jj)*(ksfit(jj)+1)/2
      xkk(:kk)=xvec(ibegin+1:ibegin+kk)
      ibegin=ibegin+kk
      return

      end subroutine pick_kmat 

c     ====================================
      SUBROUTINE COFUNC (kkfit,iparm)
c     ====================================

      use legendre
      use eigen_wrk

      integer, intent(in)                  :: kkfit, iparm

      real(8)                              :: es
      integer                              :: i,j,nneg,ifit
      integer, external                    :: ihmssf
      character(len=20),dimension(0:4)     :: txt =(/
     &                                        ' Ordinary polynomial',
     &                                        ' Legendre polynomial',
     &                                        ' User defined       ',
     &                                        ' LRS Dairy model    ',
     &                                      ' Structure Cov(Ages)'/)
      character(len=30)                    ::  fmt

      write(iun66,*)' '
      write(iun66,901)'Order of polynomial fit',kkfit
      write(iun66,*)' '
      write(iun66,*)'Estimated coefficient matrix (& correlations',
     &              ' between RR coefficients)'
      do i=1,kkfit
      do j=i+1,kkfit
      rr(j)=xkk(ihmssf(i,j,kkfit))/sqrt( xkk(ihmii(i,kkfit))*
     &                                   xkk(ihmii(j,kkfit)) )
      end do ! j
      fmt='(i2,  g13.6,  f13.3)'
      write(fmt(5:6),'(i2)')i
      if(i<kkfit)write(fmt(13:14),'(i2)')kkfit-i
      write(iun66,fmt)i,(xkk(ihmssf(i,j,kkfit)),j=1,i),rr(i+1:kkfit)
      end do
      if(iparm.eq.7)return

c     eigenvalue decomposition of coefficient matrix
      call all_eigen(kkfit)
      if(ii>0)stop 'alloc cofunc'
      call eigem(xkk,zero,es,kkfit,nneg)
      write(iun66,*)' '
      write(iun66,*)'Eigenvalues of coefficient matrix & cov. function'
      do i=1,kkfit
      write(iun66,902)i,eig(i)
      end do
      write(iun66,*)'eigenvectors'
      do i=1,kkfit
      write(iun66,902)i,vv(i,:kkfit)
      end do

c     transform eigenvectors to those of the covariance function
      call dxlgnd(kkfit)
      if(nq.eq.1.and. irropt(iparm,1).eq.1)then  !  -> need to adapt
         write(iun66,*)' '                      ! for individual traits 
         write(iun66,*)'Eigenfunctions of covariance function'
         do i=1,kkfit
         ework=0.d0
         do j=1,kkfit
         do ifit=1,kkfit
         ework(ifit)=ework(ifit)+vv(j,i)*clgndr(ifit,j)
         end do
         end do
         write(iun66,902)i,ework(:kkfit)
         end do

c        covariance function
         vv=0.d0
         do ifit=1,kkfit
         do jfit=1,kkfit
         do i=1,kkfit
         do j=1,kkfit
         vv(i,j)=vv(i,j)+clgndr(i,ifit)*clgndr(j,jfit)*
     *                              xkk(ihmssf(ifit,jfit,kkfit))
         end do
         end do
         end do
         end do
         write(iun66,*)' '
         write(iun66,*)'Coefficients of covariance function'
         do i=1,kkfit
         write(iun66,902)i-1,vv(i,:kkfit)
         end do
      else if(nq>1)then
         do iq=1,nq
         if(irropt(iparm,iq).ne.1)cycle
         do jq=iq,nq
         if(irropt(iparm,jq).ne.1)cycle
         write(iun66,*)' '
         if(iq.eq.jq)then
            write(iun66,*)'Covariance function for ',trait(iq)
         else
            write(iun66,*)'Cross-covariance function for ',trait(iq),
     &                    ' and ',trait(jq)
         end if
         vv=0.d0
         do ifit=1,kfit(iparm,iq)
         ifit1=sum(kfit(iparm,:iq-1))+ifit
         do jfit=1,kfit(iparm,jq)
         jfit1=sum(kfit(iparm,:jq-1))+jfit
         xx=xkk(ihmssf(ifit1,jfit1,kkfit))
         do i=1,kfit(iparm,iq)
         do j=1,kfit(iparm,jq)
         vv(i,j)=vv(i,j)+clgndr(i,ifit)*clgndr(j,jfit)*xx
         end do
         end do
         end do
         end do
         do i=1,kfit(iparm,iq)
         write(iun66,902)i-1,vv(i,:kfit(iparm,jq))
         end do
         end do ! jq
         end do ! iq
      end if

      do iq=1,nq
      write(iun66,*)'Form of random regression : ',iq,'  ',
     &                                          txt(irropt(iparm,iq))
      end do
      call deall_eigen
      return
 901  format(1x,a,t30,' =',i6)
 902  format(i4,(t6,8g13.6))
      end subroutine cofunc

!     ======================================================
      subroutine wr_varfun (ibegin, nb, ilgopt, kfou, omega)
!     ======================================================

      integer, intent(in) :: ibegin, nb, ilgopt, kfou
      real(8), intent(in) :: omega
      integer             :: ii, jj

      ii=mod(ilgopt,10)
      jj=ilgopt/10

!     Polynomial regression
      if(nb.eq.0)then
          write(iun66,*)'VF : No polynomial regression fitted '
      else
         write(iun66,*)'VF : polynomial regression (through mean age)'
         if(ii.eq.2)then
            write(iun66,'(1x,a,t25,a,g16.6)')'intercept',' =',
     &                                            xvec(ibegin+nb+1)
         else
            write(iun66,'(1x,a,t25,a,g16.6)')'intercept',' =',1.d0
         end if
         write(iun66,'(1x,a,t25,a,g16.6)')'linear coefficient',' =',
     &                                                xvec(ibegin+1)
         if(nb>1)write(iun66,'(1x,a,t25,a,g16.6)')
     &                 'quadratic coefficient',' =',xvec(ibegin+2)
         if(nb>2)write(iun66,'(1x,a,t25,a,g16.6)')
     &                 'cubic coefficient',' =',xvec(ibegin+3)
         do i=4,nb
         write(iun66,'(i3,a,t25,a,g16.6)')i,'th order coefficient',
     &                                            ' =',xvec(ibegin+i)
         end do
      end if

!     Fourier series approximation
      if(kfou>0)then
         write(iun66,*)'Have fitted Fourier series approximation '
         write(iun66,'(1x,a,t25,a,g16.6)')' "Omega"',' =',omega
         do ii=1,kfou
         write(iun66,'(1x,a,t22,i3,a,g16.6)')'Cosine coefficient',ii,
     &                                    ' =',xvec(ibegin+nb+ii*2-1)
         write(iun66,'(1x,a,t22,i3,a,g16.6)')'Sine coefficient',ii,
     &                                    ' =',xvec(ibegin+nb+ii*2)
         end do
      end if

!     variance
      write(iun66,'(1x,a,t25,a,g16.6)')'"Variance" @intercept',' =',
     &                                      xvec(ibegin+nb+kfou*2+1)

!     what have we modelled ...
      write(iun66,*)' Parameterisation : ',ilgopt
      if(jj.eq.1)write(iun66,*)'Function models Standard deviations'
      if(nb.eq.0 .and.kfou.eq.0)return

      if(ii.eq.0)then
         write(iun66,*)'Original Scale'
      else if(ii.eq.1)then
         write(iun66,*)'Log-linear scale : sig=sig_0 * exp{1 + sum ..)'
      else if(ii.eq.2)then
         write(iun66,*)'Log-linear scale : sig= exp{sig_0 + sum ..)'
      end if

      return
      end subroutine wr_varfun

!     ======================
      subroutine write_iun17
!     ======================

      use eigen_decomp
      use solutions
      use order
      use like_components, only : nrnkx

      integer          :: i,n,lim0=1
      character(len=25):: UNKN='unknown',FORMA='formatted',
     &                    FSTAND='DF17#DAT'

!     check that file is opened
      CALL FCONCT(IUN17,FSTAND,FORMA,UNKN)

!     write out parameter estimates with names
      do i=1,nparm
      if(param(i)(1:3).eq.'REG'.or.param(i)(1:4).eq.'KMAT')then
         write(iun17,'(g16.8,20x,a,1x,a)')xvec(i),param(i),regtyp(i)
      else
         write(iun17,'(g16.8,20x,a)')xvec(i),param(i)
      end if
      end do

!     maximum likelihood
      write(iun17,*)xlike,'   log L'

!     other info
      write(iun17,'(a,(7i3))')'order of fit   ',kfit
      if(ilgeps.eq.1)write(iun17,*)'log-linear scale for var. function'
      if(ioprun.eq.0)then
         write(iun17,'(a,7i3)')'no. eigenvalues',kfteig
         n=ksfit(7)  ! calculate reduced no. of parameters
         do i=1,5
         n=n+kfteig(i)*(ksfit(i)+1)-(kfteig(i)+1)/2
         end do
         write(iun17,'(a,2i4)')'no. parameters ',nparm,n
         do iq=1,nq
         do i=1,kftfix(iq)
         write(iun17,'(a,i3,g16.8)')' reg_fix',i,
     &                                      savsol(ieqnew(nfrst(iq)+i))
         end do
         end do
      end if
      write(iun17,'(i12,t25,a)')nrec,'No. of records'
      write(iun17,'(i12,t25,a)')nrnkx,'Rank of coefficient matrix F.E.'

      return
      end subroutine write_iun17

      end subroutine dxcovf






