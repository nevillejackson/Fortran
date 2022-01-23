!===========================================================================
      SUBROUTINE  DxRaZ3 (varvec,fvalue,iout,iopt)
!==============================================================km==1/2000===

!     residual cov matrices incorporating direct p.e. (co)variances

      use parameters, only : mxparm
      use units
      use parmap
      use combinations
      use numbers
      use traces
      use dmatrices
      use like_components, only : dete
      use residuals
      use ages
      use eigen_wrk
      use phimatrix, only : irropt

!     arguments
      real(8), dimension(mxparm), intent(inout) :: varvec
      real(8), intent(out)                      :: fvalue
      integer, intent(in)                       :: iopt
      integer, intent(out)                      :: iout

!     local variables
      real(8),dimension(:), allocatable         :: sigeps
      real(8)                                   :: es, xx, pp
      integer                                   :: ibegin

      iout=0 
      nqq=nq*(nq+1)/2

!     --------------------------
!     error covariance matrices
!     --------------------------
      ibegin=istrt(4)
 
!     multivariate analyses
      if(nq>1)then
         call all_eigen(nq)
         allocate (sigeps(nqq),stat=ii)
         if(ii>0)stop 'alloc sigeps'
         do i=1,kfit(7,1)
         sigeps=varvec(ibegin+1:ibegin+nqq)               ! measure errors
         call eigem(sigeps,zero,es,nq,ineg)
         if(ineg>0 )then
            write(*,*)'dxras3 : sigeps',i,sigeps
            if(iabs(iosrch).ne.3)then
               fvalue=big
               iout=1
               go to 99
            end if
!            varvec(istrt(4)+i)=10.d0*dsqrt(eigzer)
         end if
         ibegin=ibegin+nqq
         end do

!        pick out matrices for all ages
         do i=1,nage(1)
         k=istrt(4)+(meage(i)-1)*nqq
         eevec(i,:nqq)=varvec(k+1:k+nqq)
         end do
 99      deallocate (sigeps,stat=ii)
         if(ii>0)stop 'deall sigeps'
         call deall_eigen

!     step function or single m.e. var
      else if(kfit(7,1)>0)then
         do i=1,kfit(7,1)
         if(varvec(ibegin+i)<zero)then      ! variances must be non-zero
            write(*,*)'dxraz3 : sigeps',i,sigeps
            if(iabs(iosrch).ne.3)then
               fvalue=big
               iout=1
               return
            end if
         end if
         end do
!        pick out matrices for all ages
         if(kfit(7,1).eq.1)then
            eevec=varvec(nparm)
         else
            do i=1,nage(1)
            eevec(i,1)=varvec(istrt(4)+meage(i))
            end do
         end if

!     polynomial measurement error variance function
      else if(kfit(7,1).le.0)then    
         call check_varfun (varvec(ibegin+1:nparm-1),eevec(:nage(1),1), 
     &                      varvec(nparm), omegame,
     &                      zero,big,fvalue,-kfit(7,1),
     &                      ilgeps,kfoume,iout)
         if(iout>0)return

!        ... first derivatives of VF 
         if(iosrch.eq.3)call d1st_varfun(eevec, ee1vec,
     &                              varvec(nparm),-kfit(7,1),ilgpe)

      else                        
!        ... no measurement error variances fitted
         eevec=0.d0
      end if

       
      if(ieqmod.eq.0)return

!     -----------------------------------------
!     equivalent model for direct p.e. effects
!     -----------------------------------------

!     Legendre polynomials etc. :  pick out corresponding k-matrix 
      if(irropt(4,1)<4)then
         do i=1,ksfit(4)*(ksfit(4)+1)/2
         rxkk(i)=varvec(istrt(3)+i)
         end do

!     parametric correlation structure for p.e. effects
      else
         ibegin=istrt(3)
!        ... variance function
         if(kvfpe.ge.0)then
            call check_varfun (varvec(ibegin+1:ibegin+kvfpe+2*kfoupe),
     &                         pevec,varvec(ibegin+kvfpe+2*kfoupe+1),
     &                         omegape,zero,
     &                         big,fvalue,
     &                         kvfpe,ilgpe,kfoupe,iout)
            if(iout>0)return

!           ... first derivatives of VF for p.e. effects
            if(iosrch.eq.3)call d1st_varfun(pevec, pe1vec,
     &                              varvec(ibegin+kvfpe+1),kvfpe,ilgpe)
         else if(kvfpe.eq.-2)then
!           ... use link function to model p.e. variance
            do i=1,nage(1)
            xx=sqrt(eevec(i,1))
            pp=exp( varvec(ibegin+1)+varvec(ibegin+2)*log(xx) )
            pevec(i)=pp*pp
            end do
         end if

!        ... correlation parameters
         do i=1,ncpno
         rxkk(i)=varvec( ibegin+kfit(4,1)-ncpno+i )
         if(rxkk(i)<rhomin(i).or.rxkk(i)>rhomax(i))then  ! check validity
            iout=1
            fvalue=big
            return
         end if
         end do

!        correlation structure for SAD & 2nd order models
         if(mod(icorrf,10).ge.6)call corr_mat (icorrf)

      end if
      RETURN

      contains

!     =============================
      subroutine corr_mat( icorrf )
!     =============================

      integer, intent(in) :: icorrf
      real(8)             :: rho, lambda, f1,f2,rho1,rho2,b1,b2
      integer             :: i,j,k, jcorrf

      jcorrf=mod(icorrf,10)

!     first order SAD
      if(jcorrf.eq.6)then
         rho=rxkk(1)
         lambda=rxkk(2)
!        ... 1st off-diagonal -> that's where structure is imposed
         do i=1,nage(1)-1
         if(lambda.ne.0)then
            f1=(iiage(i,1)**lambda -1)/lambda
            f2=(iiage(i+1,1)**lambda -1)/lambda
          else
            f1=dlog(dble(iiage(i,1)))
            f2=dlog(dble(iiage(i+1,1)))
          end if
          xx=rho**(f2-f1)   ! 1st subdiagonal
          corrvec(ihmssf(i,i+1,nage(1)))=xx
          ww(i)=xx
          end do
!         ... other correlations ->  1st order antedependence structure
          do i=2,nage (1)
          xx=ww(i-1)
          do j=1,i-2
!          xx=1.d0
!          do k=j,i-1
!          xx=xx*corrvec(ihmssf(k,k+1,nage (1)))
!          end do
!          corrvec(ihmssf(i,j,nage (1)))=xx
          corrvec(ihmssf(i,j,nage(1)))=corrvec(ihmssf(i-1,j,nage(1)))*xx
          end do
          end do

!     second order AR
      else if (jcorrf.eq.7) then
          rho1=rxkk(1)
          rho2=rxkk(2)
          b1=rho1*(1.d0-rho2)/(1-rho1*rho1)
          b2=(rho2-rho1*rho1)/(1-rho1*rho1)
!         ... work out correlation for all possible lags
          ww(1)=rho1     ! 1st sub-diagonal = lag 1 correlation
          ww(2)=rho2     ! 2nd sub-diagonal = lag 2 correlation
          do k = 3, iiage(nage(1),1)-iiage(1,1)
          ww(k) = b1 *ww (k-1) + b2 * ww (k-2 ) ! lag k ...
          end do
!         ... set up correlation matrix for ages in data
          do i=1,nage(1)-1
          do j=i+1,nage(1)
          lag=iiage(j,1)-iiage(i,1)
          corrvec(ihmssf(i,j,nage(1)))=ww(lag)
          end do
          end do

!     first order ARMA
      else if (jcorrf.eq.8) then
          do i=1,nage(1)-1
          corrvec(ihmssf(i,i+1,nage(1)))=rxkk(2)
          do j=i+2,nage(1)
          lag=iiage(j,1)-iiage(i,1)
          corrvec(ihmssf(i,j,nage(1)))=rxkk(2)*(rxkk(1)**(lag-1))
          end do
          end do
          
!     second order SAD
      else if (jcorrf.eq.9) then
         rho=rxkk(1)
         lambda=rxkk(2)
!        ... 1st off-diagonal
         do i=1,nage(1)-1
         if(lambda.ne.0)then
            f1=(iiage(i,1)**lambda -1)/lambda
            f2=(iiage(i+1,1)**lambda -1)/lambda
         else
            f1=dlog(dble(iiage(i,1)))
            f2=dlog(dble(iiage(i+1,1)))
         end if
         corrvec(ihmssf(i,i+1,nage(1)))=rho**(f2-f1)   ! 1st subdiagonal
         end do
         rho=rxkk(3)
         lambda=rxkk(4)
!        ... 2nd off-diagonal
         do i=1,nage(1)-2
         if(lambda.ne.0)then
            f1=(iiage(i,1)**lambda -1)/lambda
            f2=(iiage(i+1,1)**lambda -1)/lambda
          else
            f1=dlog(dble(iiage(i,1)))
            f2=dlog(dble(iiage(i+1,1)))
          end if
          corrvec(ihmssf(i,i+2,nage(1)))=rho**(f2-f1)   ! 2nd subdiagonal
          end do
          do i=1,nage(1)-3
          rho1=corrvec(ihmssf(i,i+1,nage(1)))
          rho2=corrvec(ihmssf(i,i+2,nage(1)))
          b1=rho1*(1.d0-rho2)/(1-rho1*rho1)
          b2=(rho2-rho1*rho1)/(1-rho1*rho1)
          do k = 3, nage(1)
          corrvec(ihmssf(i,i+k,nage(1)))= 
     &                              b1*corrvec(ihmssf(i,i+k-1,nage(1)))
     &                            + b2*corrvec(ihmssf(i,i+k-2,nage(1)))
          end do
          end do
      end if

      return
      end subroutine corr_mat

      END subroutine dxraz3

!=========================================================================
      subroutine gg_inverse (xvec,iopt,ip,nobs,nna,nnq)
!=========================================================================

!     set up e-inverse & derivatives for one animal with nobs records
!     & ages at recording/traits as given in nna and nnq

      use combinations
      use ages
      use parameters, only : mxparm
      use parmap
      use numbers
      use residuals
      use dmatrices
      use units
      use like_components, only : dete
      use traces
      use phimatrix

      real(8), dimension(mxparm), intent(in)    :: xvec
      integer, intent(in)                       :: nobs,iopt,ip
      integer, dimension(mobs),intent(in)       :: nnq
      integer, dimension(mobs,nmeta),intent(in) :: nna
      real(8), dimension(mobs)                  :: w1,w2
      integer, dimension(mobs)                  :: iw
      real(8)                                   :: det,xx, cc_func
      integer                                   :: k,i,j,nrank
      real(8),dimension(mobs,mfitmx)            :: phy

!     --------------------------------------------------
!     set up inverse of residual error covariance matrix 
!     --------------------------------------------------

      work=0.d0
!     measurement error (co)variances
      call get_mevars

!     equivalent model : incorporate direct p.e. covariances
      if(ieqmod.eq.1)call add_pecovs

!     invert matrix 
      if(nq.eq.1.and.ieqmod.eq.0)then  ! residual cov matrix is diagonal
         det=0.d0
         sig=0.d0
         do i=1,nobs
         ii=ihmii(i,nobs)
         sig(i,i)=1.d0/work(ii)
         det=det+dlog(work(ii))
         end do

!      else if(nq>1 .and. ieqmod.eq.0)then   ! use that max. bandwith is nq
!        call dkmbnd (work, w1,w2,det,zero,iw,nrank,nobs,nq)

      else
         call dkmwhf(work,w1,w2,det,zero,iw,nrank,nobs,0)
         ij=0
         do i=1,nobs
         do j=i,nobs
         ij=ij+1
         sig(i,j)=work(ij)
         sig(j,i)=work(ij)
         end do
         end do
      end if

!     require e-inverse only
      if(iopt.eq.0)then
         dete(1)=dete(1)+det
         return   
      end if

!     ---------------------------------------
!     set up matrices R*(inv) x d R*/d theta
!     ---------------------------------------

      rd1=0.d0

!     set option as to how many matrices are needed ...
      if(iopt.eq.1)then       ! ... need RD for specific parameter only
         l1=ip                !      (dxmmd3 in dxspa3)
         l2=ip
         m1=ip
         m2=ip
      else  if(iopt.ge.2)then ! ... all matrices R**(-1)D required (for dxypy3)
         l1=1
         l2=nrparm
         m1=-(ngpar1+1)
         m2=-ngpar2
      end if

!     measurement error variances 
      if(l1>0) call get_rd1 (l1,l2)

!     permanent environmental covariances - implicitly
      if(ieqmod.eq.1 .and. (ip<0 .or.iopt.eq.2)) call get_rd2 (m1, m2)

!     --------------------------------------------------
!     set up -R*(inv) d R*/d theta R*(inv) (for iopt =1)
!     --------------------------------------------------

      if(iopt.eq.1)call get_rdr(ip)

      return

      contains

!     ======================     
      subroutine get_mevars
!     ======================     

      integer :: iobs, iq, iage, i, i1, jq

!     univariate analyses
      if(nq.eq.1)then                ! add diagonal matrix only
         do iobs=1,nobs              ! eevec has been set up previously
         iage=nna(iobs,1)
         work(ihmii(iobs,nobs))=eevec(iage,1)
         end do         

!     multivariate analyses : homogeneous cov only
      else ! if(nq>1)then            ! -> not diagonal but block-diagonal
         do iobs=1,nobs
         iq=nnq(iobs)
         iage=nna(iobs,1)
         work(ihmii(iobs,nobs))=eevec(iage,ihmii(iq,nq))
!        ... identify blocks
         jq=iq
         do i=1,nq-1
         i1=iobs+i
         if(i1>nobs)exit
         if(nna(i1,1).eq.iage.and. nnq(i1)>jq)then 
            jq=nnq(i1)                             
            work(ihmssf(iobs,i1,nobs))= eevec(iage,ihmssf(iq,jq,nq)) 
         end if
         end do
         end do ! iobs

      end if
      return
      end subroutine get_mevars

!     =====================
      subroutine add_pecovs
!     =====================

      integer :: iobs, jobs, iq, jq, iage, jage, ij, i, j, ii
      real(8) :: xx, cc_func

!     equivalent model for RR parameterisation for p.e. covariances

      if(irropt(4,1)<4 )then
!        pick out phimatrix for the animal
         phy=0.d0
         do iobs=1,nobs
         iq=nnq(iobs)
         iage=nna(iobs,1)
         phy(iobs,:kfit(4,iq))=phi(iage,:kfit(4,iq),4,iq) 
         end do

!        pre- and post-multiply k-matrix for p.e. CF
         ij=0
         do iobs=1,nobs
         iq=nnq(iobs)
         ii=sum(kfit(4,:iq-1))
         do jobs=iobs,nobs
         jq=nnq(jobs)
         jj=sum(kfit(4,:jq-1))
         ij=ij+1
         xx=0.d0
         do i=1,kfit(4,iq)
         do j=1,kfit(4,jq)
         xx=xx+phy(iobs,i)*rxkk(ihmssf(ii+i,jj+j,ksfit(4)))*phy(jobs,j)
         end do
         end do
         work(ij)=work(ij)+xx  ! add to residual cov matrix
         end do
         end do

!     parametric covariance structure for p.e. effects
      else if(irropt(4,1).eq.4)then
         ij=0
         do iobs=1,nobs
         iage=nna(iobs,1)
         do jobs=iobs,nobs
         jage=nna(jobs,1)
         ij=ij+1
         xx=dsqrt(pevec(iage)*pevec(jage)) ! product of standard deviations
         xx=xx*cc_func(iage,jage,iiage(iage,1),iiage(jage,1),nage(1),
     &                                                          rxkk)
         work(ij)=work(ij)+xx
         end do
         end do
      end if

      return
      end subroutine add_pecovs

!     =================================
      subroutine get_rd1 (ipar1, ipar2)
!     =================================

      integer, intent(in) :: ipar1, ipar2
      integer             :: iobs, iq, iage, jq, i1, ij, ll, i,j,k

!     measurement error variances : linear in parameters
      if(kfit(7,1)>0)then
!        ... set up "d"-matrices  
         de=.false.
         do iobs=1,nobs
         iq=nnq(iobs)
         iage=nna(iobs,1)
!        ... m.e. (co)variances
         ij=(meage(iage)-1)*nqq+ihmii(iq,nq)       ! diagonal element
         de(ihmii(iobs,nobs),ij)=.true.
!        ... identify blocks ! ... multivariate anal
         jq=iq
         do i=1,nq-1
         i1=iobs+i
         if(i1>nobs)exit
         if(nna(i1,1).eq.iage.and. nnq(i1)>jq)then
            jq=nnq(i1)
            ij=nqq*(meage(iage)-1)+ihmssf(iq,jq,nq)
            de(ihmssf(iobs,i1,nobs),ij)= .true.
         end if
         end do !i
         end do !iobs

!        premultiply d-matrices with inverse residual cov. matrix
         do ll=ipar1, ipar2
         do k=1,nobs
         do j=1,nobs
         if(de(ihmssf(k,j,nobs),ll))rd1(:nobs,j,ll)=rd1(:nobs,j,ll)+
     &                                                   sig(:nobs,k)
         end do
         end do
         end do ! ll=1,nrparm

!     measurement error variances : variance function -> non-linear
      else if(l1>0.and.kfit(7,1)<0)then
         do ll=ipar1,ipar2
         if(ieqmod.eq.0)then      ! residual is diagonal
            do k=1,nobs                             
            rd1(k,k,ll)=2.d0*eevec(k,1)*ee1vec(k,ll)
            end do           
         else if(ieqmod.eq.1)then ! residual is sum of p.e. + m.e var
            do k=1,nobs
            rd1(:nobs,k,ll)=sig(:nobs,k)*ee1vec(k,ll)
            end do
         end if
         end do ! ll=1,nrparm
      end if

!     accumulate traces r(-1)d(ij)
      if(iopt.eq.2)then
         do ll=ipar1,ipar2
         trres1(ngparm+ll,1)=trres1(ngparm+ll,1)+
     &                             sum( (/ (rd1(i,i,ll),i=1,nobs) /) )
         end do
      end if

      return
      end subroutine get_rd1

!     =================================
      subroutine get_rd2 (ipar1, ipar2)
!     =================================

      integer, intent(in) :: ipar1, ipar2
      integer             :: mm, jpar,ij, iobs, jobs, iq, jq, iage, jage,
     &                       i, j, k
      real(8)             :: xx, cc_func

      do mm=ipar1,ipar2,-1
      jpar=-mm-ngpar1

!     perm env. cov (implicitly ) : RR model (phi K phi')
      if(irropt(4,1)<4 )then
         call jhmssf(ksfit(4),jpar,iq,jq)    ! get iq & jq

         ij=0
         do iobs=1,nobs                      ! matrix of derivatives
         do jobs=iobs,nobs
         ij=ij+1
         if(iq .ne. jq)then
           dr(ij)=phy(iobs,iq)*phy(jobs,jq)+phy(iobs,jq)*phy(jobs,iq)
         else !if(iq.eq.jq) then
           dr(ij)=phy(iobs,iq)*phy(jobs,jq)
         end if
         end do ! jobs
         end do ! iobs

!     matrices e-inverse x d-matrix for parametric correlation structure
      else if(irropt(4,1).eq.4)then

!        variance function for p.e. variances
         ij=0
         do iobs=1,nobs                     
         iage=nna(iobs,1)
         do jobs=iobs,nobs
         jage=nna(jobs,1)         ! this code incomplete & unchecked !!!
         ij=ij+1
         dr(ij)=(pevec(iobs)*pe1vec(jobs,jpar)+
     &           pevec(jobs)*pe1vec(iobs,jpar)) *
     &   cc_func(iage,jage,iiage(iage,1),iiage(jage,1),nage(1), rxkk)
         end do
         end do
      end if      

      do iobs=1,nobs                      ! pre-multiply with R**(-1)
      do jobs=1,nobs                      ! (result not symmetric !)
      xx=0.d0
      do k=1,nobs
      xx=xx+sig(iobs,k)*dr(ihmssf(k,jobs,nobs))
      end do 
      rd2(iobs,jobs,jpar)=xx
      end do ! jobs
      end do ! iobs

      end do ! mm

!     accumulate traces r(-1)d(ij) 
      if(iopt.eq.2)then
         do mm= ipar1, ipar2,-1
         jpar=-mm
         if(iopt.eq.2)trres1(jpar,1)=trres1(jpar,1)+
     &                          sum((/(rd2(i,i,jpar-ngpar1),i=1,nobs)/))
         end do
      end if

      return
      end subroutine get_rd2

!     =======================
      subroutine get_rdr (ip)
!     =======================

      integer, intent(in) :: ip
      real(8)             :: dd
      integer             :: i,j,k, ij, jpar

      work=0.d0

!     measurement error variances
      if(ip>0 )then                    
         do  k=1,nobs
         do  i=1,nobs
         dd=rd1(i,k,ip)
         if(dd.ne.0)then
            do j=i,nobs
            ij=ihmssf(i,j,nobs)
            work(ij)=work(ij)+dd*sig(j,k)
            end do
         end if
         end do
         end do

!     permanent environmental covariance
      else                             
         jpar=-ip-ngpar1
         ij=0
         do i=1,nobs
         do j=i,nobs
         ij=ij+1
         xx=0.d0
         do k=1,nobs
         xx=xx+rd2(i,k,jpar)*sig(k,j)
         end do
         work(ij)=xx
         end do
         end do
      end if

!     fullstore & add minus sign
      ij=0
      do i=1,nobs
      do j=i,nobs
      ij=ij+1
      sig(i,j)=-work(ij)
      sig(j,i)=-work(ij)
      end do
      end do

      return
      end subroutine get_rdr

      end subroutine gg_inverse

!======================================
      subroutine jhmssf (nq,ipar,iq,jq)
!======================================

      integer, intent(in)   :: nq,ipar
      integer, intent(out)  :: iq,jq
      integer               :: ij

      ij=0
      do  iq=1,nq
      do  jq=iq,nq
      ij=ij+1
      if(ipar.eq.ij)return
      end do
      end do
      print *,ipar,nq
      stop 'jhm'
      end subroutine jhmssf

!=====================================================================
      subroutine check_varfun (bvec,svec,sig0,omega,zero,big,fval,
     &                         nb,klg,kfou,ind)
!=====================================================================

      use ages
      use phimatrix

      real(8), dimension(nb+2*kfou), intent(in):: bvec
      real(8), dimension(nage(1)), intent(out) :: svec
      real(8), intent(in)                      :: sig0, zero, big, omega
      real(8), intent(inout)                   :: fval
      integer, intent(in)                      :: nb, klg, kfou
      integer, intent(out)                     :: ind
      real(8)                                  :: xx,aa
      integer                                  :: ilg, jlg

      if(sig0<zero)go to 99 ! variance must be greater than zero
      ind=0                 

      ilg=mod(klg,10)         ! 0=normal, 1=log-lin 1, 2=log-lin 2
      jlg=mod(klg,100)/10     ! 0=variances, 1=sdev

!     For each age ...
      do i=1,nage(1)

!     ... polynomial variance function
      if(klg<100)then         ! ordinary polynomials of standardised age
         xx=1.d0
         if(ilg.eq.2)xx=sig0
         aa=astar(i)
         do j=1,nb
         xx=xx+bvec(j)*(aa**j)
         end do
      else                    ! Legendre polynomials
         xx=phi(i,1,6,1)
         if(ilg.eq.2)xx=xx*sig0
         do j=1,nb
         xx=xx+bvec(j)*phi(i,j+1,6,1)
         end do
      end if

!     ... overlay fourier series approximation
      aa=iiage(i,1)   ! non-standardised age !
      do j=1,kfou
      xx=xx+bvec(nb+2*j-1)*cos(omega*aa*j)+bvec(nb+2*j)*sin(omega*aa*j)
      end do

!     scaling
      if(ilg.eq.0)then
         svec(i)=xx*sig0
      else if(ilg.eq.1)then
         if(xx<-308.or.xx>308)go to 99
         svec(i)=dexp(xx)*sig0
      else if(ilg.eq.2)then
         if(xx<-308.or.xx>308)go to 99
         svec(i)=dexp(xx)
      else if(ilg.eq.3)then
         if(xx*sig0<-308.or.xx*sig0>308)go to 99
         svec(i)=dexp(xx*sig0)
      end if
      if(jlg.eq.1)svec(i)=svec(i)**2   ! we've modelled SD rather than Var
      if(svec(i)<zero)go to 99         ! all variances must be positive

      end do  ! age

      return

99    ind=1                  ! invalid VF
      fval=big
      return
      end subroutine check_varfun

!======================================================================
      subroutine d1st_varfun (svec,s1vec,zig0,nb,ilg)
!==========================================================km=10/2000==

!     ... evaluate first derivatives for parameters of variance function

      use ages
      real(8), dimension(nage(1)), intent(in)      :: svec
      real(8), dimension(nage(1),nb+1), intent(out):: s1vec
      real(8), intent(in)                          :: zig0
      integer, intent(in)                          :: nb, ilg
      integer                                      :: ia, ipar

      if(ilg>100)stop 'deriv for orth pol not done'
      s1vec=0.d0
!     ... derivatives w.r.t. regression coefficients
      do ipar=1,nb
      do ia=1,nage(1)
      if(ilg.eq.0)then              ! var. orig. scale
         s1vec(ia,ipar)=0.5d0*zig0*astar(ia)**ipar/dsqrt(svec(ia))
      else if(ilg.eq.1)then         ! var. log-lin 1
         s1vec(ia,ipar)=0.5d0*astar(ia)**ipar*dsqrt(svec(ia))
      else if(ilg.eq.2)then         ! var. log-lin 2
          s1vec(ia,ipar)=0.5d0*astar(ia)**ipar*dsqrt(svec(ia))
      else if(ilg.eq.3)then         ! var. log-lin 1
         s1vec(ia,ipar)=0.5d0*zig0*astar(ia)**ipar*dsqrt(svec(ia))
      else if(ilg.eq.10)then        ! sdev orig scale
          s1vec(ia,ipar)=zig0*astar(ia)**ipar
      else if(ilg.eq.11)then        ! sdev log-lin 1
          s1vec(ia,ipar)=astar(ia)**ipar*dsqrt(svec(ia))
      else if(ilg.eq.12)then        ! sdev log-lin 2
          s1vec(ia,ipar)=astar(ia)**ipar*dsqrt(svec(ia))
      else if(ilg.eq.13)then        ! sdev log-lin 1
          s1vec(ia,ipar)=zig0*astar(ia)**ipar*dsqrt(svec(ia))
      end if
      end do
      end do

!     ... w.r.t. to var/sd at intercept
      ipar=nb+1
      do ia=1,nage(1)
      if(ilg.eq.0)then
         s1vec(ia,ipar)=0.5d0*dsqrt(svec(ia))/zig0
      else if(ilg.eq.1)then
         s1vec(ia,ipar)=0.5d0*dsqrt(svec(ia))/zig0
      else if(ilg.eq.2)then
          s1vec(ia,ipar)=0.5d0*dsqrt(svec(ia))
      else if(ilg.eq.3)then
         s1vec(ia,ipar)=0.5d0*dsqrt(svec(ia))*dlog(svec(ia))/zig0
      else if(ilg.eq.10)then
          s1vec(ia,ipar)=svec(ia)/zig0
      else if(ilg.eq.11)then
          s1vec(ia,ipar)=svec(ia)/zig0
      else if(ilg.eq.12)then
          s1vec(ia,ipar)=dsqrt(svec(ia))
      else if(ilg.eq.13)then
          s1vec(ia,ipar)=svec(ia)*dlog(svec(ia))/zig0
      end if
      end do

      return
      end subroutine d1st_varfun

!=================================================================
      double precision function cc_func(ii,jj,iage,jage,nage,parv)
!=================================================================

      use parmap
      use units, only : pi

      implicit none
      integer, intent(in)                    :: ii,jj,iage,jage,nage
      real(8), dimension(ncpno), intent (in) :: parv
      integer                                :: lag,jcorrf, kk
      integer, external                      :: ihmssf
      real(8)                                :: cc, omega

      if(iage.eq.jage)then
         cc_func=1.d0
         return
      end if

      jcorrf=mod(icorrf,10)

      if(jcorrf.eq.0)then                         ! compound symmetry
         cc_func=parv(1)                 
      else if(jcorrf <6)then
         lag=iabs(iage-jage)
         if(icorrf.eq.1)then                      ! autocorrelation 
            cc=parv(1)**lag
         else if(jcorrf.eq.2)then                 ! exponential
            cc=dexp(-parv(1)*lag)              
         else if(jcorrf.eq.3)then                 ! gaussian
            cc=dexp(-lag*lag*parv(1) )         
         else if(jcorrf.eq.4)then                 ! damped autocorrelation
            cc=parv(1)**( lag**parv(2) )       
         else if(jcorrf.eq.5)then                 ! damped exponential
            cc=dexp(-(lag**parv(2))* parv(1) )
         end if
      else if(jcorrf.ge.6)then                    ! 1st order SAD, etc
         cc=corrvec(ihmssf(ii,jj,nage))
         if(icorrf>19)lag=iabs(iage-jage)
      end if

      if(icorrf>9.and.icorrf<20)then
!         ... correlation has constant, age independent part
          cc=parv(ncpno)+(1.d0-parv(ncpno))*cc
      else if(icorrf>19)then
!         ... correlation has cyclic part
          kk=period
          omega=2.d0*pi/period
          cc=parv(ncpno)*cos(omega*mod(lag,kk))+(1.d0-parv(ncpno))*cc
      end if
      cc_func=cc

      return
      end function cc_func








