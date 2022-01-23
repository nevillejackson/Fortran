c========================================================================
      subroutine      dxktg3 (xvec,fvalue,nparm,iout,iopt)
c========================================================================

c     purpose :       replace parameter vector (= coefficients of K
c                     matrices) by corresponding covariances
c-----------------------------------------------------------km--8/95------

      use params
      use units
      use phimatrix
      use parmap
      use numbers

!     arguments
      real(8), dimension(mxparm), intent(inout) :: xvec
      real(8), intent(out)                      :: fvalue
      integer, intent(in)                       :: nparm,iopt
      integer, intent(out)                      :: iout

!     local variables
      real(8), dimension(:), allocatable        :: eig, sig,xkk
      real(8), dimension(:,:), allocatable      :: vv
      integer                                   :: ineg,i,j,ij,l,kk,ii
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate(vv(nq,nq),eig(nq),sig(nqq),xkk(nqq),stat=ii)
      if(ii>0)stop 'alloc dxktg3'

c     check that parameter vector is within bounds
      fvalue=big
      iout=1
c     ... additive genetic
      ij=0
      do i=1,kfit(1)
      do j=i,kfit(1)
      ij=ij+1
      xkk(ij)=xvec(ij)
      end do
      end do
      call eigem(xkk,eig,zero,es,nq,vv,kfit(1),ineg)
      if(ineg.gt.0.and.iopt.eq.0)then
         return
      else if(ineg.gt.0)then
         print *,'kmat a',(eig(l),l=1,kfit(1)),ineg
         call chkmat (xvec,xkk,eig,vv,zero,kfit(1),0,nqq,nq)
      end if

c     ... maternal genetic
      if( nosvec(15) .eq.1 )then
         kk=ij
         do i=1,kfit(2)
         do j=i,kfit(2)
         ij=ij+1
         xkk(ij-kk)=xvec(ij)
         end do
         end do
         call eigem(xkk,eig,zero,es,nq,vv,kfit(2),ineg)
         if(ineg.gt.0.and.iopt.eq.0)then
            return
         else if(ineg.gt.0)then
            print *,'kmat m',(eig(l),l=1,kfit(2)),ineg
            call chkmat (xvec,xkk,eig,vv,zero,kfit(2),kk,nqq,nq)
         end if
      end if
c     worry about models with sig am some other time ...
c     ... additional random effect
      if(nosvec(14).eq.1)then
         kk=ij
         do i=1,kfit(4)
         do j=i,kfit(4)
         ij=ij+1
         xkk(ij-kk)=xvec(ij)
         end do
         end do
         call eigem(xkk,eig,zero,es,nq,vv,kfit(4),ineg)
         if(ineg.gt.0.and.iopt.eq.0)then
            return
         else if(ineg.gt.0)then
            print *,'kmat c',(eig(l),l=1,kfit(4)),ineg
            call chkmat (xvec,xkk,eig,vv,zero,kfit(4),kk,nqq,nq)
         end if
      end if
c     ... errors
      kk=ij
      do i=1,kfit(5)
      do j=i,kfit(5)
      ij=ij+1
      xkk(ij-kk)=xvec(ij)
      end do
      end do
      call eigem(xkk,eig,zero,es,nq,vv,kfit(5),ineg)
      if(ineg.gt.0.and.iopt.eq.0)then
         return
      else if(ineg.gt.0)then
         print *,'kmat e',(eig(l),l=1,kfit(5)),ineg
         call chkmat (xvec,xkk,eig,vv,zero,kfit(5),kk,nqq,nq)
      end if
      if(iomease.eq.1)then
         do i=1,nq
         ij=ij+1
         if(iopt.eq.0 .and. xvec(ij).lt.zero)return
         end do
      end if

      iout=0
      fvalue=0.d0
      if(iopt.eq.2)return

c     "uncondense" parameter vector
      do i=nparm+nq,1,-1
      ii=ntok(i)
      if(ii.gt.0)then
        xvec(i)=xvec(ii)
      else
        xvec(i)=0.d0
      end if
      end do 

c     additive genetic
      xkk=xvec(:nqq)
      call ktog(xkk,sig,nq,kfit(1))
      xvec(:nqq)=sig

c     maternal genetic
      if( nosvec(15) .eq.1 )then
         xkk=xvec(istrt(1)+1:istrt(1)+nqq)
         call ktog(xkk,sig,nq,kfit(2))
         xvec(istrt(1)+1:istrt(1)+nqq)=sig
      end if

c     worry about models with sig am some other time ...
      
c     additional random effect
      if(nosvec(14).eq.1)then
         xkk=xvec(istrt(3)+1:istrt(3)+nqq)
         call ktog(xkk,sig,nq,kfit(4))
         xvec(istrt(3)+1:istrt(3)+nqq)=sig
      end if

c     error components
      xkk=xvec(istrt(4)+1:istrt(4)+nqq)
      call ktog(xkk,sig,nq,kfit(5))
      xvec(istrt(4)+1:istrt(4)+nqq)=sig

c     leave out models with correlated residuals too for the moment

c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     NB : this subroutine assume all nqq var comp. are fitted for 
c          each random factor !
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      deallocate(vv,eig,sig,xkk,stat=ii)
      if(ii>0)stop 'alloc dxktg3'
      return
      end subroutine dxktg3

c===========================================================================
      subroutine chkmat (xvec,xkk,eig,vv,zero,kkfit,ibegin,nqq,nq)
c===========================================================================

!     arguments
      real(8), dimension(ibegin+1:ibegin+kkfit*(kkfit+1)/2),
     &                            intent(inout) :: xvec
      real(8), dimension(nqq), intent(out)      :: xkk
      real(8), dimension(kkfit), intent(inout)  :: eig
      real(8), dimension(nq,nq), intent(inout)  :: vv
      real(8), intent(in)                       :: zero
      integer, intent(in)                       :: kkfit, ibegin,nqq,nq

!     local variables
      integer                                   :: i

      do i=1,kkfit
      if(eig(i).lt.zero)eig(i)=zero
      end do
      xkk=0.d0
      xkk( (/ (ihmii(i,kkfit),i=1,kkfit) /) ) = eig(:kkfit)
      call invrt(vv,nq,kkfit)
      call matrba(vv,xkk,nq,kkfit)
      xvec(ibegin+1:ibegin+kkfit*(kkfit+1)/2)=xkk(:kkfit*(kkfit+1)/2)
      return
      end subroutine chkmat







