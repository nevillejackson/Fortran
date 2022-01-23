C==========================================================================
      SUBROUTINE  DxIPM3 (xvec)
C==========================================================================

      use parameters, only :mxparm
      use numbers
      use parmap, only : ipm, ffvec
      use eigen_decomp
      use units
      use platform

!     arguments
      real(8), dimension(mxparm), intent(inout) :: xvec

!     local variables
      integer                                   :: ibegin=0,nn,i,k,ii

      write(*,*)'Covariance of additive genetic regression coefficients'
      call fix_elements(1)

      if(ioprn2.eq.1)then
         write(*,*)'Covariance of 2nd animal regression coefficients'
         call fix_elements(2)
      end if

      write(*,*)'Covariance of perm. environ. regression coefficients'
      call fix_elements(4)

      if(kfit(7,1).gt.1)then
         write(*,fmt(ipltf))
     &               'no. of measurement error variances to be fixed ?'
         call option( nn,0,sum(kfit(7,:nq)) )
         do i=1,nn
         write(*,fmt(ipltf))'fix m.e. variance no. ?'
         call option(k,1,sum(kfit(7,:nq)))
         ipm(ibegin+k)=1
         if(xvec(ibegin+k)*ffvec(ibegin+k).lt.1.d0)then
            write(*,*)'value =',xvec(ibegin+k)*ffvec(ibegin+k)
            write(*,*)'set to zero ?'
            call yesno(ii)
            if(ii.eq.1)xvec(ibegin+k)=zero
         end if
         end do
      end if
      RETURN

      contains

!     ============================
      subroutine fix_elements(ipar)
!     ============================

      integer, intent(in) :: ipar
      integer             :: i,j,kk

      m=kfteig(ipar)-1
      nn=0
      if(m>0)then
         write(*,fmt(ipltf))
     &             'Fix columns 1 to N of the cholesky matrix - N = ?'
         call option(nn,0,m)
      end if
      if(nn.eq.0)go to 99

      kk=ibegin
      do i=1,nn
      kk=kk+1
      write(*,*)'fix diag. element',i,' at value =',xvec(kk)*ffvec(kk)
      ipm(kk)=1
      end do
      do i=1,nn
      do j=i+1,kfteig(ipar)
      kk=kk+1
      ipm(kk)=1
      write(*,*)'fix off-diag. el.',i,j,' at value =',xvec(kk)*ffvec(kk)
      end do
      end do

 99   ibegin=ibegin+kfteig(ipar)
      do i=1,kfteig(ipar)
      ibegin=ibegin+ksfit(ipar)-i
      end do
      return
      end subroutine fix_elements

      END subroutine dxipm3






