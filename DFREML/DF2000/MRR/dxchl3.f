!======================================================================
      subroutine dxchl33
!======================================================================

      use parameters
      use numbers
      use eigen_decomp
      use units
      use parmap
      use phimatrix, only : irropt

      integer                                   :: ibegin,i,ii,kk
      real(8)                                   :: ee

      kfteig=0

!     determine order of fit
      if(kopt.eq.3)then
         if(nanim>0)then
            write(*,*)'no. of non-zero eigenvalues for genetic CF ?'
            call optdef(kfteig(1),1,ksfit(1),ksfit(1))
         end if

         if(ioprn2.eq.1)then
            write(*,*)'no. of non-zero eigenvalues for',
     &                                        ' "2nd animal" CF ?'
            kk=min0(ksfit(2),kfteig(1))
            call optdef(kfteig(2),1,ksfit(2),kk)
         end if
         
         if(ioprn1.eq.1.and.irropt(4,1)<4)then
            write(*,*)'no. of non-zero eigenvalues for perm. ',
     &                                            'environm. CF ?'
            call optdef(kfteig(4),1,ksfit(4),ksfit(4))
         else if(ioprn1.eq.1)then
            kfteig(4)=ksfit(4)
         end if
         if(ioprn3.eq.1)then
            write(*,*)'no. of non-zero eigenvalues for add. ',
     &                                            'R.E. CF ?'
            if(kfteig(1)>0)then
               kk=min0(ksfit(5),kfteig(1))
            else
               kk=ksfit(5)
            end if
            call optdef(kfteig(5),1,ksfit(5),kk)
         end if
      else
         kfteig=ksfit
      end if

      kkparm=kfteig(1)*ksfit(1)-kfteig(1)*(kfteig(1)-1)/2
     &      +kfteig(4)*ksfit(4)-kfteig(4)*(kfteig(4)-1)/2 
      if(kfit(7,1)>0)then
         kkparm=kkparm+sum(kfit(7,:nq))
      else if(kfit(7,1)<0)then
         kkparm=kkparm+iabs(kfit(7,1))+1
      end if
      if(ioprn2.eq.1)kkparm=kkparm+kfteig(2)*ksfit(2)-kfteig(2)*
     &                                           (kfteig(2)-1)/2
      if(ioprn3.eq.1)kkparm=kkparm+kfteig(5)*ksfit(5)-kfteig(5)*
     &                                           (kfteig(5)-1)/2

      allocate(kindx(kfitmx,6),stat=ii)
      if(ii>0)stop 'alloc kindx'

      ee=sqrt(eigzer)
      ibegin=0
      if(nanim>0)call cholesky(1)
      if(ksfit(2)>0)call cholesky(2)
      if(ksfit(4)>0.and.irropt(4,1)<4)then
         call cholesky(4)
      else if(irropt(4,1).eq.4)then
         if(kvfpe+kfoupe.ge.0)then
            ibegin=ibegin+kvfpe+2*kfoupe+1      ! regression coeff.s
            if(xvec(ibegin).gt.eigzer)then    ! variance
               xvec(ibegin)=sqrt(xvec(ibegin))
            else
               xvec(ibegin)=ee
            end if
            if(kopt.eq.2)xvec(ibegin)=dlog(xvec(ibegin))
            ibegin=ibegin+ncpno                  ! correlation param.s
         else
            ibegin=ibegin+kfit(4,1)              ! regression coeff.s
         end if
      end if
      if(ksfit(5)>0)call cholesky(5)

!     measurement error variances
      if(kfit(7,1)>0)then
         do i=ibegin+1,kparm
         if(xvec(i).gt.eigzer)then
            xvec(i)=sqrt(xvec(i))
         else
            xvec(i)=ee
         end if
         if(kopt.eq.2)xvec(i)=dlog(xvec(i))
         end do
      else  if(kfit(7,1)<0)then             ! only transform last parameter
         if(xvec(kparm).gt.eigzer)then
            xvec(kparm)=sqrt(xvec(kparm))
         else
            xvec(kparm)=ee
         end if
         if(kopt.eq.2)xvec(kparm)=dlog(xvec(kparm))
      end if
      return

      contains

c     ========================
      subroutine cholesky (ii)
c     ========================

      integer,intent(in)                      :: ii

      integer                                 :: kp,i,k,kj,j,ij,ik
      real(8)                                 :: pivot,xx

      kindx(:ksfit(ii),ii)=(/ (i,i=1,ksfit(ii)) /)

      do i=1,kfteig(ii)

      pivot=0.d0
      do j=i,ksfit(ii)
      xx=xvec( ihmii(kindx(j,ii),ksfit(ii))+ibegin )
      if(xx>pivot)then
         pivot=xx
         kp=j
      end if
      end do
      if(pivot.lt.zero)then
        write(*,*)'small pivot encountered '
        write(*,*)'covariance matrix no. =',ii
        write(*,*)'kfit =',ksfit(ii),' kfteig=',kfteig(ii),' nr=',i-1
        pivot=eigzer
      end if

      if(kp.ne.i)then              ! exchange rows and columns
         j=kindx(i,ii)
         kindx(i,ii)=kindx(kp,ii)
         kindx(kp,ii)=j
      end if

      pivot=sqrt(pivot)
      xvec(ihmii(kindx(i,ii),ksfit(ii))+ibegin)=pivot
      do j=i+1,ksfit(ii)
c     adjust lead row elements
      kj=ibegin+ihmssf(kindx(i,ii),kindx(j,ii),ksfit(ii))
      xvec(kj)=xvec(kj)/pivot
c     row operations
      do k=i+1, j 
      ik=ihmssf(kindx(i,ii),kindx(k,ii),ksfit(ii))+ibegin
      ij=ihmssf(kindx(k,ii),kindx(j,ii),ksfit(ii))+ibegin
      xvec(ij)=xvec(ij)-xvec(kj)*xvec(ik)
      end do
      end do
      end do ! i

      do i=kfteig(ii)+1,ksfit(ii)
      do j=i,ksfit(ii)
      if(i.eq.j)then
         xvec(ibegin+ihmssf(kindx(i,ii),kindx(j,ii),ksfit(ii)))=ee
      else
         xvec(ibegin+ihmssf(kindx(i,ii),kindx(j,ii),ksfit(ii)))=0.d0
      end if
      ipm(ibegin+ihmssf(kindx(i,ii),kindx(j,ii),ksfit(ii))) =1
      end do
      end do

!     allow small diag.s to be increased (starting values)
      do i=1,kfteig(ii)
      ij=ihmii(kindx(i,ii),ksfit(ii))+ibegin
      if(xvec(ij).lt.0.1)then
         write(*,*)'small diagonal  - set to ? '
         call rvldef(XX,eigzer,1.d6,xvec(ij))
         xvec(ij)=xx  
      end if
      if(kopt.eq.2)xvec(ij)=dlog(xvec(ij))
      end do

      ibegin=ibegin+ksfit(ii)*(ksfit(ii)+1)/2
      return
      end subroutine cholesky
 
      end subroutine dxchl33
















