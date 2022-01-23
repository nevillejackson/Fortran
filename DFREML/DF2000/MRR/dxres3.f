!===========================================================================
      SUBROUTINE  DxRES3 (varvec,fvalue,iout,iopt)
!===========================================================================

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

!     arguments
      real(8), dimension(mxparm), intent(inout) :: varvec
      real(8), intent(out)                      :: fvalue
      integer, intent(in)                       :: iopt
      integer, intent(out)                      :: iout

!     local variables
      real(8)                                   :: sigeps

      iout=0 

      if(kfit(7,1)>0)then
         if(iopeps.eq.1)then
!           transform ratios to variance components
            do i=1,kfit(7,1)-1
            varvec(istrt(4)+i)=varvec(istrt(4)+i)*varvec(nparm)
            end do
         end if
         do i=1,kfit(7,1)
         sigeps=varvec(istrt(4)+i)
         if(sigeps.lt. 10.d0*dsqrt(eigzer) )then
            print *,'dxres3 : sigeps',i,sigeps
            if(iabs(iosrch).ne.3)then
               fvalue=big
               iout=1
               return
            end if
           varvec(istrt(4)+i)=10.d0*dsqrt(eigzer)
         end if
         end do

         do i=1,nage(1)
         eevec(i,1)=varvec(istrt(4)+meage(i))
         end do

      else                            ! fit variance function for sigeps

         ibegin=istrt(4)
         call check_varfun (varvec(ibegin+1:nparm-1),eevec(:nage(1),1), 
     &          varvec(nparm), zero,big,fvalue,-kfit(7,1),ilgeps,iout)
         if(iout>0)return
         zigeps=varvec(nparm)
      end if

      if(ncomb.le.max_comb)then
         eei=0.d0
         if(iopt.gt.0)rd=0.d0
      end if

      DO ICOMB=1,NCOMB

      call ee_inverse(varvec,icomb,0,0)
      nr=sum( mmark(:,icomb) )
      dete(icomb)=- sum( (/ (dlog(sig(i,i)),i=1,nr) /) )
      if(ncomb.le.max_comb)eei(:nr,:nr,icomb)=sig(:nr,:nr)
      if(iopt.eq.0)cycle

      call ee_inverse(varvec,icomb,2,0)
      do ll=1,nrparm
      trres1(ngparm+ll,icomb)=sum( (/ (rd1(i,i,ll),i=1,nr) /) )
      end do

      if(ncomb.le.max_comb)then
         rd(:nr,:nr,:nrparm,icomb)=rd1(:nr,:nr,:nrparm)
         do ll=1,nrparm
         call ee_inverse(varvec,icomb,1,ll)
         nrr=nr*(nr+1)/2
         rdr(:nrr,ll,icomb)=(/ ((sig(i,j),j=i,nr),i=1,nr) /)
         end do
      end if

      end do   ! icomb

      RETURN
      END subroutine dxres3

!=========================================================================
      subroutine ee_inverse(xvec,icomb,iopt,ip)
!=========================================================================

      use combinations
      use ages
      use parameters, only : mxparm
      use parmap
      use numbers
      use residuals
      use dmatrices

      real(8), dimension(mxparm), intent(in) :: xvec
      integer, intent(in)                    :: icomb,iopt,ip
      integer                                :: k,i,j
      integer                                :: iq=1,jlgeps,klgeps

      if(iopt>0.and.kfit(7,1)<0)then
        jlgeps=mod(ilgeps,10)
        klgeps=mod(ilgeps,100)/10
        if(klgeps.eq.1)then
           print *,'derivatives for modelling SDEVS not yet implemented'
           stop 'use variances or DF alg'
        end if
      end if

      if(iopt.eq.0)then    ! e-inverse only
        sig=0.d0
        k=0
        do i=1,nage(1)
        do j=1,mmark(i,icomb)  ! allow for >1 rec. for the same age !
        k=k+1                  
        sig(k,k)=1.d0/eevec(i,1)
        end do
        end do

      else if(iopt.eq.2)then ! products e-inverse & D-matrices
        rd1=0.d0
        k=0
        if(kfit(7,1)>0)then
           do i=1,nage(1)
           do j=1,mmark(i,icomb)
           k=k+1
           rd1(k,k,meage(i))=1.d0/eevec(i,1)
           end do
           end do
        else    ! ... variance function
           do i=1,nage(1)
           do j=1,mmark(i,icomb)
           k=k+1
           do jp=1,nrparm-1
           if(jlgeps.eq.0)then
              rd1(k,k,jp)=(zigeps/eevec(i,1))*(astar(i)**jp)
           else if(jlgeps.eq.1)then
              rd1(k,k,jp)=astar(i)**jp
           else if(jlgeps.eq.2)then
              rd1(k,k,jp)=astar(i)**jp
           else if(jlgeps.eq.3)then
              rd1(k,k,jp)=zigeps*astar(i)**jp
           end if
           end do
           if(jlgeps.eq.0)then
              rd1(k,k,nrparm)=1.d0/zigeps
           else if(jlgeps.eq.1)then
              rd1(k,k,nrparm)=1.d0/zigeps
           else if(jlgeps.eq.2)then
              rd1(k,k,nrparm)=1.d0
           else if(jlgeps.eq.3)then
              rd1(k,k,nrparm)=dlog(eevec(i,1))/zigeps
           end if
           end do
           end do
        end if

      else if(iopt.eq.1)then  ! matrices e-inverse x D x e-inverse
        sig=0.d0
        k=0
        if(kfit(7,1)>0)then
           do i=1,nage(1)
           do j=1,mmark(i,icomb)
           k=k+1
           if(meage(i).eq.ip)sig(k,k)=-1.d0/(eevec(i,1)**2)
           end do
           end do
        else if(ip.eq.nrparm)then
           do i=1,nage(1)
           do j=1,mmark(i,icomb)
           k=k+1
           if(jlgeps.eq.0)then
              sig(k,k)=-1.d0/(eevec(i,1)*zigeps)
           else if (jlgeps.eq.1)then
              sig(k,k)=-1.d0/(eevec(i,1)*zigeps)
           else if(jlgeps.eq.2)then
              sig(k,k)=-1.d0/eevec(i,1)
           else if(jlgeps.eq.3)then
              sig(k,k)=-1.d0*dlog(eevec(i,1))/(eevec(i,1)*zigeps)
           end if
           end do           
           end do 
        else          
           do i=1,nage(1)
           do j=1,mmark(i,icomb)
           k=k+1
           if(jlgeps.eq.0)then
              sig(k,k)=(-1.d0*zigeps*astar(i)**ip)/(eevec(i,1)**2)
           else if(jlgeps.eq.1)then
              sig(k,k)=(-1.d0*astar(i)**ip)/eevec(i,1)
           else if(jlgeps.eq.2)then
              sig(k,k)=(-1.d0*astar(i)**ip)/eevec(i,1)
           else if(jlgeps.eq.3)then
              sig(k,k)=(-1.d0*zigeps*astar(i)**ip)/eevec(i,1)
           end if
           end do           
           end do 
        end if
      end if
      
      return
      end subroutine ee_inverse
































