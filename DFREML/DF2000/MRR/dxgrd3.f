c===========================================================================
      SUBROUTINE      DXgrd3 (zero)
c===========================================================================

c     purpose :       write out grid for plotting
c-------------------------------------------------------------km--3/96------

      use parameters
      use legendre
      use ages
      use numbers
      use names, only : trait
      use phimatrix, only : irropt
      use units, only : iun15, iun66

!     arguments
      real(8), intent(in)                     :: zero
!     local variables
      real(8)                                 :: aamin,aamax,a,xupp,xlow
     &,                                          step,aa,arange,xx
      real(8),dimension (:),allocatable       :: aage,xkk,a1,a2
      real(8),dimension (:,:),allocatable     :: vv
      character(len=25)                       :: UNKN,FORMA,FSTAND
      integer                                 :: n,i,iq,jq,mq
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate(a1(kfitmx),a2(kfitmx),vv(kfitmx,kfitmx),xkk(kfitmx*
     &                                       (kfitmx+1)/2),stat=ii)
      if(ii>0)stop 'dxgrid : alloc'

      UNKN='UNKNOWN'
      FORMA='FORMATTED'
      FSTAND='DF15#DAT'
      CALL FCONCT(IUN15,FSTAND,FORMA,UNKN)

      n=nage(1)
      write(*,*)'age range in data =',iiage(1,1),' to',iiage(n,1)
      write(*,*)'generate grid for ? '
      write(*,9)' 1  ...  ages in the data '
      write(*,9)' 2  ...  specified range & step size (interpolate) '
      call optdef(ii,1,2,1)

      if(ii.eq.1)then
         mq=nage(1)
         allocate(aage(mq),stat=ii)
         if(ii>0)stop 'dxgrd3 : alloc'
         aage=iiage(:,1)
         aamin=iiage(1,1)
         aamax=iiage(nage(1),1)

      else

 11      write(*,*)'minimum age ?'
         call rvalue(aamin,0.d0,1.d12)
         if(aamin.lt.iiage(1,1))then
             write(*,*)'warning : extrapolation outside data range  !'
             write(*,*)'first age in data is :',iiage(1,1)
             go to 11
         end if
20       write(*,*)'maximum age ?'
         call rvalue(aamax,aamin,1.d12)
         if(aamax.gt.iiage(nage(1),1))then
             write(*,*)'warning : extrapolation outside data range  !'
             write(*,*)'last age in data is :',iiage(nage(1),1)
             go to 20
         end if
         write(*,*)'step size ?'
         arange=aamax-aamin
         call rvalue(step,zero,arange)
         mq= int( arange/step) +1
         print *,'step =',step,arange/step
         a=aamin+(mq-1)*step
         print *,'no. of steps =',mq,a,aamax
         if(dabs(a-aamax).gt.1.d-5)then
           write(*,*)'step size and upper age limit incompatible'
           write(*,*)'upper limit set to =',a
           aamax=a
         end if
         allocate(aage(mq),stat=ii)
         if(ii>0)stop 'dxgrd3 : alloc'
         do i=1,mq
         aage(i)=aamin+(i-1)*step
         end do
      end if
         
c     set up vector of standardised ages
      xlow=-1.d0
      xupp=1.d0
      aa=(xupp-xlow)/(aamax-aamin)
      astar(:mq)=xlow+aa*(aage-aamin)
      write(*,*)'which covariance function ?'
      write(*,9)' 1  ...  additive-genetic '
      if(ioprn2.eq.1)write(*,9)' 2  ...  maternal-genetic '
      if(ioprn1.eq.1)write(*,9)' 4  ...  add. random effect (perm. en.)'
      if(ioprn3.eq.1)write(*,9)' 5  ...  2nd add. random effect '
c      write(*,9)' 7  ...  measurement error (temporary)'
      call option(iparm,1,6)

c     pick out k-matrix (all traits)
      ibegin=0
      do i=1,iparm-1
      ibegin=ibegin+ksfit(i)*(ksfit(i)+1)/2
      end do
      kk=ksfit(iparm)
      xkk(:kk*(kk+1)/2)=xvec(ibegin+1:ibegin+kk*(kk+1)/2)

      do iq=1,nq
      if(irropt(iparm,iq).ne.1)cycle
      do jq=iq,nq
      if(irropt(iparm,jq).ne.1)cycle
      if(iq.eq.jq)then
         write(iun66,*)'Covariance function for ',trait(iq)
      else
         write(iun66,*)'Cross-covariance function for ',trait(iq),
     &                 ' and ',trait(jq)
      end if

c     covariance function
      vv=0.d0
      do ifit=1,kfit(iparm,iq)
      ifit1=sum(kfit(iparm,:iq-1))+ifit
      do jfit=1,kfit(iparm,jq)
      jfit1=sum(kfit(iparm,:jq-1))+jfit
      xx=xkk(ihmssf(ifit1,jfit1,kk))
      do i=1,kfit(iparm,iq)
      do j=1,kfit(iparm,jq)
      vv(i,j)=vv(i,j)+clgndr(i,ifit)*clgndr(j,jfit)*xx
      end do
      end do
      end do ! jfit
      end do ! ifit

      write(iun66,*)' '
      do i=1,kfit(iparm,iq)
      write(iun66,902)i-1,vv(i,:kfit(iparm,jq))
      end do
      
c     calculate "generated" covariance matrix
      a1(1)=1.d0
      a2(1)=1.d0
      do i=1,mq
      a1(2:kfit(iparm,iq))=(/ (astar(i)**(m-1),m=2,kfit(iparm,iq))/)
      do j=1,mq
      a2(2:kfit(iparm,jq))=(/ (astar(j)**(m-1),m=2,kfit(iparm,jq))/)
      xx=0.d0
      do k=1,kfit(iparm,iq)
      do m=1,kfit(iparm,jq)
      xx=xx+a1(k)*a2(m)*vv(k,m)
      end do
      end do
      if(j<mq)then
      write(iun15,'(2i4,2f8.1,f20.4,g20.8)')i,j,aage(i),aage(j),xx,xx
      else
      write(iun15,'(2i4,2f8.1,f20.4,g20.8/)')i,j,aage(i),aage(j),xx,xx
      end if
      end do
      end do
      write(iun15,'(/)')

      end do !jq
      end do ! iq
      
      write(*,*)'Output file is "DF15#DAT" '
    
      stop 'end of "DXGRID" '
9     FORMAT(8X,A)
902   format(i4,(t6,8g13.6))
      end subroutine dxgrd3
















