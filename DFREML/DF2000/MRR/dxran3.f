!===========================================================================
      SUBROUTINE DxRAN3 (parvec,fvalue,iout,iopt)
!===========================================================================

      use parameters, only : mxparm
      use names
      use units
      use phimatrix
      use parmap
      use sigmas
      use numbers
      use traces
      use dmatrices
      use like_components, only : deta, detm,detam,detc,detq

!     arguments
      real(8), dimension (mxparm), intent(in) :: parvec
      real(8), intent(out)                    :: fvalue
      integer, intent(in)                     :: iopt
      integer, intent(out)                    :: iout

!     local variables
      CHARACTER(len=8)                        :: PP1
      integer                                 :: i,j, ipar
      real(8), dimension(:), allocatable      :: work
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate(work(nqq2),stat=ii)
      if(ii>0)stop 'dxran3 : alloc'

      nq20=mfitmx+mfitmx
      iout=0

C     ---------------------------------------------------------
C     CONVERT PARAMETERS TO VARIANCE MATRICES & OBTAIN INVERSES
C     ---------------------------------------------------------

C     ADDITIV GENETIC VARIANCE ("DIRECT")
      siga=0.d0
      if(nanim>0)then
         CALL DXCOVM(NPARM,0,ksfit(1),IOUT,2,mfitmx,SIGA,PARVEC,WORK,
     *                                                  FVALUE,DETA)
         if(iout.eq.1)print *,'sig a not p.d'
         IF(IOUT.EQ.1)go to 99
      end if

C     VARIANCE DUE TO ADDITIONAL, UNCORRELATED EFFECT
      IF(IOPRN1.EQ.1 .and. ieqmod.eq.0 )THEN
         CALL DXCOVM(NPARM,ISTRT(3),ksfit(4),IOUT,1,mfitmx,SIGC,
     *                                    PARVEC,WORK,FVALUE,DETC)
         IF(IOUT.EQ.1)then
            print *,'sig C not p.d !'
            go to 99
         end if
         do i=1,ksfit(4)
         if(sigc(i,i).eq.0)then
            print *,'sigc inv',i,'  dia',sigc(i,i)
            iout=1
            fvalue=big
            go to 99
         end if
         end do
      END IF

      IF(IOPRN3.EQ.1)THEN
         CALL DXCOVM(NPARM,ISTRT(5),ksfit(5),IOUT,1,mfitmx,SIGQ,
     *                                    PARVEC,WORK,FVALUE,DETQ)
         IF(IOUT.EQ.1)then
            print *,'sig Q not p.d !'
         do i=1,ksfit(5)
         print *,i,sigq(i,:ksfit(5))
         end do
            go to 99
         end if
         do i=1,ksfit(5)
         if(sigq(i,i).eq.0)then
            print *,'sigq inv',i,'  dia',sigq(i,i)
            iout=1
            fvalue=big
            go to 99
         end if
         end do
      END IF

C     VARIANCE DUE TO SECOND ANIMAL EFFECT ("MATERNAL")
      IF(IOPRN2.EQ.1)THEN
         IF(IOPCOV.LE.2)THEN
            NQ2=ksfit(1)+ksfit(2)
            JPICK=2
         ELSE
            NQ2=ksfit(1)
            JPICK=1
         END IF
         CALL DXCOVM(NPARM,ISTRT(1),ksfit(2),IOUT,jpick,mfitmx,SIGM,
     *                                     PARVEC,WORK,FVALUE,DETM)

         IF(IOUT.EQ.1)go to 99
      else
         jpick=0
         nq2=ksfit(1)
      END IF

      if(nanim>0)then
         work=0.d0
         DO I=1,ksfit(1)
         DO J=I,ksfit(1)
         WORK(IHMSSF(I,J,NQ2))=SIGA(J,I)
         end do
         end do
         if(jpick.eq.2)then
            DO I=1,ksfit(2)
            DO J=I,ksfit(2)
            WORK(IHMSSF(ksfit(1)+I,ksfit(1)+J,NQ2))=SIGM(J,I)
            end do
            end do
         end if
         IF(IOPCOV.EQ.2)THEN          ! pick out direct-maternal covariances
           IBEGIN=ISTRT(2)
           DO  I=IBEGIN+1,MIN0(IBEGIN+NQSQ,NPARM)
           READ(parvar(i),'(a8,2i3)')PP1,I1,J1
           IF(PP1.NE.'SIG AM  ')GO TO 46
           WORK(IHMSSF(I1,ksfit(1)+J1,NQ2))=PARVEC(I)
           end do
         END IF
 46      continue

         CALL DXCOVM(NPARM,0,NQ2,IOUT,0,nq20,SIGAM,PARVEC,WORK,FVALUE,
     &            DETAM)

         IF(IOUT.EQ.1.and.iopt.ne.0) stop '"DXRAN3" : sigam'
         IF(IOUT.EQ.1.and.iopt.eq.0)then
            fvalue=big
            print *,'sigam',iout,fvalue
            go to 99
         end if
         do i=1,nq2
         if(sigam(i,i).eq.0)then
            print *,'siga inv',i,'  dia',sigam(i,i)
            iout=1
            fvalue=big
            go to 99
         end if
         end do
      end if
     
      if(iopt.eq.0)go to 100 ! log L only

c     initialise ...
      tdt=0.d0
      td=0.d0
      d=.false.

c     set up d-matrices
      if(nanim>0)then
         do  i=1,ksfit(1)
         do  j=i,ksfit(1)
         ipar=nparno(j,i,1)
         if(ipar.gt.0)d(ihmssf(i,j,nq2),ipar)=.true.
         end do
         end do

         if(ioprn2.eq.1)then
            do  i=1,ksfit(2)
            do  j=i,ksfit(2)
            ipar=nparno(j,i,2)
            if(ipar>0)d(ihmssf(ksfit(1)+i,ksfit(1)+j,nq2),ipar)=.true.
            end do
            end do
            if(iopcov.eq.2)then
               do  i=1,ksfit(1)
               do  j=1,ksfit(2)
               ipar=nparno(i,j,3)
               if(ipar.gt.0)d(ihmssf(i,ksfit(1)+j,nq2),ipar)=.true.
               end do
               end do
            end if 
         end if 
      end if 

      if(ioprn1.eq.1 .and.ieqmod.eq.0)then
         do i=1,ksfit(4)
         do j=i,ksfit(4)
         ipar=nparno(j,i,4)
         if(ipar.gt.0)d(ihmssf(i,j,ksfit(4)),ipar)=.true.
         end do
         end do
      end if 

      if(ioprn3.eq.1)then
         do  i=1,ksfit(5)
         do j=i,ksfit(5)
         ipar=nparno(j,i,5)
         if(ipar.gt.0)d(ihmssf(i,j,ksfit(5)),ipar)=.true.
         end do
         end do
      end if 

c     calculations for animal effects ...
      if(nanim>0)then
         do  ipar=1,ngpar1
c        set up matrices T(-1)D(ij) 
         do  j=1,nq2
         do  k=1,nq2
         if( d(ihmssf(k,j,nq2),ipar))td(1:nq2,j,ipar)=td(1:nq2,j,ipar)
     *                                                 +sigam(1:nq2,k)
         end do
         end do

c        calculate traces t(-1)d(ij)
         trran1(ipar)=sum( (/ (td(i,i,ipar),i=1,nq2) /) )

c        set up matrices T(-1)D(ij)T(-1)
!         tdt(ipar,:nq2,:nq2)=matmul(td(:nq2,:nq2,ipar),sigam(:nq2,:nq2) )
         do i=1,nq2                      ! old-fashioned loops run quicker
         do k=1,nq2                      ! than intrinsic routine !
         if(td(i,k,ipar).ne.0.d0)then
            do j=1,nq2
            tdt(ipar,i,j)=tdt(ipar,i,j)+td(i,k,ipar)*sigam(k,j)
            end do
         end if
         end do
         end do
         end do ! end of loop  for ipar
      end if

c     same calculations for additional random effect

      if(ioprn1.eq.1 .and. ieqmod.eq.0)then
         do ipar=ngpar1+1,ngpar2
         do j=1,ksfit(4)
         do k=1,ksfit(4)
         if( d(ihmssf(k,j,ksfit(4)),ipar) ) td(:ksfit(4),j,ipar)=
     *                      td(:ksfit(4),j,ipar) +sigc(:ksfit(4),k)
         end do
         end do

         trran1(ipar)=sum( (/ (td(i,i,ipar),i=1,ksfit(4)) /) )

         do i=1,ksfit(4)
         do k=1,ksfit(4)
         if(td(i,k,ipar).ne.0.d0)then
            do j=1,ksfit(4)
            tdt(ipar,i,j)=tdt(ipar,i,j)+td(i,k,ipar)*sigc(k,j)
            end do
         end if
         end do
         end do

         end do
      end if

      if(ioprn3.eq.1)then
         do  ipar=ngpar2+1,ngparm
         do j=1,ksfit(5)
         do k=1,ksfit(5)
         if( d(ihmssf(k,j,ksfit(5)),ipar) ) td(:ksfit(5),j,ipar)=
     *                      td(:ksfit(5),j,ipar) +sigq(:ksfit(5),k)
         end do
         end do

         trran1(ipar)=sum( (/ (td(i,i,ipar),i=1,ksfit(5)) /) )
         do i=1,ksfit(5)
         do k=1,ksfit(5)
         if(td(i,k,ipar).ne.0.d0)then
            do j=1,ksfit(5)
            tdt(ipar,i,j)=tdt(ipar,i,j)+td(i,k,ipar)*sigq(k,j)
            end do
         end if
         end do
         end do

         end do ! ipar
      end if

 100  continue

99    deallocate(work,stat=ii)
      if(ii>0)stop 'dxran3 : de-alloc'
      return
      end subroutine dxran3



 















