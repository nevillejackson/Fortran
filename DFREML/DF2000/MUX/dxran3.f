c===========================================================================
      SUBROUTINE      DxRAN3 (parvec,fvalue,nparm,iout,iopt)
C===========================================================================

      use params
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
      integer, intent(in)                     :: nparm, iopt
      integer, intent(out)                    :: iout

!     local variables
      CHARACTER(len=8)                        :: PP1
      CHARACTER(len=12)                       :: PARA
      integer                                 :: i,j, ipar,nq20
      real(8), dimension(:), allocatable      :: work
      logical, dimension(:,:), allocatable    :: d
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate(work(nqq2),d(nqq2,nparm),stat=ii)
      if(ii>0)stop 'dxran3 : alloc'

      nq2=nq
      nq20=nq+nq

C     ---------------------------------------------------------
C     CONVERT PARAMETERS TO VARIANCE MATRICES & OBTAIN INVERSES
C     ---------------------------------------------------------

C     ADDITIV GENETIC VARIANCE ("DIRECT")
      CALL DXCOVM(NPARM,0,NQ,NQQ,IOUT,2,nq,SIGA,PARVEC,WORK,FVALUE,DETA)
      IF(IOUT.EQ.1)return

C     VARIANCE DUE TO ADDITIONAL, UNCORRELATED EFFECT
      IF(IOPRN1.EQ.1)THEN
         CALL DXCOVM(NPARM,ISTRT(3),NQ,NQQ,IOUT,1,nq,SIGC,PARVEC,WORK,
     *                                                      FVALUE,DETC)
         IF(IOUT.EQ.1)return
         if(ioprn3.eq.1)then
            CALL DXCOVM(NPARM,ISTRT(7),NQ,NQQ,IOUT,1,nq,SIGQ,PARVEC,
     *                                                WORK,FVALUE,DETQ)
            IF(IOUT.EQ.1)return
         END IF
      END IF

C     VARIANCE DUE TO SECOND ANIMAL EFFECT ("MATERNAL")
      IF(IOPRN2.EQ.1)THEN
         IF(IOPCOV.LE.2)THEN
            NQ2=NQ+NQ
            JPICK=2
         ELSE
            NQ2=NQ
            JPICK=1
         END IF
         CALL DXCOVM(NPARM,ISTRT(1),NQ,NQQ,IOUT,jpick,nq,SIGM,PARVEC,
     *                                              WORK,FVALUE,DETM)
         IF(IOUT.EQ.1)return
      else
         jpick=0
      END IF

      work=0.d0
      nqq22=nq2*(nq2+1)/2
      DO I=1,NQ
      DO J=I,NQ
      IF(JPICK.EQ.2)WORK(IHMSSF(NQ+I,NQ+J,NQ2))=SIGM(J,I)
      WORK(IHMSSF(I,J,NQ2))=SIGA(J,I)
      end do
      end do
      IF(IOPCOV.EQ.2)THEN
         IBEGIN=ISTRT(2)
 20      FORMAT(A8,2I2)
         DO I=IBEGIN+1,MIN0(IBEGIN+NQSQ,NPARM)
         PARA=PARvar(I)
         READ(PARA,20)PP1,I1,J1
         IF(PP1.NE.'SIG AM  ')GO TO 46
         WORK(IHMSSF(I1,NQ+J1,NQ2))=PARVEC(I)
         end do
      END IF
 46   CALL DXCOVM(NPARM,0,NQ2,NQQ22,IOUT,0,nq20,SIGAM,PARVEC,WORK,
     *                                                FVALUE,DETAM)
      IF(IOUT.EQ.1.and.iopt.ne.0) stop '"DXRAN3" : sigam'
      IF(IOUT.EQ.1.and.iopt.eq.0)then
        fvalue=big
        return
      end if
     
      if(iopt.eq.0)go to 100 ! log L only

c     initialise ...
      tdt=0.d0
      td=0.d0
      d=.false.

c     set up d-matrices 
      do 2 i=1,nq
      do 2 j=i,nq
      ipar=nparno(j,i,1)
      if(ipar.gt.0)d(ihmssf(i,j,nq2),ipar)=.true.
 2    continue
      if(ioprn2.eq.1)then
         do i=1,nq
         do j=i,nq
         ipar=nparno(j,i,2)
         if(ipar.gt.0)d(ihmssf(nq+i,nq+j,nq2),ipar)=.true.
         end do
         end do
         if(iopcov.eq.2)then
            do i=1,nq
            do j=1,nq
            ipar=nparno(i,j,3)
            if(ipar>0)d(ihmssf(i,nq+j,nq2),ipar)=.true.
            end do
            end do
         end if 
      end if 

      if(ioprn1.eq.1)then
         do i=1,nq
         do j=i,nq
         ipar=nparno(j,i,4)
         if(ipar.gt.0)d(ihmssf(i,j,nq),ipar)=.true.
         end do
         end do
         if(ioprn3.eq.1)then
            do i=1,nq
            do j=i,nq
            ipar=nparno(j,i,7)
            if(ipar.gt.0)d(ihmssf(i,j,nq),ipar)=.true.
            end do
            end do
         end if 
      end if 

c     calculations for animal effects ...
      do 5 ipar=1,ngpar1

c     set up matrices T(-1)D(ij) 
      do j=1,nq2
      do k=1,nq2
      if( d(ihmssf(k,j,nq2),ipar) ) td(1:nq2,j,ipar)=td(1:nq2,j,ipar)
     *                                                +sigam(1:nq2,k)
      end do
      end do

c     calculate traces t(-1)d(ij)
      trran1(ipar)=sum( (/ (td(i,i,ipar),i=1,nq2) /) )

c     set up matrices T(-1)D(ij)T(-1)
      tdt(ipar,:nq2,:nq2)=matmul(td(:nq2,:nq2,ipar),sigam(:nq2,:nq2) )

 5    continue ! end of loop  for ipar

c     same calculations for additional random effect
      if(ioprn1.eq.0)go to 100

c     set up matrices T(-1)D(ij) 
      do  ipar=ngpar1+1,ngpar2
      do j=1,nq
      do k=1,nq
      if( d(ihmssf(k,j,nq),ipar) ) td(:nq,j,ipar)=td(:nq,j,ipar)
     *                                              +sigc(:nq,k)
      end do
      end do

c     calculate traces t(-1)d(ij)
      trran1(ipar)=sum( (/ (td(i,i,ipar),i=1,nq) /) )

c     set up matrices T(-1)D(ij)T(-1)
      tdt(ipar,:nq,:nq)=matmul(td(:nq,:nq,ipar),sigc(:nq,:nq) )

      end do
      if(ioprn3.eq.0)go to 100

      do  ipar=ngpar2+1,ngparm
      do j=1,nq
      do k=1,nq
      if( d(ihmssf(k,j,nq),ipar) ) td(:nq,j,ipar)=td(:nq,j,ipar)
     *                                              +sigq(:nq,k)
      end do
      end do
      trran1(ipar)=sum( (/ (td(i,i,ipar),i=1,nq) /) )
      tdt(ipar,:nq,:nq)=matmul(td(:nq,:nq,ipar),sigq(:nq,:nq) )
      end do

 100  continue

      deallocate(work,d,stat=ii)
      if(ii>0)stop 'dxran3 : de-alloc'
      return
      end subroutine dxran3



 















