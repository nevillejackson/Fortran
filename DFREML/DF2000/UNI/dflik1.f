!===========================================================================
      SUBROUTINE DFLIK1 (nparm,parvec,fvalue,zige)
!===========================================================================

      use sparse
      use xsprse
      use lnk
      use likelihoods
      use spasol
      use diagonal
      use like
      use levels
      use iterates
      use constants
      use units
      use numbers
      use variance_ratios
      use parameters, only : mxfunc

      implicit double precision (a-h,o-z)

      integer, intent(in)                       :: nparm
      real(8), dimension(nparm), intent(inout)  :: parvec
      real(8), intent(inout)                    :: zige
      real(8), intent(out)                      :: fvalue
      real(8), dimension(nq)                    :: evec
      integer,save                              :: kkopt
      real(8)                                   :: sigp, detr,sigm,sigam,
     &                                             time, xsecs,esq
      integer :: ind

      if(ipvrun.eq.0)kkopt=1

!     check whether point has been evaluated before
      FVALUE=0.D0
      call lookup_logl (ind)
      if(ind.eq.1)return

      IF(NTIME.LT.3)CALL DFTIME(time,xsecs,isecs,0,'DFLIK1')


!     determine variance ratios & check validity of parameters
      FVALUE=0.D0
      call var_ratios
      if(fvalue.eq.big)return

C     SET UP DATA PART OF AUGMENTED MIXED MODEL EQUATIONS
      if(kkopt.gt.10)kkopt=kkopt-10
 100  CALL DFMMD1(NPARM,PARVEC,FVALUE)
      IF(dabs(FVALUE-BIG)<10.d-9)RETURN

C     ADD PEDIGREE PART
      CALL DFMMP1
      YRY=XHX(NTRAIT,NTRAIT)

C     CARRY OUT GAUSSIAN ELIMINATION
      CALL DFSPA1 (kkopt)
      if(kkopt.eq.2)go to 100

C     CALCULATE LOG LIKELIHOOD
      YPY=DIA(NTRAIT)
      NCALL=NCALL+1
      NDF2=NREC-nsrow
      NDF1=NDF2+NANIM+NRAND1+NRAND2
      DO IQ=1,NQ
      ZIGE=DIA(IQ)/NDF1
      EVEC(IQ)=ZIGE
      DETR=NDF2*DLOG(ZIGE)
      FVLVEC(IQ)=DETC+DETR+NDF1+2.D0*DETL

      SIGP=ZIGE/ESQ
      if(nainv>1)then
         do i=1,nainv
         fvlvec(iq)=fvlvec(iq)+NNA(i)*DLOG(PARVEC(i)*SIGP)
         end do
      else
         fvlvec(iq)=fvlvec(iq)+NANIM*DLOG(PARVEC(1)*SIGP)
      end if
      m=nainv
      IF(nrand2>0)THEN
         IF(IOPCOV.LE.2)fvlvec(iq)=fvlvec(iq)+2.d0*detl
         SIGM=PARVEC(nainv+1)*SIGP
         m=m+1
         IF(IOPCOV.EQ.2)THEN
            SIGAM=PARVEC(nainv+2)*SIGP   ! sigm given siga
            SIGM=SIGM-( (SIGAM*SIGAM) / (SIGP*PARVEC(1)) )
            m=m+1
         END IF
         IF(SIGM>ZERO)FVLVEC(IQ)=FVLVEC(IQ)+Nrand2*DLOG(SIGM)
      END IF

      do i=1,ioprn1
      if(nlev(nfix+i).eq.0)cycle
      m=m+1
      SIG=PARVEC(m)*SIGP
      IF(SIG.GT.ZERO)FVLVEC(IQ)=FVLVEC(IQ)+NLEV(NFIX+I)*DLOG(SIG)
      END do

      end do ! iq

      FVALUE=FVLVEC(NTRAIT)
      ZIGE=EVEC(NTRAIT)
      IF(FVALUE.LT.FFMIN)THEN
         FFMIN=FVALUE
         SAVSOL(1:)=RHS(NTRAIT,:nsrow)
      END IF
      IF(NTIME.LT.3)THEN
         CALL DFTIME(time,xsecs,isecs,14,'DFLIK1')
         BTIME=BTIME+XSECS
         NTIME=NTIME+1
      END IF
C     WRITE RESULT TO STANDARD OUTPUT
      XLIKE=-0.5D0*FVALUE
      WRITE(*,'(I5,F19.9,(1X,T26,4F12.6))')NCALL,xLIKE,PARVEC(:NPARM),
     &                                                           zige

C     STORE LIKELIHOOD VALUE WITH PERTAINING PARAMETERS
      n1c=ncall
      do while (n1c>mxfunc)
      n1c=n1c-mxfunc
      end do
      FEVAL(:nparm,n1c)=PARVEC(:nparm)
      FEVAL(NPARM+1,n1c)=fvalue
      FEVAL(NPARM+2,n1c)=EVEC(NTRAIT)

C     SAVE ALL LIKELIHOODS FOR RETRIEVAL FILE "59"
      CLOSE(IUN59)
      OPEN(IUN59,FILE='DF59#DAT',POSITION='APPEND',FORM='UNFORMATTED',
     &                                               status='unknown')
      WRITE(IUN59)NCALL,PARVEC(:NPARM),FVLVEC(:NQ),EVEC(:NQ)
      CLOSE(IUN59)
      ipvrun=1
      RETURN
   
      contains

!     ---------------------
      subroutine var_ratios
!     ---------------------

      real(8)                             :: hsq,xmsq,csq,cam,xx,zer100
      integer                             :: m

      zer100=zero*100.d0

!     parameters are heritabilities, etc.
      ESQ=1.D0-parvec(1)-sum(parvec(nainv+1:nparm))
      IF(ESQ<ZER100)GO TO 9000

      do i=1,nainv
      HSQ=PARVEC(i)
      IF(HSQ<ZER100.OR.HSQ>ONE)GO TO 9000
      XLAMB(i)=ESQ/HSQ
      end do
      m=nainv

      IF(nrand2>0)THEN
        XMSQ=PARVEC(nainv+1)
        if(nrand2>0.and.(XMSQ<ZER100.OR.XMSQ>ONE))GO TO 9000
        IF(IOPCOV.NE.2)THEN
           XKAPPA=ESQ/XMSQ
           m=m+1
        ELSE  !                           only allowed for nainv=1 !!
           CAM=PARVEC(nainv+2)
           IF((CAM*CAM/(parvec(1)*XMSQ))>one)GO TO 9000
           XX=ESQ/(parvec(1)*XMSQ-CAM*CAM)
           XLAMB(1)=XX*XMSQ
           XKAPPA=XX*parvec(1)
           ALPHA=(-CAM)*XX
           m=m+2
         END IF
      END IF

      do ii=1,ioprn1
      if(nlev(nfix+ii).eq.0)cycle
      m=m+1
      CSQ=PARVEC(m)
      IF(CSQ<ZER100.OR.CSQ>ONE)GO TO 9000
      GAMMA(ii)=ESQ/CSQ
      END DO
      return

!     INVALID PARAMETER VECTOR ENCOUNTERED
9000  FVALUE=BIG
      RETURN

      end subroutine var_ratios

!     ============================
      subroutine lookup_logl (ind)
!     ============================

      integer, intent(out) :: ind

      ind=0
      DO 400 I=MIN0(NCALL,MXFUNC),1,-1
      DO J=1,NPARM
      IF( DABS(PARVEC(J)-FEVAL(J,I)).GT.ZERO )GO TO 400
      end do
      FVALUE=FEVAL(NPARM+1,I)
      ZIGE=FEVAL(NPARM+2,I)
      XLIKE=-0.5D0*FVALUE
      WRITE(*,'(I5,F19.9,(1X,T26,4F12.6))')-I,XLIKE,PARVEC(:NPARM)
      ind=1
      RETURN
 400  CONTINUE
      end subroutine lookup_logl

      END subroutine dflik1







