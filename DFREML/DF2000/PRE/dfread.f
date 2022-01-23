c============================================================================
      subroutine dfrd33 (iun33, iofflp, ivec, n, iend, infmtp)
c============================================================================

      integer, intent(in)                :: iun33, iofflp, n
      integer, intent(out)               :: iend
      integer, dimension(5), intent(out) :: ivec
      character(len=60), intent(in)      :: infmtp
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
      if(iofflp.eq.1)then
          read(iun33,end=99)ivec(:n)
      else if(iofflp.eq.2)then
          read(iun33,*,end=99)ivec(:n)
      else if(iofflp.eq.3)then
          read(iun33,infmtp,end=99)ivec(:n)
      end if
      return
99    iend=99
      return
      end subroutine dfrd33

c============================================================================
      SUBROUTINE      DFRD34 (iun34,ioffld,iopt,ivec,iq,iend,infmtd)
c============================================================================

      use c_numbers

!     arguments
      integer, intent(in)                :: iun34,ioffld,iopt
      integer, intent(out)               :: iend,iq
      integer, dimension(5), intent(out) :: ivec
      CHARACTER(len=60), intent(in)      :: INFMTD
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C     UNIVARIATE ...
 50   iq=1
      lint1=kint(1)
      IF(IOPT.EQ.1 .AND. IOFFLD.EQ.1)THEN
         READ(IUN34,END=99)IVEC(:3),JVEC(:LINT1),XVEC(:KREAL8(1))
      ELSE IF(IOPT.EQ.1 .AND. IOFFLD.EQ.2)THEN
         READ(IUN34,*,END=99)IVEC(:3),JVEC(:LINT1),XVEC(:KREAL8(1))
      ELSE IF(IOPT.EQ.1 .AND. IOFFLD.EQ.3)THEN
         READ(IUN34,INFMTD,END=99)IVEC(:3),JVEC(:LINT1),XVEC(:KREAL8(1))

C     MULTIVARIATE (DFMUV) ...
      ELSE IF(IOPT.EQ.2 .AND. IOFFLD.EQ.1)THEN
         READ(IUN34,END=99)iq,IVEC(:3),JVEC(:LINT1),XVEC(:KREAL8(1))
      ELSE IF(IOPT.EQ.2 .AND. IOFFLD.EQ.2)THEN
         READ(IUN34,*,END=99)iq,IVEC(:3),JVEC(:LINT1),XVEC(:KREAL8(1))
      else IF(IOPT.EQ.2 .AND. IOFFLD.EQ.3)THEN
         READ(IUN34,INFMTD,END=99)iq,IVEC(:3),JVEC(:LINT1),
     &                                                XVEC(:KREAL8(1))

C     MULTIVARIATE (DFMUW) ...
      ELSE IF(IOPT.EQ.3 .AND. IOFFLD.EQ.1)THEN
         READ(IUN34,END=99)IQ,IVEC(:3),JVEC(:kINT(IQ)),XVEC(:KREAL8(IQ))
      ELSE IF(IOPT.EQ.3 .AND. IOFFLD.EQ.2)THEN
         READ(IUN34,*,END=99)IQ,IVEC(:3),JVEC(:kINT(IQ)),
     &                                                 XVEC(:KREAL8(IQ))
      ELSE IF(IOPT.EQ.3 .AND. IOFFLD.EQ.3)THEN
         READ(IUN34,INFMTD,END=99)IQ,IVEC(:3),JVEC(:kINT(IQ)),
     &                                                 XVEC(:KREAL8(IQ))

c     multivariate : covariance functions
      ELSE IF(IOPT.ge.4 .AND. IOFFLD.EQ.1)THEN
c        ... put age at the end of jvec
         READ(IUN34,END=99)IQ,IVEC(:3),JVEC(:kINT(IQ)),XVEC(:KREAL8(IQ))
     &,                        jvec(kint(iq)+1:kint(iq)+nmeta+nfxreg)
      ELSE IF(IOPT.ge.4 .AND. IOFFLD.EQ.2)THEN
         READ(IUN34,*,END=99)IQ,IVEC(:3),JVEC(:kINT(IQ)),
     *          XVEC(:KREAL8(IQ)),jvec(kint(iq)+1:kint(iq)+nmeta+nfxreg)
      ELSE IF(IOPT.ge.4 .AND. IOFFLD.EQ.3)THEN
         READ(IUN34,INFMTD,END=99)IQ,IVEC(:3),JVEC(:kINT(IQ)),
     &      XVEC(:KREAL8(IQ)),jvec(kint(iq)+1:kint(iq)+nmeta+nfxreg)
      else
         print *,'iopt=',iopt,' ioffld=',ioffld
         stop 'dfread'
      END IF

      if(iq.lt.1.or.iq.gt.nq)then
         write(*,*)'error in reading data : '
         write(*,*)'trait no. found   =',iq
         write(*,*)'permissible range = 1 to',nq
         stop
      end if

!     sire model
      if(iospec.eq.4)then
         if(ivec(2).eq.0)go to 50         ! missing sire ID
!        ... replace animal p.e. effect with sire p.e. effect
         if(iopt.ge.5.and.iopsm1>0)jvec(kfix(iq)+iopsm1)=ivec(2)  
      end if

      RETURN
99    IEND=99
      RETURN
      END subroutine dfrd34










