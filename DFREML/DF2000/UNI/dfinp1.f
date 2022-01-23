!===========================================================================
      SUBROUTINE DFINP1 (ioprun,nparm,kparm)
!===========================================================================

      use names, only : param
      use parameters
      use units
      use constants
      use numbers
      use levels
      use platform
      use means, only : ysdev
      use variance_ratios

      integer, intent(in)  :: ioprun
      integer, intent(out) :: kparm,nparm
      real(8)              :: hsq,csq,xmsq,cam,onem,zige,sigp
      integer              :: iainv, mparm

      mparm=nainv+ioprn1+ioprn2+1                 ! max. no. of parameters
      if(iopcov.eq.2)mparm=mparm+1
      allocate(xvec(mparm),param(mparm),stat=ii)
      if(ii>0)stop 'alloc xvec'
      xvec=0.d0

      nparm=0
      do iainv=1,nainv
      IF(IOPRUN.eq.0.or.ioprun.eq.6)THEN
         WRITE(*,FMT(ipltf))'STARTING VALUE FOR HERITABILITY ?'
         CALL RVALUE(HSQ,0.d0,ONE)
      ELSE
         HSQ=0.2D0
      END IF
      NPARM=nparm+1
      XVEC(nparm)=HSQ
      PARAM(nparm)='HERITABILITY'
      end do    ! iainv

      IF(nfix2>nfix1.and.nlev(nfix2)>0)THEN
         IF(IOPRUN.eq.0. or. ioprun.eq.6)THEN
            WRITE(*,FMT(ipltf))'STARTING VALUE FOR "M-SQUARED" ?'
            CALL RVALUE(XMSQ,0.d0,ONE)
         ELSE
            XMSQ=0.1D0
         END IF
         NPARM=NPARM+1
         PARAM(NPARM)='"M-SQUARED" '
         XVEC(NPARM)=XMSQ
         IF(IOPCOV.EQ.2)THEN
            ONEM=-ONE
            IF(IOPRUN.eq.0 .or. ioprun.eq.6)THEN
            WRITE(*,*)'STARTING VALUE FOR COVARIANCE BETWEEN '
            WRITE(*,*)'ADDITIV-GENETIC & 2ND ANIMAL EFFECT ? '
            WRITE(*,FMT(ipltf))'(AS PROPORION OF PHENOTYPIC VARIANCE !)'
               CALL RVALUE(CAM,ONEM,ONE)
            ELSE
               CAM=0.05D0
            END IF
            NPARM=NPARM+1
            PARAM(NPARM)='COV(AM)/SIGP'
            XVEC(NPARM)=CAM
         END IF
      END IF
      IF(IOPRN1.gt.0 )THEN
         do i=1,ioprn1
         if(nlev(nfix+i).eq.0)cycle
         IF(IOPRUN.eq.0 .or. ioprun.eq.6 )THEN
            WRITE(*,fmt3(ipltf))'STARTING VALUE FOR "C-SQUARED" ?',i
            CALL RVALUE(CSQ,0.d0,ONE)
         ELSE
            CSQ=0.12D0/dble(ioprn1)
         END IF
         NPARM=NPARM+1
         XVEC(NPARM)=CSQ
         PARAM(NPARM)='"C-SQUARED" '
         end do
      END IF

      NTRAIT=1
      IF(NQ.GT.1.AND.IOPRUN.GE.0.and.ioprun.le.8)THEN
         WRITE(*,fmt(ipltf)) 'TRAIT TO BE ANALYZED ? (GIVE RUNNING NO.)'
         CALL OPTION(NTRAIT,1,NQ)
      END IF

      allocate(start(nparm),ipm(nparm),stat=ii)
      if(ii>0)stop 'alloc ipm'
      ipm=0
      do i=1,nparm
      if(xvec(i).eq.0.d0)ipm(i)=1
      end do
      KPARM=NPARM-sum(ipm)

      RETURN
      END subroutine dfinp1

C==========================================================================
      SUBROUTINE DFIPM1 (nparm,kparm)
C==========================================================================

      use names, only : param
      use parameters
      use constants
      use platform
      use numbers

      integer, intent(in)  :: nparm
      integer, intent(out) :: kparm
      real(8)              :: xx,yy

      IF(NPARM.EQ.1)RETURN

      WRITE(*,*)'NO. OF PARAMETERS IN TOTAL    =',NPARM
      WRITE(*,*)' '
      WRITE(*,*)'NO. OF PARAMETERS TO BE FIXED ?'
      M=NPARM-1
      CALL OPTDEF(NC,0,M,1)
      WRITE(*,*)' '

      DO I=1,NC
      WRITE(*,fmt(ipltf))'RUNNING NO. OF PARAMETER TO BE FIXED ?'
      CALL OPTION(N,1,NPARM)
      WRITE(*,*)'PARAMETER TO BE FIXED =  ',PARAM(N)
      WRITE(*,'(a,g16.8)')'CURRENT VALUE =',XVEC(N)
      WRITE(*,*)'GIVE OPTION :'
      WRITE(*,*)'        0  ...  FIX AT CURRENT VALUE '
      WRITE(*,*)'        1  ...  READ IN NEW VALUE    '
      WRITE(*,*)'        2  ...  ADD CONSTANT TO CURRENT VALUE '
      CALL OPTdef(IIFIX,0,2,0)
      IF(IIFIX.EQ.1)THEN
         WRITE(*,FMT4(ipltf))'NEW VALUE FOR ',PARAM(N),'  ?'
         YY=XVEC(N)
         CALL RPARAM (XX)
         XVEC(N)=XX
      ELSE IF(IIFIX.EQ.2)THEN
         WRITE(*,*)'CURRENT VALUE FOR  ',PARAM(N),'   =',XVEC(N)
         WRITE(*,*)'CONSTANT TO BE ADDED ?'
         CALL RVLDEF(XX,-1.D0,1.D0,0.001D0)
         XVEC(N)=XVEC(N)+XX
      END IF
      WRITE(*,'(I3,2X,A,2X,''FIXED AT ='',G16.8)')N,PARAM(N),XVEC(N)
      IPM(N)=1
      end do
      KPARM=NPARM-NC
      RETURN

      contains

!     ======================
      SUBROUTINE RPARAM (XX)
!     ======================

      real(8), intent(out) :: xx
      REAL(8)              :: x1,x2

      IF(IOPCOV.EQ.2.AND.N.EQ.3)THEN
         X1=-1.D0
         X2=-ONE
      ELSE
         X1=0.D0
         X2=ZERO
      END IF
      IF(YY.NE.0.D0)THEN
         CALL RVLDEF(XX,X1,1.D0,YY)
      ELSE
         CALL RVALUE(XX,X1,1.D0)
      END IF
      IF(XX.LT.X2)THEN
         XX=X2
      ELSE IF(XX.GT.ONE)THEN
         XX=ONE
      END IF
      RETURN
      END subroutine rparam

      END subroutine dfipm1


















