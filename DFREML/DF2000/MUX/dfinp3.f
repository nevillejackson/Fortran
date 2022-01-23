C============================================================================
      SUBROUTINE DFINP3 (iinput,nparm,ioprun,xvec)
C============================================================================

      use params 
      use names
      use units
      use parmap
      use means
      use combinations
      use numbers
      use levels
      use fixcor
      use current
      USE platform

!     arguments
      integer, intent(inout)                  :: ioprun
      integer, intent(out)                    :: nparm,iinput
      real(8), dimension(mxparm), intent(out) :: xvec

!     local variables
      real(8), dimension(:), allocatable      :: sig
      character (len=25)                      :: FSTAND,OLD='OLD',
     &                                           FORMA='FORMATTED'
      CHARACTER(len=8)                        :: PP
      CHARACTER(len=12)                       :: PARA
      integer                                 :: iq,jq,iopsav,isilen,ii
     & ,                                         jj
      real(8)                                 :: xx
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate (nparno(nq,nq,8),sig(nq*(2*nq+1)),stat=ii)
      if(ii>0)stop 'dfinp3 : alloc'
      allocate (ipm(mxparm),ffvec(mxparm),stat=ii)
      if(ii>0)stop 'dfinp3 : alloc'
    
      JPARAM=1
      NPARM=0
      IOPSAV=IOPRUN
      kfit=0

      IF(IOPRUN.LE.-2.OR.(IOPRUN>4.and.ioprun.lt.10))THEN
         ISILEN=1
         IF(IOPRUN.EQ.8)THEN
            WRITE(*,*)'FULL SET OF PARAMETERS FITTED ?'
            CALL YNDEF(JJ,1)
            IF(JJ.EQ.0)IOPRUN=18
         END IF

      ELSE 
         ISILEN=0
         if(iopmod.eq.4)then
            write(*,*)'Parameterisation : Estimate ... ? '
            WRITE(*,9)'1  ... Covariance components '
            WRITE(*,9)'2  ... Covariance functions  '
            call optdef(jparam,1,2,1)
         end if
         if(ioprun.eq.10)then
            iinput=4
         else if(ioprun.eq.1)then
            iinput=5
         else
            WRITE(*,*)' '
            WRITE(*,*)'Read starting values for ... from ... ? '
            WRITE(*,9)'1  ... Covariance components / standard input '
            WRITE(*,9)'2  ... Covariance components / "DF18#DAT"     '
            if(jparam.eq.2)then
               WRITE(*,9)'3  ... Covariance functions  / standard input'
               WRITE(*,9)'4  ... Covariance functions  / "DF16#DAT"    '
            end if
            WRITE(*,fmt9(ipltf))
     *                  '5  ... Covariance components / "DF59#DAT"     '
            CALL OPTION(IINPUT,1,5)
9           FORMAT(7X,A)
         end if

         if(iinput.eq.1.or.iinput.eq.3)then
            iun=0
         else 
            if(iinput.eq.2)then
               IUN=18
               FSTAND='DF18#DAT'
            else if(iinput.eq.4)then
               IUN=16
               FSTAND='DF16#DAT'
            else
               iun=0
            end if
            if(iun>0)CALL FCONCT(IUN,FSTAND,FORMA,OLD)
         END IF
         if(iinput.gt.2)then
            ioprun=-2
            isilen=1
         end if
      END IF
      NOSVEC(22)=JPARAM

c     zero out array
      NPARNO=0

C     DIRECT ADDITIVE GENETIC COVARIANCES
      K=0
      DO Iq=1,NQ
      DO Jq=Iq,NQ
      CALL RSTVAL('SIG A   ','KMAT A  ',K,XX,1)
      END DO
      END DO
      CALL CHKSIG('SIG A   ',ZERO,XVEC,SIG,NPARM,NQ,ioprun)

      NPARM=NPARM+K
      IF(jparam.eq.2 .and. (IOPRUN.NE.-2.or.iopsav.ne.ioprun) )THEN
         WRITE(*,*)'ORDER OF POLYNOMIAL FIT FOR "SIG A"  ?'
         CALL OPTDEF(KFIT(1),1,NQ,NQ)
      ELSE
         KFIT(1)=NQ
      END IF

C     MATERNAL GENETIC COVARIANCE COMPONENTS
      IF( NOSVEC(15) .EQ.1 )THEN
         IF(ISILEN.EQ.0)WRITE(*,*)' '
         ISTRT(1)=NPARM
         K=0
         sig=0.d0
         DO Iq=1,NQ
         IF( NFIX2(Iq) .GT. NFIX3(Iq) )THEN
            DO Jq=Iq,NQ
            IF( NFIX2(Jq).GT.NFIX3(Jq))CALL RSTVAL('SIG M   ',
     *                                             'KMAT M  ',K,XX,2)
            END DO
         ELSE IF(ISILEN.EQ.0)THEN
            WRITE(*,*)'2ND ANIMAL EFFECT NOT FITTED FOR TRAIT NO.',Iq
         END IF
         END DO
         CALL CHKSIG('SIG M   ',ZERO,XVEC,SIG,NPARM,NQ,ioprun)

C        ... DIRECT-MATERNAL GENETIC COVARIANCES
         if(nosvec(16).eq.2.and.(ioprun.ge.0.or.iinput.eq.5) )then
            write(*,*)'Fit covariances between animal effects ?'
            call yndef(ii,0)
            if(ii.eq.0)then
              iopcov=1
              nosvec(16)=iopcov
              imodel=imodel-1
              nosvec(17)=imodel
            end if
         end if
         IF( NOSVEC(16) .EQ.2)THEN
            IF(ISILEN.EQ.0)WRITE(*,*)' '
            sig=0.d0
            ISTRT(2)=NPARM+K
            NQ2=NQ+NQ222
            DO iq=1,NQ
            DO jq=1,NQ
            IF(NFIX2(jq) .GT. NFIX3(jq)) CALL RSTVAL ('SIG AM  ',
     *             'KMAT AM ',K,XX,3)
            END DO
            END DO
            ij=0
            kk=nqq+nqq
            do i=1,nq
            do j=1,nq
            if(j.ge.i)then
               ij=ij+1
               sig(ihmssf(i,j,nq2))=xvec(ij)
               sig(ihmssf(nq+i,nq+j,nq2))=xvec(nqq+ij)
            end if
            kk=kk+1
            sig(ihmssf(i,nq+j,nq2))=xvec(kk)
            end do
            end do
            CALL CHKSIG('SIG AM ',ZERO,XVEC,SIG,0,NQ2,ioprun)
            IF(jparam.eq.2.and.(IOPRUN.NE.-2.or.iopsav.ne.ioprun))THEN
               WRITE(*,*)'ORDER OF POLYNOMIAL FIT FOR "SIG AM"  ?'
               CALL OPTDEF(KFIT(3),1,NQ2,NQ2)
            ELSE
               KFIT(3)=NQ2
            END IF
         END IF
         NPARM=NPARM+K
         IF(jparam.eq.2 .and. (IOPRUN.NE.-2.or.iopsav.ne.ioprun) )THEN
            WRITE(*,*)'ORDER OF POLYNOMIAL FIT FOR "SIG M"  ?'
            CALL OPTDEF(KFIT(2),1,NQ,kfit(1))
         ELSE
            KFIT(2)=NQ222
         END IF
      END IF
      NGPAR1=NPARM

C     ADDITIONAL RANDOM EFFECT COVARIANCE COMPONENTS
      IF(ioprn1.EQ.1)THEN
         IF(ISILEN.EQ.0)WRITE(*,*)' '
         ISTRT(3)=NPARM
         sig=0.d0
         K=0
         DO Iq=1,NQ
         IF(NFIX1(Iq).GT.NFIX(Iq))THEN
             DO Jq=Iq,NQ
             IF( NFIX1(Jq).GT.NFIX(Jq))CALL RSTVAL('SIG C   ','KMAT C  '
     *,                                            K,XX,4)
             END DO
         ELSE IF(ISILEN.EQ.0)THEN
             WRITE(*,*)'EFFECT NOT FITTED FOR TRAIT NO.',Iq
         END IF
         END DO
         CALL CHKSIG('SIG C   ',ZERO,XVEC,SIG,NPARM,NQ,ioprun)
         NPARM=NPARM+K
         IF(jparam.eq.2 .and. (IOPRUN.NE.-2.or.iopsav.ne.ioprun) )THEN
            WRITE(*,*)'ORDER OF POLYNOMIAL FIT FOR "SIG C"  ?'
            CALL OPTDEF(KFIT(4),1,NQ,kfit(1))
         ELSE
            KFIT(4)=NQ111
         END IF
         ngpar2=nparm
         if(ioprn3.eq.1)then
            IF(ISILEN.EQ.0)WRITE(*,*)' '
            ISTRT(7)=NPARM
            sig=0.d0
            K=0
            DO Iq=1,NQ
            IF(NFIX3(Iq).GT.NFIX1(Iq))THEN
                DO Jq=Iq,NQ
                IF( NFIX3(Jq).GT.NFIX1(Jq))CALL RSTVAL('SIG Q   ',
     *                                          'KMAT Q  ',K,XX,7)
                END DO
            ELSE IF(ISILEN.EQ.0)THEN
                WRITE(*,*)'EFFECT NOT FITTED FOR TRAIT NO.',Iq
            END IF
            END DO
            CALL CHKSIG('SIG Q   ',ZERO,XVEC,SIG,NPARM,NQ,ioprun)
            NPARM=NPARM+K
            IF(jparam.eq.2 .and.(IOPRUN.NE.-2.or.iopsav.ne.ioprun))THEN
               WRITE(*,*)'ORDER OF POLYNOMIAL FIT FOR "SIG Q"  ?'
               CALL OPTDEF(KFIT(7),1,NQ,kfit(1))
            ELSE
               KFIT(7)=NQ333
            END IF
         end if
      END IF

C     NO. OF PARAMETERS DUE TO RANDOM EFFECTS
      NGPARM=NPARM

C     ERROR COVARIANCE COMPONENTS
      IF(ISILEN.EQ.0)WRITE(*,*)' '
      ISTRT(4)=NPARM
      K=0
      IJ=0
      DO  Iq=1,NQ
      DO  Jq=Iq,NQ
      IJ=IJ+1
      IF( (IRPMOD.EQ.2. AND. Iq.EQ.Jq) .AND. MXOBS(Iq).EQ.1)THEN
         WRITE(*,*)' '
         WRITE(*,*)'NO REPEATED RECORDS FOR TRAIT =',Iq
         WRITE(*,*)' -->  CANNOT SEPARATE PERMANENT & TEMPORARY '
     *,            'ENVIRONMENTAL VARIANCES !'
         PP='SIG R+E '
      ELSE
         PP='SIG E   '
      END IF

      IF((IRPMOD.NE.2.AND.NBOTH(IJ).GT.0).OR.(IRPMOD.EQ.2.AND.
     *                                                Iq.EQ.Jq))THEN
         CALL RSTVAL(PP,'KMAT E  ',K,XX,5)
      ELSE 
         IF(ISILEN.EQ.0)THEN
            if(jparam.eq.2)then
               write(*,*)'cov. function model can not cope',
     *                  'with "missing" components ... '
               stop 'dfinp3'
            end if
            WRITE(*,*)' '
            IF(IRPMOD.NE.2)THEN
               WRITE(*,*)'CANNOT ESTIMATE COVARIANCE COMPONENT',PP,Iq,Jq
               WRITE(*,*)' -->  NO ANIMALS WITH RECORDS FOR BOTH TRAITS'
            ELSE
               WRITE(*,*)'MODEL DOES NOT ALLOW FOR COVARIANCE',PP,Iq,Jq
               WRITE(*,*)' -->  ASSUME RECORDS TAKEN AT DIFFERENT TIMES'
            END IF
         END IF
         XX=0.D0
         K=K+1
         SIG(K)=XX
         FFVEC(NPARM+K)=1.d0
729      FORMAT(A8,2I2)
         WRITE(PARA,729)PP,IQ,JQ
         PARAM(NPARM+K)=PARA
         PARVAR(NPARM+K)=PARA
         XX=XX/FFVEC(NPARM+K)
         XVEC(NPARM+K)=XX
         NPARNO(Iq,Jq,5)=NPARM+K
         NPARNO(Jq,Iq,5)=NPARM+K
      END IF

      IF(XX.EQ.0)then
         if(jparam.eq.1)ipm(nparm+k)=1
         NBOTH(IJ)=-NBOTH(IJ)
      end if
      END DO
      END DO
      CALL CHKSIG('SIG E   ',ZERO,XVEC,SIG,NPARM,NQ,ioprun)

      NPARM=NPARM+K
      IF(jparam.eq.2.and. (IOPRUN.NE.-2.or.iopsav.ne.ioprun) )THEN
         WRITE(*,*)'ORDER OF POLYNOMIAL FIT FOR "SIG E"  ?'
         CALL OPTDEF(KFIT(5),1,NQ,kfit(1))
      ELSE
         KFIT(5)=NQ
      END IF
 
C     "EQUIVALENT" REPEATABILITY MODEL
      IF(IRPMOD.EQ.2)THEN
         IF(ISILEN.EQ.0)WRITE(*,*)' '
         ISTRT(5)=NPARM
         K=0
         IJ=0
         DO Iq=1,NQ
         DO Jq=Iq,NQ
         IJ=IJ+1
         NBREP(IJ)=0
         IF( (Iq.NE.Jq.AND.NBOTH(IJ).NE.0) .OR.(Iq.EQ.Jq.AND.MXOBS(Iq)
     *                                                   .GT.1))THEN
          CALL RSTVAL('SIG R   ','KMAT R  ',K,XX,6)
           IF(XX.NE.0) NBREP(IJ)=K
         END IF
         END DO
         END DO
         CALL CHKSIG('SIG R   ',ZERO,XVEC,SIG,NPARM,NQ,ioprun)
         NPARM=NPARM+K
         IF(jparam.eq.2.and.(IOPRUN.NE.-2.or.iopsav.ne.ioprun))THEN
            WRITE(*,*)'ORDER OF POLYNOMIAL FIT FOR "SIG R"  ?'
            CALL OPTDEF(KFIT(6),1,NQ,NQ)
         ELSE
            KFIT(6)=NQ
         END IF
      END IF

C     NO. OF PARAMETERS FOR RESIDUAL COV.S
      NRPARM=NPARM-NGPARM

      IF(IOPRUN.EQ.18)IOPRUN=8

      if(jparam.eq.2 .and. kfit(5).lt.nq)then
         write(*,*)'fit i.i.d. measurement errors ?'
         call yndef(iomease,1)
         if(iomease.eq.1)then
           k=0
           do iq=1,nq
           jq=iq
           CALL RSTVAL('SIG ME  ','SIG ME  ',K,XX,8) 
           end do
         end if
      else
         iomease=0
      end if

      ioprun=iopsav
      allocate (corfix(nparm),ivarvc(2,nparm),stat=ii)
      if(ii>0)stop 'dfipm3 : alloc '
      allocate(currnt(nparm+nq),stat=ii)
      if(ii>0)stop 'alloc : current'
      currnt=0.d0

      RETURN

      contains

C     =================================
      SUBROUTINE RSTVAL (pp,qq,k,xx,ic)
C     =================================

      USE platform
      character(len=8),intent(in) :: pp,qq
      integer, intent(inout)      :: k
      real(8), intent (out)       :: xx
      integer, intent(in)         :: ic

      CHARACTER(len=12)           :: PARA
      real(8)                     :: x1,x2,xx1

      WRITE(PARA,729)PP,Iq,Jq

      XX=100.d0
      IF(PP.EQ.'SIG AM  '.or.iq.ne.jq)XX=0.1d0
      XX1=XX
      IF(IOPRUN.NE.-2.AND.IOPRUN.LE.4.and.iinput.ne.5)THEN
         X2=20.D0*SDEV(Iq)*SDEV(Jq)
         IF(IQ.EQ.JQ .and .PP.NE.'SIG AM  ')THEN
            X1=0.D0
         ELSE
            X1=-X2
         END IF
         IF(IUN.EQ.0)WRITE(*,719)PARA
         IF(ipltf.eq.2)WRITE(*,*)' '
         CALL rvalfl(XX,X1,X2,iun)
         IF(IUN.GT.0)WRITE(*,*)' READ : ',PARA,' = ',XX
      ELSE IF(IOPRUN.EQ.18)THEN
         WRITE(*,719)PARA
         CALL RVLDEF(XX,-99999.d0,99999.d0,XX1)
      END IF

      K=K+1
      IF(NPARM+K.GT.MXPARM)THEN
         WRITE(*,*)'ROUTINE "RSTVAL" : DIMENSION EXCEEDED !!!'
         WRITE(*,*)'CURRENT MAXIMUM OF PARAMETERS =',MXPARM
         STOP 'RESET PARAMETER "MXPARM" !'
      END IF

C     FIX ZERO COVARIANCES
      IPM(NPARM+K)=0
      IF(JPARAM.EQ.1 .AND.XX.EQ.0)IPM(NPARM+K)=1

      SIG(K)=XX
      FFVEC(NPARM+K)=1.D0
      XX=XX/FFVEC(NPARM+K)
      XVEC(NPARM+K)=XX

      PARVAR(NPARM+K)=PARA
      IF(JPARAM.EQ.2) WRITE(PARA,729)QQ,IQ,JQ
      PARAM(NPARM+K)=PARA
      NPARNO(Iq,Jq,IC)=NPARM+K
      IF(IC.NE.3)NPARNO(Jq,Iq,IC)=NPARM+K
      
      RETURN
719   FORMAT(' GUESS FOR :  ',A,'  ?     ')
729   FORMAT(A8,2I2)
      END subroutine rstval
 
      end subroutine dfinp3
