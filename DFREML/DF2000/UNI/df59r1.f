!==========================================================================
      SUBROUTINE DFRC59 (nparm,kparm,kq,fmin,sige)
!==========================================================================

      use likelihoods
      use comments
      use parameters
      use iterates
      use constants
      use units
      use numbers

      integer, intent(in)                :: kparm,nparm,kq
      real(8), intent(inout)             :: fmin, sige

      real(8), dimension(:), allocatable :: wvec
      LOGICAL                            :: LEXIST

C     RECOVER ANY LIKELIHOOD EVALUATIONS FROM UNFINISHED RUNS
      II59=-1
      N=-1
      INQUIRE(FILE='DF59#DAT',EXIST=LEXIST)
      IF(LEXIST)THEN
C        ... CHECK THAT WE'VE GOT A FILE FOR THIS DATA SET & ANALYSIS !
         READ(IUN59,END=789)II59
         N=0
         IF(II59.EQ.0)THEN
            READ(IUN59)krec,kfix,kfix1,kfix2,kcov,kfr,kfl,koprn1,koprn2,
     &                 kmodel,kim1,kim2,kim3
            if(krec.eq.nrec)go to 100
            if(kfix.eq.nfix.and.kfix1.eq.nfix1.and.kfix2.eq.nfix2)
     &      go to 100
            if(kfl.eq.nfl.and.kfr.eq.nfr)go to 100
            if(kcov.eq.ncov.and.kmodel.eq.imodel)go to 100
            if(koprn1.eq.ioprn1.and.koprn2.eq.ioprn2)go to 100
            if(kim1.eq.lim1.and.kim2.eq.lim2.and.kim3.eq.lim3)go to 100
            stop '"dfrc59" : wrong file DF59#DAT ?!! '
 100        READ(IUN59)LLINES
            IF(LLINES.NE.KLINES)THEN
               WRITE(*,*)'WRONG FILE "59" ??!!!'
               WRITE(*,*)'DISCREPANCY FOUND IN NO. OF COMMENT LINES'
               STOP 'STRAIGHTEN OUT FILES & MODELS !'
            END IF
            IF(KLINES.GT.0)THEN
               DO I=1,KLINES
               READ(IUN59)TTEXT
               IF(TTEXT.NE.TEXT(I))THEN
                  WRITE(*,*)'WRONG FILE "59" ??!!!'
                  WRITE(*,*)'DISCREPANCY FOUND IN COMMENT LINE',I
                  WRITE(*,*)'FOUND :  ',TTEXT
                  WRITE(*,*)'EXPECT : ',TEXT(I)
                  STOP 'STRAIGHTEN OUT FILES & MODELS !'
               END IF
               end do
            END IF
         END IF

799      NN4=NPARM+KQ
         allocate(wvec(nn4+kq),stat=ii)
         if(ii>0)stop 'alloc wvec'
777      READ(IUN59,END=789,ERR=779)N,WVEC
         IF(N.GT.NCALL)THEN
            NCALL=NCALL+1
            IF(NCALL.LE.MXFUNC)THEN
               FEVAL(:nparm,NCALL)=WVEC(:nparm)
               FEVAL(NPARM+1,nCALL)=WVEC(NPARM+NTRAIT)
               FEVAL(NPARM+2,nCALL)=WVEC(NN4+NTRAIT)
            END IF
            IF(KPARM.EQ.NPARM .AND. WVEC(NPARM+NTRAIT).LT.FMIN)THEN
               FMIN=WVEC(NPARM+NTRAIT)
               XVEC(:nparm)=WVEC(:nparm)
               sige=wvec(nparm+nq+ntrait)
            END IF
         END IF
         GO TO 777
      END IF

789   WRITE(*,*)' '
      IF(II59.EQ.-1.AND.N.EQ.-1)then
         rewind(iun59)
         WRITE(IUN59)II59
      end if
      RETURN
 779  write(*,*)'routine "dfrc59" : read error when picking up ',
     *                           'previously evaluated  points!'
      write(*,*)'** likely reason is discrepancy in no. of parameters'
      write(*,*)'** between this and a previous run'
      write(*,*)'   no. of values expected for current run =',nn4+kq
      stop

C==========================================================================
      ENTRY DFWR59(NPARM,KPARM,KQ,MM,FMIN,sige)
C==========================================================================

      II59=0
      WRITE(IUN59)II59 
      write(IUN59)nrec,nfix,nfix1,nfix2,ncov,nfr,nfl,ioprn1,ioprn2,
     &            imodel,lim1,lim2,lim3
      WRITE(IUN59)KLINES
      DO  I=1,KLINES
      WRITE(IUN59)TEXT(I)
      end do
      CLOSE(IUN59)
      RETURN
      END subroutine dfrc59

