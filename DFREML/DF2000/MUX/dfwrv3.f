C============================================================================
      SUBROUTINE DFWRV3 (xvec,nparm)
C============================================================================

      use params
      use names
      use units
      use parmap
      use sigmas
      use correlations
      use combinations
      use numbers

!     arguments
      real(8), dimension(mxparm),intent(in) :: xvec
      integer, intent(in)                   :: nparm
     
!     local variables
      CHARACTER(len=25)                     :: UNKN,FORMA,FSTAND
      integer                               :: i,j,ij
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      UNKN='UNKNOWN'
      FORMA='FORMATTED'
      FSTAND='DF19#DAT'
      CALL FCONCT(IUN19,FSTAND,FORMA,UNKN)

      WRITE(IUN66,908)'-------------------------------------------'
      WRITE(IUN66,*)  'ESTIMATES OF VARIANCES & GENETIC PARAMETERS'
      WRITE(IUN66,*)  '-------------------------------------------'
      WRITE(IUN66,*)' '

      WRITE(IUN66,908)'ADDITIV-GENETIC (DIRECT) COVARIANCE MATRIX'
      DO  I=1,NQ
      WRITE(IUN66,911)I,SIGA(:i,I)
      WRITE(IUN19,913)(SIGA(I,J),parvar(nparno(i,j,1)),J=I,nq)
      end do
      if(nquni.eq.0)then
         WRITE(IUN66,*)'... WITH EIGENVALUES '
         WRITE(IUN66,910)EIGA(:NQ)
      end if
      IF(IOPRN2.EQ.1)THEN
         WRITE(IUN66,908)'SECOND "ANIMAL" COVARIANCE MATRIX'
         DO I=1,NQ
         WRITE(IUN66,911)I,SIGM(I,:i)
         end do
         if(nquni.eq.0)then
            WRITE(IUN66,*)'... WITH EIGENVALUES '
            WRITE(IUN66,910)EIGM(:NQ)
         end if
         do i=1,nq
         do j=i,nq
         ij=nparno(i,j,2)
         if(ij.gt.0)WRITE(IUN19,913)SIGM(J,I),parvar(ij)
         end do
         end do
      END IF

      IF(IOPCOV.EQ.2)THEN
         WRITE(IUN66,908)'COVARIANCE MATRIX BETWEEN ANIMAL EFFECTS'
         DO I=1,NQ
         WRITE(IUN66,911)I,SIGAM(I,:nq)
         end do
         do i=1,nq
         do j=1,nq
         ij=nparno(i,j,3)
         if(ij.gt.0)WRITE(IUN19,913)SIGAM(J,i),parvar(ij)
         end do
         end do
      END IF

      IF(IOPRN1.EQ.1)THEN
         WRITE(IUN66,908)'COVARIANCE MATRIX FOR ADD. RANDOM EFFECT'
         DO I=1,NQ
         WRITE(IUN66,911)I,SIGC(:i,i)
         end do
         if(nquni.eq.0)then
            WRITE(IUN66,*)'... WITH EIGENVALUES '
            WRITE(IUN66,910)EIGC(:NQ)
         end if
         do i=1,nq
         do j=i,nq
         ij=nparno(i,j,4)
         if(ij.gt.0)WRITE(IUN19,913)SIGC(J,I),parvar(ij)
         end do
         end do
      END IF
      IF(IOPRN3.EQ.1)THEN
         WRITE(IUN66,908)'COVARIANCE MATRIX FOR 2nd ADD. RANDOM EFFECT'
         DO I=1,NQ
         WRITE(IUN66,911)I,SIGQ(:i,i)
         end do
         if(nquni<1)then
            WRITE(IUN66,*)'... WITH EIGENVALUES '
            WRITE(IUN66,910)EIGQ(:NQ)
         end if
         do i=1,nq
         do j=i,nq
         ij=nparno(i,j,7)
         if(ij.gt.0)WRITE(IUN19,913)SIGQ(J,I),parvar(ij)
         end do
         end do
      END IF

      IF(IRPMOD.NE.2)THEN
         WRITE(IUN66,908)'ERROR COVARIANCE MATRIX'
      ELSE
         WRITE(IUN66,908)'RESIDUAL COVARIANCE MATRIX'
      END IF
      DO I=1,NQ
      WRITE(IUN66,911)I,SIGE(I,:i)
      END DO
      if(nquni<1)then
         WRITE(IUN66,*)'... WITH EIGENVALUES '
         WRITE(IUN66,910)EIGE(:NQ)
      end if
      do i=1,nq
      do j=i,nq
      ij=nparno(i,j,5)
      if(ij.gt.0)WRITE(IUN19,913)SIGE(J,I),parvar(ij)
      end do
      end do
      IF(IRPMOD.EQ.2)THEN
         WRITE(IUN66,908)'PARTITIONING OF RESIDUAL VARIANCES'
         DO I=1,NQ
         WRITE(IUN66,9452)I,NXOBS(I),SIGE(I,I)
         IF(NXOBS(I).GT.1)WRITE(IUN66,9453)SIGR(I,I),SIGE(I,I)-SIGR(I,I)
          END DO
      END IF

      if(iomease.eq.1)then
         WRITE(IUN66,908)'Measurement errors'
         do i=1,nq
         write(iun66,911)i,xvec(nparm+i)
         WRITE(IUN19,913)xvec(nparm+i),parvar(nparm+i)
         end do
      end if

      WRITE(IUN66,908)'PHENOTYPIC COVARIANCE MATRIX'
      DO I=1,NQ
      WRITE(IUN66,911)I,SIGP(I,:i)
      END DO
      if(nquni<1)then
         WRITE(IUN66,*)'... WITH EIGENVALUES '
         WRITE(IUN66,910)EIGP(:NQ)
      end if

      WRITE(IUN66,908)'HERITABILITIES & ADDITIV-GENETIC CORRELATIONS'
      DO I=1,NQ
      WRITE(IUN66,912)I,(RRA(IHMSSF(I,J,NQ)),J=1,I)
      END DO

      IF(IOPRN2.EQ.1)THEN
         WRITE(IUN66,908)'MATRIX OF "M-SQUARED" VALUES & CORRELATIONS'
         DO I=1,NQ
         IF(SIGM(I,I).GT.0)WRITE(IUN66,912)I,(RRM(IHMSSF(I,J,NQ)),J=1,I)
         END DO

         IF(IOPCOV.EQ.2)THEN
            WRITE(IUN66,908)'CORRELATION MATRIX BETWEEN ANIMAL EFFECTS'
            DO I=1,NQ
            WRITE(IUN66,912)I,(RRAM(I,J),J=1,NQ)
            END DO
        END IF
      END IF

      IF(IOPRN1.EQ.1)THEN
         WRITE(IUN66,908)'MATRIX OF "C-SQUARED" VALUES & CORRELATIONS'
         DO I=1,NQ
         IF(SIGC(I,I).GT.0)WRITE(IUN66,912)I,(RRC(IHMSSF(I,J,NQ)),J=1,I)
         END DO
      END IF
      IF(IOPRN3.EQ.1)THEN
         WRITE(IUN66,908)'MATRIX OF 2nd "C-SQUARED" V. & CORRELATIONS'
         DO I=1,NQ
         WRITE(IUN66,912)I,(RRQ(IHMSSF(I,J,NQ)),J=1,I)
         END DO
      END IF

      WRITE(IUN66,908)'MATRIX OF "E-SQUARED" VALUES & CORRELATIONS'
      DO I=1,NQ
      WRITE(IUN66,912)I,(RRE(IHMSSF(I,J,NQ)),J=1,I)
      end do

      if(nquni<1)then
         WRITE(IUN66,908)'MATRIX OF PHENOTYPIC CORRELATIONS'
         DO I=1,NQ
         WRITE(IUN66,912)I,(RRP(IHMSSF(I,J,NQ)),J=1,I)
         end do
      end if
      IF(IRPMOD.EQ.2)THEN
         WRITE(IUN66,908)'"P-SQUARED" VALUES & REPEATABILITIES'
         DO IQ=1,NQ
         IF(NXOBS(IQ).GT.1)THEN
            PSQ=SIGR(IQ,IQ)/SIGP(IQ,IQ)
            REP=(SIGA(IQ,IQ)+SIGR(IQ,IQ))/SIGP(IQ,IQ)
            WRITE(IUN66,9541)IQ,NXOBS(IQ),PSQ,REP
         END IF
         END DO
      END IF
      RETURN
C...........................................................................
902   FORMAT(1X,A,T50,'=',I20)
922   FORMAT(1X,A,T50,':',4X,A)
906   FORMAT(1X,A,T50,'=',G20.12)
907   FORMAT(1X,A,T50,'=',G23.15)
908   FORMAT(/1X,A)
910   FORMAT(4x,(t5,6G13.5))
911   FORMAT(I4,(t5,6G13.5))
912   FORMAT(I4,(t5,6F13.4))
913   FORMAT(g20.8,20x,a)
919   FORMAT(G18.10,(1X,T20,7G12.5))
2412  FORMAT(I4,4X,A,2G20.10)
9452  FORMAT(/I4,4x,'MAX. NO. OF REPEATED RECORDS     =',I12/
     *          8X,'TOTAL RESIDUAL VARIANCE          =',G13.5)
9453  FORMAT(   8X,'PERMANENT ENVIRONMENTAL VARIANCE =',G13.5/
     *          8X,'TEMPORARY ENVIRONMENTAL VARIANCE =',G13.5)
9541  FORMAT(I4,I6,2F10.4)

      END subroutine dfwrv3





