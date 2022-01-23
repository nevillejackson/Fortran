c========================================================================
      subroutine dfpids (jospec,iopt,npedr)
c========================================================================

      use list_of_ids
      use form_of_inputs
      use c_numbers
      use units

!     arguments
      integer, intent(in)  :: jospec,iopt
      integer, intent(out) :: npedr
      integer              :: ivec(5)=0, iend=0

      npedr=0
      MROW2=0
      kfirst=0

      if(fped(1:6).eq.'      ')go to 100
      rewind(iun33)

50    CALL DFRD33(IUN33,IOFFLP,IVEC,3,IEND,INFMTP)
      IF(IEND.EQ.99)GO TO 99

      IF( (ivec(2).GE.ivec(1)) .OR. (ivec(3).GE.ivec(1)) )THEN
         WRITE(*,*)'PROGENY CODED BEFORE PARENTS !!!!'
         WRITE(*,*)'ANIMAL  =',ivec(1)
         WRITE(*,*)'SIRE    =',ivec(2)
         WRITE(*,*)'DAM     =',ivec(3)
         STOP 'RECODE PEDIGREES !'

      ELSE IF(ivec(2).GT.0 .AND. ivec(2).EQ.ivec(3))THEN
c        comment this section out if selfing is feasible (e.g. trees)
         WRITE(*,*)'ANIMAL HAS IDENTICAL PARENTS !!!!'
         WRITE(*,*)'ANIMAL  =',ivec(1)
         WRITE(*,*)'SIRE    =',ivec(2)
         WRITE(*,*)'DAM     =',ivec(3)
         STOP 'CHECK PEDIGREES !'

      ELSE IF(ivec(1).EQ.0)THEN
         WRITE(*,*)'ANIMAL ID IS ZERO !'
         WRITE(*,*)'PEDIGREE RECORD NO. =',NPEDR+1
         STOP 'CHECK PEDIGREE FILE'
      END IF

      CALL KOUNT(NPEDR,2500)
      CALL LNKTAB(ivec(1))
      IF(ivec(2).GT.0)CALL LNKTAB(ivec(2))
      IF(ivec(3).GT.0)CALL LNKTAB(ivec(3))
      GO TO 50

 99   WRITE(*,111)'NO. OF PEDIGREES READ',NPEDR
      WRITE(*,111)'NO. OF IDENTITIES FOUND',NANIM2
      NBASE=NANIM2-NPEDR
      WRITE(*,111)'NO. OF "BASE" ANIMALS',NBASE
      IF(JOSPEC.EQ.4)THEN
C        ... READ 2ND ANIMAL CODES & ADD TO ID LIST IF APPLICABLE
         REWIND(IUN34)
         N34=0
         IEND=0
         IQ=1
350      CALL DFRD34 (IUN34,IOFFLD,IOPT,IVEC,IQ,IEND,INFMTD)
         IF(IEND.EQ.99)GO TO 399
         IF(KNSERT(IQ).GT.3)THEN
            IFOST=JVEC(KINT(IQ))
            CALL LNKTAB( IFOST )
         END IF
         N34=N34+1
         GO TO 350
 399     WRITE(*,111)'NO. OF ID.S INCLUDING 2ND ANIM. CODES',NANIM2
         WRITE(*,111)'NO. OF "DATA" RECORDS READ',N34
      END IF

      return
111   FORMAT(1X,A,T45,' = ',I15)


 100  write(*,*)'Read animal IDs from data file - ignore pedigrees !'
      rewind(iun34)

 150  CALL DFRD34 (IUN34,IOFFLD,IOPT,IVEC,IQ,IEND,INFMTD)
      IF(IEND.EQ.99)GO TO 199
      CALL KOUNT(NPEDR,2500)
      if(iospec.ne.4)then
         CALL LNKTAB(ivec(1))
      else
         if(ivec(2).eq.0)go to 150
         CALL LNKTAB(ivec(2))
      end if
      go to 150
 199  write(*,111)'No. of animals',nanim2
      return

      contains

C     ======================
      SUBROUTINE LNKTAB(IDS)
C     ======================

      integer, intent(in)  :: ids

      IF(IDS.EQ.0)return
      IF(IDS.GT.IDMAX)THEN
         WRITE(*,*)'ID GIVEN IS GREATER THAN MAXIMUM SPECIFIED !'
         WRITE(*,*)'MAX. WAS =',IDMAX,'  CURRENT ID IS =',IDS
         STOP 'SUBROUTINE "LNKTAB"'
      END IF

C     SPLIT ID INTO "ROW" AND "COLUMN"
      IROW=IDS/IFAC+1
      ICOL=MOD(IDS,IFAC)
      IF(IROW.GT.MROW2)MROW2=IROW
      IPRE=0
      IPLACE=KFIRST(IROW)
      do while(iplace>0)
         if(kvcol(iplace).eq.icol)return
         if(kvcol(iplace)>icol)exit
         IPRE=IPLACE
         IPLACE=KNEXT(IPLACE)
      end do
      NANIM2=NANIM2+1
      IF(NANIM2.GT.MXREC2)STOP '"LNKTAB" : DIMENSIONS EXCEEDED !'
      IDVEC(NANIM2)=IDS
      KVCOL(NANIM2)=ICOL
      IF(IPLACE.EQ.0.AND.KFIRST(IROW).EQ.0)THEN
         KFIRST(IROW)=NANIM2
         KNEXT(NANIM2)=0
      ELSE IF(IPLACE.EQ.0.AND.KFIRST(IROW).NE.0)THEN
         KNEXT(IPRE)=NANIM2
         KNEXT(NANIM2)=0
      ELSE IF(IPLACE.NE.0.AND.IPRE.EQ.0)THEN
         KNEXT(NANIM2)=KFIRST(IROW)
         KFIRST(IROW)=NANIM2
      ELSE
         KNEXT(NANIM2)=KNEXT(IPRE)
         KNEXT(IPRE)=NANIM2
      END IF
      RETURN
      END subroutine lnktab

      end subroutine dfpids











