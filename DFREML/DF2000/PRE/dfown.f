!=========================================================================
      SUBROUTINE DFOWN (iopt,iun34,nnown)
!=========================================================================

      use pedigrees
      use list_of_ids
      use form_of_inputs
      use c_numbers
      use ages

!     arguments
      integer, intent(in)   :: iopt,iun34
      integer, intent(out)  :: nnown
      
!     local variables
      integer, dimension(5) :: ivec
      integer               :: ids,ids1,nd,iend,iq,ianim

      mrow2=ubound(kfirst,1)

      REWIND(IUN34)
      nown=0
      ND=0
      IDS1=0
      IEND=0
      if(iopt>3)nage=0

 750  CALL DFRD34 (IUN34,IOFFLD,IOPT,IVEC,IQ,IEND,INFMTD)
      IF(IEND.EQ.99)GO TO 799
      CALL KOUNT(ND,5000)

      ids=ivec(1)
      if(iospec.eq.4)then
         ids=ivec(2)                         ! sire model
         if(ids.eq.0)go to 750
      end if

      IF(IDS.EQ.0)THEN
         WRITE(*,*)'DATA ERROR : ANIMAL ID IS ZERO !'
         WRITE(*,*)'RECORD NO. =',ND
         STOP 'TIDY UP DATA !!'
      END IF

      IF(IDS.NE.IDS1)THEN
         CALL LNKFND(IDS,IANIM)
         IDS1=IDS
      END IF
      NOWN(IANIM)=NOWN(IANIM)+1

      IF(IOPT.ge.4)CALL DFAGES(IQ,IUN11,IUN66)

      GO TO 750

799   NNOWN=count ( mask=(nown>0) )
      WRITE(*,111)'NO. OF ANIMALS IN DATA',NNOWN

      IF(IOPT.ge.4)THEN
         WRITE(*,111)'NO. OF "AGES" FOUND',NAGE
         if(iopt.eq.4)then
            NQ=NAGE(1)
            KCOV(2:nq)=KCOV(1)
            KFIX(2:nq)=KFIX(1)
            KINT(2:nq)=KINT(1)
            KNSERT(2:nq)=KNSERT(1)
         end if
      END IF         

      RETURN
111   FORMAT(1X,A,T45,' = ',I15)
      END subroutine dfown














