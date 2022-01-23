c==========================================================================
      subroutine dfprec (npedr,iun33,iopibr,iopbas,ivlong)
c==========================================================================

      use pedigrees
      use list_of_ids
      use form_of_inputs
      use c_numbers
      use gen_groups

      integer, intent(in)   :: npedr, iun33, iopibr,iopbas,ivlong
      integer, dimension(5) :: ivec
     
      npro=0
      isex=0
      idsire=0
      iddam=0
      if(fped(1:6).eq.'      ')return

      IF(IOPIBR.EQ.1) igvec=0

      REWIND IUN33

      DO  I=1,NPEDR

      IF(IOPIBR.EQ.0)THEN
         CALL DFRD33(IUN33,IOFFLP,IVEC,IVLONG,IEND,INFMTP)
      ELSE
         CALL DFRD33(IUN33,IOFFLP,IVEC,IVLONG,IEND,INFMTP)
         IGEN=IVEC(4)
         IF(IGEN.GT.LLEV.OR.IGEN.LT.0)THEN
            WRITE(*,*)'INVALID CODE FOR ',GNAME,' :',IGEN
            STOP 'CHECK RECORDS !'
         END IF
      END IF

      CALL LNKFND(IVEC(1),IANIM)

      DO II=2,3
      IF(IVEC(II).GT.0)THEN
         CALL LNKFND(IVEC(II),MDS)
         IF(II.EQ.2)THEN
            IdsIRE(IANIM)=MDS
         ELSE
            IDDAM(IANIM)=MDS
         END IF
         NPRO(MDS)=NPRO(MDS)+1
         IF(ISEX(MDS).EQ.0)THEN
            ISEX(MDS)=II-1
         ELSE IF(ISEX(MDS).NE.II-1.and.iospec.ne.4)THEN
            WRITE(*,*)'ERROR IN PEDIGREE ENCOUNTERED :'
            WRITE(*,*)'    ANIMAL             =',IVEC(1),IANIM
            WRITE(*,*)'    PARENT NO.         =',II
            WRITE(*,*)'    ID                 =',IVEC(II),MDS
            WRITE(*,*)'    NO. PROGENY SO FAR =',NPRO(MDS)
            WRITE(*,*)'    PREVIOUS SEX CODE  =',ISEX(MDS)
            STOP 'CHECK PEDIGREES !!!'
        END IF
      END IF
      end do

      IF(IOPIBR.EQ.1)IGVEC(IANIM)=IGEN
      IF(IOPBAS.EQ.3)IFXMRK(I)=IVEC(IVLONG)
      end do

      return

      end subroutine dfprec








