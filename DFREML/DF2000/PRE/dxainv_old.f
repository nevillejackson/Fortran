!=======================================================================
      subroutine aa_inverse(nanim, iopbas,iopibr,ninbr,detl,
     &                                               xinbr,sfval)
!=======================================================================

      use parameters
      use pedigrees
      use list_of_ids
      use nrm_inverse
      use c_numbers
      use gen_groups
      use linkrow
      use work
      use units
      use form_of_inputs

      integer, intent(in)                  :: nanim,iopbas,iopibr
      integer, intent(out)                 :: ninbr
      real(8), intent(out)                 :: xinbr, sfval,detl

      real(8), dimension(:), allocatable   :: v
      real(8)                              :: d,d4,xx,xsave
      integer                              :: nafix,narnd,mzhz,iowrt=0,
     &                                        kafix,karnd,kanim,ii
      integer, dimension(:), allocatable   :: itype
      logical                              :: lexist
      character(len=8)                     :: fname='DF44#DAT'

      DETL=0.D0
      NINBR=0    ! count(mask(u>1.d0))
      SFVAL=0.d0 ! sum(u,mask=u>1.d0)
      NAFIX=0
      NARND=0
      allocate(u(0:nanim2),v(nanim),ivec(nanim),itype(nanim2),stat=ii)
      if(ii>0)stop 'alloc nrm'
      ivec=0

!     set up list of "type" of animal codes (multiple NRM)
      itype=1                       ! animals *not* in pedigree list are given
      allocate(nna(nainv))          ! code "1"
      nna=0
      if(nainv>1)then       
         rewind(iun33)
         iend=0
150      call dfrd33(iun33,iofflp,ivec,4,iend,infmtp)
         if(iend.eq.99)go to 199
         if(ivec(4)<1.or.ivec(4)>nainv)then
            write(*,*)'invalid "type of animal" code',ivec(4)
            write(*,*)'range allowed',1,nainv
            stop 'check type codes'
         end if
         call lnkfnd(ivec(1),nds)
         itype(nds)=ivec(4)
         go to 150
 199     continue
         write(*,*)'no. of "types" of animals  =',nainv
         do i=1,nainv
         nna(i)=count(mask=(itype.eq.i))
         write(*,*)i,'    N =', nna(i)
         end do
      end if
      
!     check whether a-inverse already exists
      inquire(IUN44,name=fname,EXIST=LEXIST)
      if(lexist)then
         read(iun44,iostat=ii)detl,iowrt,kanim,kafix,karnd,kamod,kainv

         if(ii.eq.0.and.(kanim.eq.nanim.and.kafix.eq.nafix.and.
     &                                            kainv.eq.nainv))then
            write(*,*)'File "DF44#DAT" already exists in current',
     &                                               ' directory'
            write(*,*)'Skip calculation of NRM inverse ?'
            call yndef(jj,1)
            if(jj.eq.1)then
                xinbr=0.d0
                close(iun44)
                return
            end if
            DETL=0.D0
         end if
      end if

!     -----------
!     NRM inverse
!     -----------

!     allocate arrays
      mzhz=nanim2*50
      write(*,*)'Max. no. of non-zero elements in NRM inverse ?'
      call optdef(mzhz,nanim2,99999999,mzhz)
      allocate(zhz(mzhz),ifirst(nanim2),inext(mzhz),ivcol(mzhz),
     &         rvec(nanim),stat=ii)
      if(ii>0)then
         write(*,*)'could not allocate arrays required to set up NRM'
         stop 'alloc nrm 2'
      end if
      
      rewind(iun44)
      DETL=0.D0
      NINBR=0
      SFVAL=0.D0
      NAFIX=0
      NARND=0

      do iainv=1,nainv
      NREC1=0                     ! build L matrix using all animals
      ifirst=0
      zhz=0.d0
      inext=0
      ivcol=0
      u=0.d0
      DO  2 IA=1,NANIM
      IDAD=IDSIRE(IA)
      IMUM=IDDAM(IA)
      ianim=ia

C     DIAGONAL ELEMENT OF COLUMN I OF TRIANGULAR MATRIX L
      XX=0.D0
      IF(IDAD.GT.0)XX=U(IDAD)
      IF(IMUM.GT.0)XX=XX+U(IMUM)
      IF(XX.NE.0.D0)THEN
         XX=1.D0-0.25D0*XX
         D=1.D0/XX
         U(IANIM)=U(IANIM)+XX
         XX=DSQRT(XX)
         V(IANIM)=XX
      ELSE
         D=1.D0
         U(IANIM)=U(IANIM)+1.D0
         V(IANIM)=1.D0
      END IF
      XSAVE=XX

C     OTHER ELEMENTS OF I-TH COLUMN
      DO LA=IA+1,NANIM
      L=la
      J=IDSIRE(LA)
      K=IDDAM(LA)
      XX=0.D0
      IF(J.GE.IANIM)XX=V(J)
      IF(K.GE.IANIM)XX=XX+V(K)
      IF(XX.NE.0.D0)THEN
         XX=0.5D0*XX
         U(L)=U(L)+XX*XX
      END IF
      V(L)=XX
      end do

C     CONTRIBUTIONS TO NRM INVERSE
      if(itype(ia).ne.iainv)go to 2 !only contributions for current type of an.
      IF(XSAVE.GT.ZERO)DETL=DETL+DLOG(XSAVE)
      D2=-0.5D0*D
      D4=0.25D0*D
      I1=MIN0(IDAD,IMUM)
      I2=MAX0(IDAD,IMUM)
      LL=0
      IF(I1.GT.0)THEN
         LL=LL+1
         IVEC(LL)=I1
         RVEC(LL)=D4
         CALL LNKROW(I1,LL)
      END IF
      IF(I2.GT.0)THEN
         LL=LL+1
         IVEC(LL)=I2
         RVEC(LL)=D4
         CALL LNKROW(I2,LL)
      END IF
      RVEC(:ll)=D2
      LL=LL+1
      RVEC(LL)=D
      IVEC(LL)=IANIM
      CALL LNKROW(IANIM,LL)

 2    CONTINUE  ! NRM complete (for current type of animal)

!     WRITE OUT ELEMENTS TO UNIT "44"
      if(iainv.eq.1)WRITE(IUN44)-0.5d0*DETL,IOWRT,NANIM,NAFIX,NARND,
     &                                      NAMOD,nainv,nna(:nainv)
      
      DO IROW=1,NANIM
      UU=U(IROW)
      IF(DABS(UU-1.D0).GT.ZERO)THEN
          NINBR=NINBR+1
          SFVAL=SFVAL+UU
      END IF
      KK=0
      IJ=IFIRST(IROW)
      do while(ij>0)
         WRITE(IUN44)IVCOL(IJ),IROW,ZHZ(IJ),iainv
         IJ=INEXT(IJ)
      end do
      end do ! irow

C     COUNT NO. OF ANIMALS IN MODEL OF ANALYSIS
      NAMOD=namod+count (mask=(ifirst>0) )

      end do ! ianinv

      write(iun44)-1,-1,detl,0
      close(iun44)

      WRITE(*,111)'NO. OF "EFFECTIVE" ANIMALS IN THE ANALYSIS',NAMOD
      WRITE(*,111)'NO. OF NON-ZERO ELEMENTS OF NRM INVERSE ',NREC1
      WRITE(*,112)'LOG DETERMINANT OF NRM',(DETL+DETL)

      XINBR=(SFVAL+NANIM2-NINBR)/NANIM2
      WRITE(*,111)'TOTAL NO. OF ANIMALS',NANIM2
      WRITE(*,112)'... WITH AVERAGE INBREEDING COEFFICIENT',XINBR

      deallocate (zhz,ifirst,inext,ivcol,rvec,stat=ii)
      if(ii>0)stop 'dealloc nrm'

111   FORMAT(1X,A,T45,' = ',I15)
112   FORMAT(1X,A,T45,' = ',F15.5)
      return

      contains

!     ==========================
      SUBROUTINE LNKROW(IROW,NN)
!     ==========================

      IF(NN.EQ.0)RETURN
      KK=0
      IPRE=0
      IPLACE=IFIRST(IROW)
      do while(kk<nn)
      KK=KK+1
      ICOL=IVEC(KK)

      do while( (IPLACE.GT.0) .AND. (IVCOL(IPLACE).LT.ICOL) )
         IPRE=IPLACE
         IPLACE=INEXT(IPLACE)
      end do
      IF( (IPLACE.GT.0) .AND. (ICOL.EQ.IVCOL(IPLACE)) )THEN
         ZHZ(IPLACE)=ZHZ(IPLACE)+rVEC(KK)
         IPRE=IPLACE
         IPLACE=INEXT(IPLACE)
      ELSE
         NREC1=NREC1+1
         IF(NREC1.GT.MZHZ)stop '"LNKROW" : DIMENSIONS EXCEEDED !!'
         ZHZ(NREC1)=rVEC(KK)
         IVCOL(NREC1)=ICOL
         IF(IPLACE.EQ.0.AND.IFIRST(IROW).EQ.0)THEN
            IFIRST(IROW)=NREC1
            INEXT(NREC1)=0
         ELSE IF(IPLACE.EQ.0.AND.IFIRST(IROW).NE.0)THEN
             INEXT(IPRE)=NREC1
             INEXT(NREC1)=0
         ELSE IF(IPLACE.NE.0.AND.IPRE.EQ.0)THEN
             INEXT(NREC1)=IFIRST(IROW)
             IFIRST(IROW)=NREC1
         ELSE
             INEXT(NREC1)=INEXT(IPRE)
             INEXT(IPRE)=NREC1
         END IF
         IPRE=NREC1
         IPLACE=INEXT(NREC1)
      END IF
      end do
      RETURN
      END subroutine lnkrow

      end subroutine aa_inverse























