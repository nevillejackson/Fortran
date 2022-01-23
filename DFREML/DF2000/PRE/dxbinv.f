!=======================================================================
      subroutine b_inverse(nanim,ninbr,detl,xinbr,sfval)
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

      integer, intent(in)                  :: nanim
      integer, intent(out)                 :: ninbr
      real(8), intent(out)                 :: xinbr, sfval,detl

      real(8)                              :: d,d4
      integer                              :: nafix,narnd,mzhz,iowrt=0,
     &                                        kafix,karnd,kanim,ii

      character(len=8)                     :: fname='DF44#DAT'
      real(8)                              :: X1611=16.D0/11.D0,
     &                                        XM811=-8.D0/11.D0,
     &                                        XP411=4.D0/11.D0,
     &                                        XM411=-4.D0/11.D0,
     &                                        XP211=2.D0/11.D0,
     &                                        XP111=1.D0/11.D0,
     &                                        X43=4.D0/3.D0,
     &                                        XM23=-2.D0/3.D0,
     &                                        X13=1.D0/3.D0,
     &                                        X1615=16.D0/15.D0,
     &                                        XM415=-4.D0/15.D0,
     &                                        X115=1.D0/15.D0

      allocate(u(0:nanim2),ivec(nanim),stat=ii)
      if(ii>0)stop 'alloc nrm'
      u=0.d0
      ivec=0

 944  mzhz=nanim2*15
      print *,'mzhz=',mzhz,nanim2
      allocate(zhz(mzhz),ifirst(nanim2),inext(mzhz),ivcol(mzhz),
     &         rvec(nanim),stat=ii)
      if(ii>0)stop 'alloc nrm 2'
      
      DETL=0.D0
      NINBR=0    ! count(mask(u>1.d0))
      SFVAL=0.d0 ! sum(u,mask=u>1.d0)
      NAFIX=0
      NARND=0

!     set up NRM inverse
      nrczhz=0
      nrec1=0
      ifirst=0
      do ian=1,nanim
      IDAD=IDSIRE(ian)
      IMGS=IDDAM(ian)
      NARND=NARND+1
      IF(IDAD>0 .AND.IMGS>0 )THEN                  ! SIRE & MGS KNOWN
         ivec(1)=imgs
         ivec(2)=idad
         ivec(3)=ian
         rvec(1)=xp111
         CALL LNKrow(IMGS,1)
         rvec(1)=xp211
         rvec(2)=xp411
         call lnkrow(idad,2)
         rvec(1)=xm411
         rvec(1)=xm811
         rvec(1)=x1611
         call lnkrow(ian,3)
      ELSE IF(IDAD.GT.0.AND.IMGS.EQ.0)THEN
C        SIRE KNOWN, MGS UNKNOWN
         ivec(1)=idad
         ivec(2)=ian
         rvec(1)=x13
         call lnkrow(idad,1)
         rvec(1)=xm23
         rvec(2)=x43
         call lnkrow(ian,2)
      ELSE IF(IDAD.EQ.0.AND.IMGS>0)THEN          !  sire unknown, mgs known
         ivec(1)=imgs
         ivec(2)=ian
         rvec(1)=x115
         CALL LNKrow(IMGS,1)
         rvec(1)=xm415
         rvec(2)=x1615
         call lnkrow(ian,2)
      ELSE
         ivec(1)=ian
         rvec(1)=1.d0
         CALL LnKrow(IAN,1)
      END IF

      end do ! ian

      NAMOD=count (mask=(ifirst>0) )
      detl=0.d0                                ! do not evaluate det|a|

C    -------------------------------
C    WRITE OUT ELEMENTS TO UNIT "44"
C    -------------------------------

      WRITE(*,111)'NO. OF NON-ZERO ELEMENTS OF NRM INVERSE ',NREC1

      rewind(iun44)
      WRITE(IUN44)DETL,IOWRT,NANIM,NAFIX,NARND,NAMOD,1,1

      DO IROW=1,NANIM
      UU=U(IROW)
      IF(UU>1.d0)THEN
          NINBR=NINBR+1
          SFVAL=SFVAL+UU
      END IF
      IJ=IFIRST(IROW)
      do while(ij>0)
         WRITE(IUN44)IVCOL(IJ),IROW,ZHZ(IJ),1
         IJ=INEXT(IJ)
      end do
      end do ! irow

      XINBR=(SFVAL+NANIM2-NINBR)/NANIM2
      IF(NINBR.GT.1)SFVAL=SFVAL/NINBR

      deallocate (zhz,ifirst,inext,ivcol,rvec,stat=ii)
      if(ii>0)stop 'dealloc nrm'
      close(iun44)

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

      end subroutine b_inverse






















