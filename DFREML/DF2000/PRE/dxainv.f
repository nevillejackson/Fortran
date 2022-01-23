!=======================================================================
      subroutine a_inverse(nanim, iopbas,iopibr,ninbr,detl,
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

      real(8)                              :: d
      integer                              :: nafix,narnd,mzhz,iowrt=0,
     &                                        kafix,karnd,kanim,ii
      integer, dimension(:), allocatable   :: iprev, itype
      logical                              :: lexist
      character(len=8)                     :: fname='DF44#DAT'

      DETL=0.D0
      NINBR=0    ! count(mask(u>1.d0))
      SFVAL=0.d0 ! sum(u,mask=u>1.d0)
      NAFIX=0
      NARND=0
      allocate(u(0:nanim2),ivec(nanim),itype(nanim2),stat=ii)
      if(ii>0)stop 'alloc nrm'
      u=0.d0
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
                if(nainv>1)DETL=0.D0
                return
            end if
         end if
      end if

!     -----------
!     NRM inverse
!     -----------

!     allocate arrays
      mzhz=nanim2*100
      write(*,*)'Max. no. of non-zero elements in NRM inverse ?'
      call optdef(mzhz,nanim2,99999999,mzhz)
      allocate(zhz(mzhz),ifirst(nanim2),inext(mzhz),ivcol(mzhz),
     &         iprev(mzhz),rvec(nanim),stat=ii)
      if(ii>0)then
         write(*,*)'could not allocate arrays required to set up NRM'
         stop 'alloc nrm 2'
      end if
      
      if(iopibr.eq.1)then
         allocate(ng(0:llev),sg(0:llev),stat=ii)
         if(ii>0)stop 'alloc g groups'
         ng=0
         sg=0.d0
      end if

!     determine elements of A to be calculated
      ifirst=0
      nrczhz=0
      do ia=1,nanim                           ! forwards loop : cov(parents)
      if(mod(ia,5000).eq.0)print *,ia,nrczhz
      ipos=0
      call iadd(idsire(ia),iddam(ia),ipos)
      u(ia)=ipos
      end do
      print *,'begin backward loop ..'
      do irow=nanim,1,-1                        ! backward loop
      if(mod(irow,5000).eq.0)print *,irow,nrczhz
      ij=ifirst(irow)
      do while (ij>0)
      icol=ivcol(ij)
      if(idsire(icol)>0)call iadd(irow,idsire(icol),ipos)
      if(iddam(icol)>0)call iadd(irow,iddam(icol),ipos)
      ij=iprev(ij)
      end do
      end do
      print *,'end backward loop',nrczhz

!     reverse linked list
      do irow=1,nanim
      ij=ifirst(irow)
      if(ij.eq.0)cycle
      inext(ij)=0
 2    ipre=iprev(ij)
      if(ipre>0)then
         inext(ipre)=ij
         ij=ipre
         go to 2
      else
         ifirst(irow)=ij
      end if
      end do
      deallocate(iprev,stat=ii)
      if(ii>0)stop 'deall iprev'

!     calculate inbreeding coefficients
      do irow=1,nanim
      if(mod(irow,5000).eq.0)print *,'F',irow
!     diagonal
      ipos=u(irow)
      if(ipos.eq.0)then
         u(irow)=1.d0
      else
         u(irow)=1.d0+zhz(ipos)*0.5d0
      end if

!     selected off-diagonals
      ij=ifirst(irow)
      do while (ij>0)
         icol=ivcol(ij)
         zhz(ij)=0.d0
         if(idsire(icol)>0)call find(zhz(ij),irow,idsire(icol))
         if(iddam(icol)>0)call find(zhz(ij),irow,iddam(icol))
         zhz(ij)=zhz(ij)*0.5d0
         ij=inext(ij)
      end do

      end do  ! irow

!     calculate log determinant of NRM
      detl=0.d0
      do ian=1,nanim
      IDAD=IDSIRE(ian)
      IMUM=IDDAM(ian)
      DETL=DETL+DLOG(1.d0-0.25d0*(u(idad)+u(imum)) )
      end do
      detl=0.5d0*detl

!     set up NRM inverse
      rewind(iun44)

      do iainv=1,nainv
      ifirst=0        ! zero out arrays ...
      nrczhz=0      
      nrec1=0

      do ian=1,nanim
      if(itype(ian).ne.iainv)cycle
      IDAD=IDSIRE(ian)
      IMUM=IDDAM(ian)
      NARND=NARND+1
      d=1.d0/(1.d0-0.25d0*u(idad)-0.25d0*u(imum) )
      I1=MIN0(IDAD,IMUM)
      I2=MAX0(IDAD,IMUM)
      LL=0
      IF(I1.GT.0)THEN
         LL=LL+1
         IVEC(LL)=I1
         RVEC(LL)=0.25d0*D
         CALL LNKROW(I1,LL)
      END IF
      IF(I2.GT.0)THEN
         LL=LL+1
         IVEC(LL)=I2
         RVEC(LL)=0.25d0*d
         CALL LNKROW(I2,LL)
      END IF
      RVEC(:ll)=-0.5d0*d
      LL=LL+1
      RVEC(LL)=D
      IVEC(LL)=IAN
      CALL LNKROW(IAN,LL)
      end do

!     WRITE OUT ELEMENTS TO UNIT "44"
      if(iainv.eq.1)WRITE(IUN44)DETL,IOWRT,NANIM,NAFIX,NARND,
     &                                      NAMOD,nainv,nna(:nainv)
      DO IROW=1,NANIM
      call acc_inbreed(u(irow))
      ij=ifirst(irow)
      do while(ij>0)
         write(iun44)ivcol(ij),irow,zhz(ij),iainv
         ij=inext(ij)
      end do
      end do ! irow
      end do ! iainv
      close(iun44)

C     COUNT NO. OF ANIMALS IN MODEL OF ANALYSIS
      NAMOD=count (mask=(ifirst>0) )

      IF(IOPBAS.GT.0)THEN
         WRITE(*,111)'NO. OF ANIMALS TREATED AS RANDOM',NARND
         WRITE(*,111)'NO. OF ANIMALS TREATED AS FIXED',NAFIX
      END IF
      WRITE(*,111)'NO. OF "EFFECTIVE" ANIMALS IN THE ANALYSIS',NAMOD
      WRITE(*,111)'NO. OF NON-ZERO ELEMENTS OF NRM INVERSE ',NREC1
      WRITE(*,112)'LOG DETERMINANT OF NRM',(DETL+DETL)

      XINBR=(SFVAL+NANIM2-NINBR)/NANIM2
      IF(NINBR.GT.1)SFVAL=SFVAL/NINBR
      WRITE(*,111)'NO. OF INBRED ANIMALS ',NINBR
      IF(NINBR.GT.0)WRITE(*,112)
     *             '... WITH AVERAGE INBREEDING COEFFICIENT',SFVAL
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

!     ===========================
      subroutine iadd(ii,jj,ipos)
!     ===========================

!     start at end of row, point backwards

      integer, intent(in)    :: ii,jj
      integer, intent(inout) :: ipos
      integer                :: irow,icol,inxt,ij

      if(ii.eq.0.or.jj.eq.0.or.ii.eq.jj)return
      irow=min0(ii,jj)
      icol=max0(ii,jj)
 
      ij=ifirst(irow)
      inxt=0
      do while( ij>0 .and. ivcol(ij)> icol )
         inxt=ij
         ij=iprev(ij)
      end do
      if( ij>0 .and. icol.eq.ivcol(ij) )then
         ipos=ij
         return                           ! element already flagged
      else
         nrczhz=nrczhz+1
         if(nrczhz>mzhz)then
            print *,irow,icol
            stop '"lnkirw" : dimensions exceeded !!'
         end if
         ipos=nrczhz
         ivcol(nrczhz)=icol
         if(ij.eq.0.and.ifirst(irow).eq.0)then ! 1st element in row
            ifirst(irow)=nrczhz
            iprev(nrczhz)=0
         else if(ij.eq.0.and.ifirst(irow)>0)then ! new last element 
             iprev(inxt)=nrczhz
             iprev(nrczhz)=0
         else if( ij>0 .and.inxt.eq.0)then ! new 1st element
             iprev(nrczhz)=ifirst(irow)
             ifirst(irow)=nrczhz
         else
             iprev(nrczhz)=iprev(inxt)   ! slot in right order
             iprev(inxt)=nrczhz
         end if
      end if
      return
      end subroutine iadd

!     =========================
      subroutine find (x,ii,jj)
!     =========================

      real(8), intent(inout) :: x
      integer, intent(in)    :: ii,jj
      integer                :: irow,icol,ij

      if(ii.eq.jj)then
          x=x+u(ii)
          return
      end if

      irow=min0(ii,jj)
      icol=max0(ii,jj)
 
      ij=ifirst(irow)
      do while(ij>0)
         if(ivcol(ij).eq.icol)then
            x=x+zhz(ij)
            return
         end if
         ij=inext(ij)
      end do
      print *,'err : el not found',ii,jj
      stop '"find"'
      end subroutine find

!     =========================
      subroutine acc_inbreed(uu)
!     =========================

      real(8), intent(in)  :: uu

      IF(UU>1.d0)THEN
          NINBR=NINBR+1
          SFVAL=SFVAL+UU
      END IF
      IF(IOPIBR.EQ.1)THEN
         IGEN=IGVEC(IROW)
         NG(IGEN)=NG(IGEN)+1
         SG(IGEN)=SG(IGEN)+UU
      END IF
      return
      end subroutine acc_inbreed

      end subroutine a_inverse























