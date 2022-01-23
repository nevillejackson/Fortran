!------------------------------------------------------------------------
!     Auxiliary program to DFREML

!     -> extract subset of pedigree records for a given data file

!        data file must be in "DFREML form", formatted (not binary !)
         & with space-separated columns

!        pedigree file must be formatted, with space-separated columns
!        & contain animal, sire and dam ID in columns 2, 3 and 4
!-------------------------------------------------------------km--7/2000-

      module lnklst
         integer, dimension(:), allocatable, save :: inext,ivcol,ifirst,
     *                                               iddvc
         integer, save                            :: maxrec,nan,ifac,
     *                                               idmax,mrow
      end module lnklst

!     ===================
      program extractpeds
!     ====================

      use lnklst

      integer, parameter             :: ipltfm=1 ! 1 =Unix, 2=Lahey PC

      integer                        :: maxan, ibig=2147483646, 
     &                                  nint,iadd=0
      integer                        :: iian,jan,jdad,jmum,kmum,iend,
     &                                  iopmat,ll
      integer, dimension(:), allocatable :: ivec
      character(len=25)              :: fdata, fped, fpedsub='peds.dat'
      character(len=4), dimension(2) :: fmt=(/ '(a )','(a/)' /)
      character(len=6), dimension(2) :: fmt2=(/ '(a,a )','(a,a/)' /)


      write(*,*)'            * * * Program "EXTRACTPEDS" * * *     '
      write(*,*)'Auxiliary program to DFREML to extract subset of '
      write(*,*)'pedigree records for a given data file           '
      write(*,*)' '

      write(*,fmt(ipltfm))'Name of DATA file ? (max 25 char.s)  '
      read(*,'(a)')fdata
      open(1,file=fdata,status='old',form='formatted')
      write(*,*)'File "',trim(fdata),'" has been opened '
      write(*,*)' '
      write(*,'(a)')'Record layout for '
      write(*,'(a)')      '   1  ...  UNIvariate analysis '
      write(*,fmt(ipltfm))'   2  ...  MULTIvariate analysis '
      call optjon(nint,1,2,1)
      nint=nint-1
      write(*,*)'Do records include code for 2nd genetic effect',
     &                                                ' for animals ?'
      write(*,'(a)')'   0  ...  NO  '
      write(*,fmt2(ipltfm))
     &              '   n  ...  YES : code = n-th integer variable',
     &                                                  ' on record'
      call optjon(iopmat,0,99,0)
      if(iopmat>0)then
         write(*,fmt(ipltfm))'Constant added to animal IDs ?'
         call optjon(iadd,0,ibig,10000)
      end if
      write(*,*)' '

      write(*,fmt(ipltfm))'Name of PEDIGREE file ? (max 25 characters) '
      read(*,*)fped
      open(2,file=fped,status='old',form='formatted')
      write(*,*)'File "',trim(fped),'" has been opened '
      write(*,*)' '
      open(3,file=fpedsub,status='unknown',form='formatted')
      write(*,fmt(ipltfm))'Maximum NUMBER of animal IDs ? '
      call optjon(maxan,1,999999,222222)
      write(*,fmt(ipltfm))'Highest animal ID ? '
      call optjon(idmax,1,ibig,ibig)
      write(*,fmt(ipltfm))'Maximum number of pedigree loops allowed ? '
      call optjon(mloop,1,999,99)

      ll=max0(nint+3,iopmat)
      allocate (iddvc(maxan),ifirst(maxan),inext(maxan),ivcol(maxan),
     &  ivec(ll), stat=ii)
      if(ii>0)stop 'Cannot allocate LNKLST'

      nan=0
      maxrec=maxan
      IFAC=(IDMAX/MAXAN +2)
      ifirst=0
      iddvc=0

!     read data file & collect IDS of animals and their parents
      nrec=0
 50   read(1,*,end=99)ivec
      call kount(nrec,5000)
      jan=ivec(nint+1)
      jdad=ivec(nint+2)
      jmum=ivec(nint+3)
      if(iopmat>0)kmum=ivec(iopmat)
      call add_ans
      go to 50
 99   write(*,*)'No. of data file records read  =',nrec
      write(*,*)'No. of animal IDs found so far =',nan
      close(1)

      do iloop=1,mloop
      rewind(2)
      nan0=nan
      nrr=0

 150  read(2,'(i10,3i7)',end=199)iian,jan,jdad,jmum
      nrr=nrr+1
      if(mod(nrr,50000).eq.0)print '(a,i3,2i10)','ped',iloop,nrr,nan
      jan=jan+iadd
      CALL LNKTaB(jan,MDS,1)         ! is animal in list ?
      if(mds>0)then
         IF(jdad.GT.0)then
            jdad=jdad+iadd
            CALL LNKTAB(jdad,MS,0)  ! add sire id to list of animals
         end if
         IF(jmum>0)then
            jmum=jmum+iadd
            CALL LNKTAB(jmum,MD,0)  ! add dam id ...
         end if
      end if
      go to 150
 199  write(*,*)'Pedigree loop no.          =',iloop
      write(*,*)'No. of additional animals  =',nan-nan0
      write(*,*)'Total no. animals now      =',nan
      write(*,*)' '
      if(nan.eq.nan0)exit                      ! no new animals added
      end do
      write(*,*)'end of pedigree loops'

!     collect parental ids and recode
      kkk=0
      n00=0
      rewind(2)
 250  read(2,'(i10,3i7)',end=299)iian,jan,jdad,jmum
      jan=jan+iadd
      CALL LNKtab(jan,ii,1)
      if(ii<1)go to 250 ! not  an animal in the list
      kkk=kkk+1
      if(mod(kkk,25000).eq.0)print *,'Peds out ',kkk
      idad=0
      IF(jdad>0)then
         jdad=jdad+iadd
         CALL LNKtab(jdad,MS,1)
         if(ms>0)idad=iddvc(ms)
      end if
      imum=0
      IF(jmum.GT.0)then
         jmum=jmum+iadd
         CALL LNKtab(jmum,MD,1)
         if(md>0)imum=iddvc(md)
      end if
      if(idad.eq.0.and.imum.eq.0)n00=n00+1
      write(3,'(3i12,i4)')iddvc(ii),idad,imum
      go to 250

 299  write(*,*)'Pedigree file written out                     = ',
     &                                                  trim(fpedsub)
      write(*,*)'No. of records written out                    =',nan
      write(*,*)'No. of animals with unknown parents           =',n00
      if(iopmat>0)then
      write(*,*)'Constant added to IDs                         =',iadd
      write(*,*)'No. of dummy IDs for 2nd animal code assigned =',ndam
      end if
      write(*,*),'No. of pedigree loops carried out            =',iloop
 
      contains

!     ===================
      subroutine add_ans
!     ==================

!     add animal IDs to pedigree list, incl. dummy IDs for missing parents

!      jan=jan+iadd
      CALL LNKTAB(jan,jj,0)
      if(jdad>iadd)then
!         jdad=jdad+iadd
         call lnktab(jdad,mds,0)
      end if
      if(jmum>iadd)then
!         jmum=jmum+iadd
         CALL LNKTAB(jmum,MDS,0)
      else if(jmum.eq.0)then
         ndam=ndam+1           ! dummy dam ID for maternal effects
         jmum=ndam
      end if
      if(iopmat.eq.0)return
      if(kmum<1)then
         ndam=ndam+1
         kmum=ndam
      else
!         kmum=kmum+iadd
         CALL LNKTAB(kmum,MDS,0)
      end if
      return
      end subroutine add_ans

!     ========================================
      SUBROUTINE OPTJON(IOPT,IOMIN,IOMAX,idef)
!     ========================================

      integer, intent(in)         :: iomin,iomax, idef
      integer, intent(out)        :: iopt
      CHARACTER(len=20)           :: a 
      CHARACTER(len=7)            :: blnk='       '
      real(8)                     :: XOPT

2     READ(*,'(a)')A
      if(a(1:7).eq.blnk)then
         iopt=idef
      else
         CALL CHKDIG(A,IOPT,XOPT,20,1)
         IF( (IOPT.LT.IOMIN) .OR. (IOPT.GT.IOMAX) )THEN
            WRITE(*,1)'INVALID OPTJON GIVEN - PERMISSIBLE RANGE IS',
     *                IOMIN,'TO',IOMAX
            WRITE(*,1)'VALUE SPECIFIED WAS',IOPT
            WRITE(*,*)'TRY AGAIN ...'
            WRITE(*,*)'OPTJON ?'
            GO TO 2
1           FORMAT(1X,A,I5,3X,A,I6,' !!')
         END IF
      END IF
      RETURN
      END subroutine optjon

      end program extractpeds
    
C=======================================================================
      SUBROUTINE LNKTAB(IDS,NDS,iopt)
C=======================================================================

      use lnklst,nplace=>nan,idvec=>iddvc

      integer, intent(in)  :: ids, iopt
      integer, intent(out) :: nds

      NDS=0
      IF(IDS<1)RETURN

      IF(IDS.GT.IDMAX)THEN
         WRITE(*,*)'ID GIVEN IS GREATER THAN MAXIMUM SPECIFIED !'
         WRITE(*,*)'MAX. WAS =',IDMAX,'  CURRENT ID IS =',IDS
         STOP 'SUBROUTINE "LNKTAB"'
      END IF

C     SPLIT ID INTO "ROW" AND "COLUMN"
      IROW=IDS/IFAC+1
      ICOL=MOD(IDS,IFAC)
      IF(IROW.GT.MROW)MROW=IROW

      IPRE=0
      IPLACE=IFIRST(IROW)

C     GO TO NEXT ELEMENT IN THIS ROW
10    IF((IPLACE.GT.0).AND.(IVCOL(IPLACE).LT.ICOL))THEN
         IPRE=IPLACE
         IPLACE=INEXT(IPLACE)
         GO TO 10

C     ELEMENT FOUND
      ELSE IF((IPLACE.GT.0).AND. (ICOL.EQ.IVCOL(IPLACE)) )THEN
C        IF(IDVEC(IPLACE).NE.IDS)STOP 'ERROR : IDS DO NOT AGREE !'
         NDS=IPLACE

C     ELEMENT IROW,ICOL HAS NOT BEEN ACCESSED BEFORE
      ELSE if(iopt.eq.1)then
         return
      else
         NPLACE=NPLACE+1
         IF(NPLACE.GT.MAXREC)STOP '"LNKTAB" : DIMENSIONS EXCEEDED !'
         IDVEC(NPLACE)=IDS
         IVCOL(NPLACE)=ICOL
         NDS=NPLACE
C        FIRST ELEMENT IN THIS ROW
         IF(IPLACE.EQ.0.AND.IFIRST(IROW).EQ.0)THEN
            IFIRST(IROW)=NPLACE
            INEXT(NPLACE)=0
C        ALL PREVIOUS ELEMENTS IN THE ROW HAVE LOWER COLUMN NO.
         ELSE IF(IPLACE.EQ.0.AND.IFIRST(IROW).NE.0)THEN
            INEXT(IPRE)=NPLACE
            INEXT(NPLACE)=0
C        FIT ELEMENT IN "RIGHT" PLACE, SWAPPING INDICATORS
         ELSE IF(IPLACE.NE.0.AND.IPRE.EQ.0)THEN
            INEXT(NPLACE)=IFIRST(IROW)
            IFIRST(IROW)=NPLACE
         ELSE
            INEXT(NPLACE)=INEXT(IPRE)
            INEXT(IPRE)=NPLACE
         END IF
      END IF
      RETURN
      end subroutine lnktab



















