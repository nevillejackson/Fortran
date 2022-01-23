C===========================================================================
      SUBROUTINE DVORD3 (nparm,parvec)
C===========================================================================

      use params
      use units
      use sparse
      use levels
      use numbers
      use adjacency
      use order
      use like_components
      use zero_rows

!     arguments
      integer, intent(in)                     :: nparm
      real(8), dimension (mxparm), intent(in) :: parvec

!     local variables
      integer, dimension(:),allocatable :: iwork,nnvec,invprm
      integer, dimension (maxfix,maxnq) :: iposfx
      real(8)                           :: time,xsecs
      integer :: ii,lim0,nalld,nallp,idelta,ibig,ierror,iq,i,j,k,mxspaa
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      IF(IORDER.NE.0)CALL DFERR3(IORDER,'DFORD3')
      CALL DFTIME(time,xsecs,isecs,0,'DVORD3')

      LIM0=1
      IORDER=4
      allocate (iwork(0:neqns),nnvec(neqns),kfirst(neqns+1),stat=ii)
      if(ii>0)stop 'dvord3 : alloc IWORK'
      write(*,*)'give value for "MAXSPA" (halfstored)  '
      ii=(neqns/10+1)*250
      call optdef(mxspaa,1,999999999,ii)
      allocate(ixsub(mxspaa),kvcol(mxspaa+mxspaa),stat=ii)
      if(ii>0)stop 'dvord3 : alloc KFIRST,KVCOL'

c     zero out rows & cols. for p.d. coefficient matrix : `mapping' 
c     between original equation no.s & new row no.s

      iflag=0
      nsrow=0
      do 1 i=lim0+1,neqns
      if(i.gt.lim1 .and. i.le.lim2)then
         do j=1,nrzero
         if(krzero(j).eq.i)go to 1
         end do
      end if
      nsrow=nsrow+1
      iflag(i)=nsrow
1     continue

      print *,'"dvord3" : new no. of rows                  =',nsrow

!     allocate arrays depending on nsrow
      allocate(ixvec1(nsrow+2),ixvec2(nsrow+2),stat=ii)
      if(ii>0)stop 'dvord3 : alloc ixvec'
      allocate(ivperm(nsrow+1),invprm(nsrow+1),stat=ii)
      if(ii>0)stop 'dvord3 : alloc iperm'
      invprm=0
      ivperm=0

!     look for new order set up in previous, incomplete run
      open(iun49,file='DF49#DAT',form='unformatted',status='unknown')

      read(iun49,end=99,err=99)ksrow,keqns
      if(ksrow.eq.nsrow .and. keqns.eq.neqns )then
         WRITE(*,'(a,2i8)')' found file "49" - use info ? ',ksrow,keqns
         call yndef(ii,1)
         if(ii.eq.1)then
            read(iun49)iflag
            read(iun49)invprm
            read(iun49)ivperm
            nofsub=0
            go to 199
         end if
      end if
    
C     SET UP ADJACENCY STRUCTURE OF COEFFICIENT MATRIX 
 99   CALL DxADJ3(PARVEC,NALLD,NALLP,NPARM,0,mxspaa)

C     DETERMINE MINIMUM DEGREE ORDERING 
      IDELTA=0
      IBIG=1234567890

      mkvcol=size(kvcol)
      call ggenmmd(nsrow,kfirst,kvcol,invprm,ivperm,idelta,ibig,nofsub,
     *                                                          mkvcol)

      print *,'end of genmmd',nsrow,'  nofsub=',nofsub

c     write out info on re-ordering
      rewind(iun49)
      write(iun49)nsrow,neqns
      write(iun49)iflag
      write(iun49)invprm
      write(iun49)ivperm
      close(iun49)

C     REBUILD ADJACENCY STRUCTURE ADDING EXTRA ROW/COL
 199  NSROW=NSROW+1
      print *,'dxa',nsrow,mxspaa,nparm,1
      CALL DxADJ3(PARVEC,NALLD,NALLP,NPARM,1,mxspaa)
      ivperm(nsrow)=nsrow
      invprm(nsrow)=nsrow

C     CARRY OUT SYMBOLIC FACTORISATION & SET UP COMPRESSED  STORAGE SCHEME     
      maxsub=mxspaa
      maxlnz=0
      ixvec1=0  
      ixvec2=0
      ixsub=0

      call smbfct(nsrow,kfirst,kvcol,ivperm,invprm,ixvec1,maxlnz,
     *            ixvec2,ixsub,maxsub,iwork,ieqnew,nnvec,ierror)

      print *,'end of smbfct : maxsub',maxsub
      print *,'                maxlnz',maxlnz

      if(ierror.gt.0)then
         write(*,*)'routine "SMBFCT" : too many subscripts !'
         write(*,*)'max. allowed   =',mxspaa
         write(*,*)'no. required   =',maxsub
         stop 'reset parameter "maxspa" !'
      end if

      deallocate(kvcol,stat=ii)
      if(ii>0)stop 'de-alloc : kvcol'

c     set up vector of new equation no.s
      ieqnew=0 ! array
      do  i=lim0+1,neqns
      j=iflag(i)
      if(j.gt.0) ieqnew(i)=invprm(j)
      end do
      nfill=999999
     
!     write out summary to unit "66"
      call wr_summary

C     --------------------------------
C     RECODE DATA TO NEW EQUATION NO.S
C     --------------------------------

      REWIND(IUN22)
      REWIND(IUN52)
      CALL DFTIME(time,xsecs,isecs,14,'DVORD3')

      K=LIM1
      DO IQ=1,NQ
      DO I=1,NFIX(IQ)
      IPOSFX(I,IQ)=K
      K=K+NLEV(I,IQ)
      END do
      end do
      CALL DFMME3(IPOSFX)

      RETURN

      contains

c     =====================
      subroutine wr_summary
c     =====================

      write(iun66,*)' '
      write(iun66,*)'-----------------------------------------'
      write(iun66,*)'characterictics of symbolic factorisation'
      write(iun66,*)'-----------------------------------------'
      write(iun66,*)' '
      write(iun66,11)'no. of equations in mmm',neqns
      write(iun66,11)'no. of rows in p.d. coefficient matrix',nsrow
      write(iun66,11)'no. non-zero off-diagonals : data',nalld
      write(iun66,11)'                           : pedigree',nallp
      write(iun66,11)'                           : total',nalld+nallp
      write(iun66,11)'no. of elements in adjacency matrix',
     *                                            kfirst(nsrow+1)-1
      write(iun66,*)' '
      write(iun66,*)'ordering strategy : minimum degree '
      write(iun66,*)'storage scheme    : compressed '
      write(iun66,11)'no. of subscripts encountered (nofsub) ',nofsub
      write(iun66,11)'no. of subscripts used in storage (maxsub)',
     *                                                         maxsub
      write(iun66,11)'no. of non-zero elements in fact. (maxlnz)',
     *                                                         maxlnz

      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'DISTRIBUTION OF NO.S OF NON-ZERO OFFDIAGONALS'
      WRITE(IUN66,*)'---------------------------------------------'

      iwork=0 ! array
      nnvec=0

c     count no. of off-diagonals per row
      do icol=1,nsrow
      ksub=ixvec2(icol)
      do k=ixvec1(icol),ixvec1(icol+1)-1
      nnvec(ixsub(ksub))=nnvec(ixsub(ksub))+1
      ksub=ksub+1
      END do
      END do

      do i=1,nsrow
      iwork(nnvec(i))=iwork(nnvec(i))+1
      END do
      nnall=0

      call dfwdis(iwork,nsrow,10,nlow,nup,nnall,iun66)
      return
      
11    FORMAT(1X,A,T45,'=',I15)

      end subroutine wr_summary

      END subroutine dvord3
