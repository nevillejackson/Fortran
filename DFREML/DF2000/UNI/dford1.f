C===========================================================================
      SUBROUTINE DFORD1 
C===========================================================================

      use sparse
      use kzhz
      use lnk
      use diagonal, only : dia
      use order
      use rows
      use like
      use solve, only : krzero
      use levels
      use units
      use numbers

      integer, dimension(:), allocatable     :: iwork,iposfx,nnvec
      real(8)                                :: xsecs,time
      integer                                :: isecs

      IF(IORDER.NE.0 )CALL DFERR3(IORDER,'DFORD1')
      CALL DFTIME(time,xsecs,isecs,0,'DFORD1')

      iorder=3
      lim0=nq
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

      print *,'"dford1" : new no. of rows                  =',nsrow
      
      write(*,*)'give value for "MAXSPA"  '
      m1=nsrow*50
      call optdef(maxspa,1,29999999,m1)

      allocate( KFIRST(neqns),KVCOL(maxspa),KNEXT(maxspa),stat=ii)
      if(ii>0)stop 'alloc dford1'
      allocate(ivperm(nsrow+1),stat=ii)
      allocate(ixvec1(nsrow+2),ixvec2(nsrow+2),ixsub(maxspa),stat=ii)      
      if(ii>0)stop 'alloc dfadj1'
      allocate(iwork(0:neqns),IPOSFX(nfix1), nnvec(neqns),stat=ii)
      if(ii>0)stop 'alloc dford1'
      ivperm=0
      ivec=0

C     SET UP ADJACENCY STRUCTURE OF COEFFICIENT MATRIX 
      CALL DFADJ1(NALLD,NALLP,maxspa)

      na=kfirst(nsrow+1)-1
      knext(:na)=kvcol(:na)

!     look for new order set up in previous, incomplete run
      open(iun49,file='DF49#DAT',form='unformatted',status='unknown')
      read(iun49,end=4099,err=4099)ksrow,keqns
      if(ksrow.eq.nsrow .and. keqns.eq.neqns )then
         print *,'found file "49" - use info ? ',ksrow,keqns
         call yndef(ii,1)
         if(ii.eq.1)then
            read(iun49)iflag
            read(iun49)ivec
            read(iun49)ivperm
            nofsub=0
            go to 4199
         end if
      end if

 4099 idelta=0
      IBIG=1234567890
      mkvcol=size(kvcol)
      call ggenmmd(nsrow,kfirst,knext,ivec,ivperm,idelta,ibig,nofsub,
     *                                                          mkvcol)
      print *,'end of genmmd',nsrow,'  nofsub=',nofsub

c     write out info on re-ordering
      rewind(iun49)
      write(iun49)nsrow,neqns
      write(iun49)iflag
      write(iun49)ivec
      write(iun49)ivperm
      close(iun49)

c      ... carry out symbolic factorisation & set up compressed
c                                                storage scheme     
 4199 maxsub=maxspa
      maxlnz=0

      call smbfct(nsrow,kfirst,kvcol,ivperm,ivec,ixvec1,maxlnz,
     *            ixvec2,ixsub,maxsub,knext,ieqnew,nnvec,ierror)

      print *,'maxsub',maxsub
      print *,'maxlnz',maxlnz

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

c     set up vector of new equation no.s
      ieqnew=0
      do i=lim0+1,neqns
      j=iflag(i)
      if(j.gt.0)ieqnew(i)=ivec(j)
      end do
      nfill=999999

c     count no. of off-diagonals per row
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'DISTRIBUTION OF NO.S OF NON-ZERO OFFDIAGONALS'
      WRITE(IUN66,*)'---------------------------------------------'
      nnvec=0
      do icol=1,nsrow
      ksub=ixvec2(icol)
      do k=ixvec1(icol),ixvec1(icol+1)-1
      nnvec(ixsub(ksub))=nnvec(ixsub(ksub))+1
      ksub=ksub+1
      end do
      end do
      iwork=0
      do i=1,nsrow
      iwork(nnvec(i))=iwork(nnvec(i))+1
      end do
      nnall=0
      call dfwdis(iwork,nsrow,10,nlow,nup,nnall,iun66)
9000  Format(1x,'RE-ORDER EQUATIONS & CALCULATE FILL-IN'/
     *       8x,'0  ...  CONSIDER "STANDARD" ORDER ONLY '/
     *       8x,'1  ...  RE-ORDER ACCORDING TO NUMBER OF NON-ZERO'/
     *       8x,'        OFF-DIAGONAL ELEMENTS PER ROW '/
     *       8x,'2  ...  RE-ORDER AS SPECIFIED BY USER '/
     *       8x,'        (requires new row no.s as input from "12")'/
     *       8x,'3  ...  RE-ORDER using Minimum Degree algorithm'/
     *       8x,'         --> Symmetric Factorization; SPARSPAK')
9     FORMAT(8X,A)
11    FORMAT(1X,A,T45,'=',I15)
12    FORMAT(1X,A,T40,I8,2X,'TO',I8)
13    FORMAT(3I10)
14    FORMAT(1X,A,T45,'=',F15.1)

C     --------------------------------
C     RECODE DATA TO NEW EQUATION NO.S
C     --------------------------------

5000  REWIND(IUN22)
      REWIND(IUN52)

      CALL DFTIME(time,xsecs,isecs,14,'DFORD1')
      K=LIM1
      DO I=1,NFIX1
      IPOSFX(I)=K
      K=K+NLEV(I)
      end do

      CALL DFMME1(IPOSFX)
      RETURN

      END subroutine dford1
