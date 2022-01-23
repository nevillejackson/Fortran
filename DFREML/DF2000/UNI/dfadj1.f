C===========================================================================
      SUBROUTINE  DFADJ1 (nalld,nallp,maxspa)
C===========================================================================

      use sparse
      use kzhz
      use rows
      use like
      use solve
      use means
      use levels
      use units
      use numbers

      integer, intent(in)                :: maxspa
      integer, intent(out)               :: nalld,nallp
      integer, dimension(:), allocatable :: iwork,iivec

c     ---------------------------------------------------------------
c     accumulate adjacency structure of lower triangle in linked list
c     ---------------------------------------------------------------

c     initialize
      rewind(iun52)
      mfix2=count(nlev>0)
      nabs=mfix2+1
      nnabs=nabs+sum(npow(ncov1+1:ncov1+ncov2))
      nall=0
      maxrec=maxspa
      kfirst=0

      allocate( IWORK(0:neqns),IIVEC(nnabs),stat=ii)
      if(ii>0)stop 'alloc dfadj1'

c     -------------
c     ... data part
c     -------------

c     cov * cov : assume filled for all animals -> do only once
      if(nfr1>0)then
         ivec(:nfr1)=(/ (i,i=1,nfr1) /)
         do  i=2,nfr1
         call lnkirw(ivec(i),i-1,nall)
         end do
      end if
      nrc=0

50    read(iun52,end=99)iivec
      nrc=nrc+1
c     collect equation no.s for all effects (written in asc. order by dfmme1)
      nn=nfr1
      do i=1,nnabs
      if(iivec(i)==0)cycle
      irow=iflag( iivec(i) )
      if(irow.eq.0)cycle
      nn=nn+1
      ivec(nn)=irow
      end do
c     add markers for filled cells
      do j=nfr1+1,nn
      call lnkirw(ivec(j),j-1,nall)
      end do
      go to 50

99    nalld=nall
      print *,'data : no. of connections in lower triangle =',nall

c     -----------------
c     ... pedigree part
c     -----------------

      rewind(iun44)
      read(iun44)detl

c     models without "maternal" effect
      IF(IOPRN2.EQ.0.or.nrand2.eq.0)THEN
         READ(IUN44)IC,IR
         IROW=iflag(LIM3+IR)
         JROW=IR
         NN=0
         GO TO 52
51       READ(IUN44,END =59)IC,IR
         if(ic<0)go to 59

52       IF(IR.NE.JROW)THEN
c           ... coefficients for this animal complete
            call lnkirw(irow,nn,nall)
c           ... reset for next animal
            NN=0
            JROW=IR
            IROW=Iflag(LIM3+IR)
         END IF
c        ... accumulate off-diagonal coefficients
         if(ir.ne.ic)then
            NN=NN+1
            ivec(NN)=iflag(LIM3+IC)
         end if
         GO TO 51
c        ... vector for last animal
59       call lnkirw(irow,nn,nall)
         go to 199

      else
c        models including second random effect for each animal
         nn=1
         if(iopcov.eq.2)nn=2
150      read(iun44,end=199)ic,ir
         if(ic<0)go to 199
         icol1=iflag( lim3+ic+ic-1 )
         icol2=iflag( lim3+ic+ic )

c        off-diagonal animal blocks
         if(ir.ne.ic)then
c           ... direct * direct/mat.
            irow1=iflag(lim3+ir+ir-1)
            ivec(1)=icol1
            if(iopcov.eq.2)ivec(2)=icol2
            call lnkirw(irow1,nn,nall)
c           ... mat. * direct/mat.
            if(iopcov.eq.1)then
               ivec(1)=icol2
            else if(iopcov.gt.2)then
               go to 150
            end if
            irow2=iflag(lim3+ir+ir)
            call lnkirw(irow2,nn,nall)
c        diagonal blocks : off-diag. elements for cov(a,m) only
         else if(iopcov.eq.2)then
            ivec(1)=icol1
            call lnkirw(icol2,1,nall)
         end if
         go to 150
      end if

c     model 6 or 10 
199   if(iopcov.eq.4)then
         rewind(iun45)
         nn=1
255      read(iun45,end=299)ic,ir
         if(ic.ne.ir)then
            ivec(1)=iflag(lim3+ic+ic)
            irow2=iflag(lim3+ir+ir)
            call lnkirw(irow2,nn,nall)
         end if
         go to 255
      end if
299   nallp=nall-nalld
      print *,'ped. : no. of connections in lower triangle =',nallp
      print *,'all  : no. of connections in lower triangle =',nall

c     check that there is enough space for complete adjacency structure
      nall2=nall+nall
      CALL CHKLEV(NALL2,MAXSPA,'MAXSPA',
     *                         'ELEMENTS IN FULL ADJACENCY MATRIX',32)
      
c     -------------------------------
c     sort linked list of connections 
c     -------------------------------

      newplc=0
      do irow=1,nsrow
      iplace=kfirst(irow)
      nrow=0
      do while(iplace>0)
      newplc=newplc+1
      icol=kvcol(iplace)
      do while ( (iplace.gt.0) .and. (iplace.lt.newplc) )
         iplace=knext(iplace)
      end do
      jplace=knext(iplace)
      if(iplace.ne.newplc)then
         knext(iplace)=knext(newplc)
         knext(newplc)=iplace
         icol=kvcol(iplace)
         kvcol(iplace)=kvcol(newplc)
         kvcol(newplc)=icol
      end if
      nrow=nrow+1
      iplace=jplace
      end do
      kfirst(irow)=nrow
      end do

      k=1
      do irow=1,nsrow
      n=kfirst(irow)
      kfirst(irow)=k
      k=k+n
      end do
      kfirst(nsrow+1)=k

c     -----------------------------------------------
c     expand  "upper" to a "full" adjacency structure
c     -----------------------------------------------

c     count no. of connections to be added for each row
      iwork=0
      do i=1,nsrow
      do j=kfirst(i),kfirst(i+1)-1
      iwork( kvcol(j) ) =iwork( kvcol(j) ) + 1
      end do
      end do

c     insert spaces for the connections to be added  ...

c     ... total no. to be added is equal to no. in lower triangle
      nn=nall
      k1=nall+1
      kfirst(nsrow+1)=nall2+1

c     ... move portion for one row at a time; in reverse order
      do irow=nsrow,1,-1
c     ... last element for this row
      k2=k1-1
c     ... first element for this row
      k1=kfirst(irow)
c     ... distance to be moved
      nn=nn-iwork(irow)
      kvcol(k1+nn:k2+nn)=kvcol(k1:k2)
c     ... adjust pointer to first element in this row
      kfirst(irow)=k1+nn
c     ... store position of currently last element; overwrite "iwork"
      iwork(irow)=k2+nn
      end do

c     insert "lower" connections; each row only adds elements to rows
c                                                     with a lower no. 
      do  irow=1,nsrow
      do  ij=kfirst(irow),iwork(irow)
      icol=kvcol(ij)
      k=iwork(icol)+1
      kvcol(k)=irow
      iwork(icol)=k
      end do
      end do

      deallocate(iwork,iivec)
      return

      contains

!     ==================================
      SUBROUTINE LNKIRW (irow,nn,nrczhz)
!     ==================================

      IF(NN.EQ.0)RETURN
      KK=0
      IPRE=0
      IPCOL=0
      IPLACE=KFIRST(IROW)
20    KK=KK+1
      ICOL=IVEC(KK)
      IF(ICOL.LE.IPCOL)THEN
         WRITE(*,*)'SUBROUTINE "LNKIRW" : COL. NO.S OUT OF ORDER !'
         WRITE(*,*)' ROW NO. =',IROW
         DO 222 I=1,NN
222      WRITE(*,*)' COLUMN   ',I,IVEC(I)
         STOP
      END IF
      if(icol.ge.irow)print *,'col.gt.row',irow,icol
10    IF( (IPLACE.GT.0) .AND. (KVCOL(IPLACE).LT.ICOL) )THEN
         IPRE=IPLACE
         IPLACE=KNEXT(IPLACE)
         GO TO 10
      ELSE IF( (IPLACE.GT.0) .AND. (ICOL.EQ.KVCOL(IPLACE)) )THEN
         IPRE=IPLACE
         IPLACE=KNEXT(IPLACE)
      ELSE
         NRCZHZ=NRCZHZ+1
         IF(NRCZHZ>MAXREC)STOP '"LNKIRW" : DIMENSIONS EXCEEDED !!'
         KVCOL(NRCZHZ)=ICOL
         IF(IPLACE.EQ.0.AND.KFIRST(IROW).EQ.0)THEN
            KFIRST(IROW)=NRCZHZ
            KNEXT(NRCZHZ)=0
         ELSE IF(IPLACE.EQ.0.AND.KFIRST(IROW).NE.0)THEN
             KNEXT(IPRE)=NRCZHZ
             KNEXT(NRCZHZ)=0
         ELSE IF(IPLACE.NE.0.AND.IPRE.EQ.0)THEN
             KNEXT(NRCZHZ)=KFIRST(IROW)
             KFIRST(IROW)=NRCZHZ
         ELSE
             KNEXT(NRCZHZ)=KNEXT(IPRE)
             KNEXT(IPRE)=NRCZHZ
         END IF
         IPRE=NRCZHZ
         IPLACE=KNEXT(NRCZHZ)
      END IF
      IPCOL=ICOL
      IF(KK.LT.NN)GO TO 20
      RETURN
      END subroutine lnkirw

      END subroutine dfadj1







