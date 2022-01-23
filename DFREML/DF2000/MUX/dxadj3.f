C===========================================================================
      SUBROUTINE  DxADJ3 (parvec,nalld,nallp,nparm,ixopt,mxspaa)
C===========================================================================

      use params
      use units
      use sparse
      use combinations
      use levels
      use numbers
      use adjacency
      use like_components
      use residuals

!     arguments
      real(8), dimension(mxparm), intent(in) :: parvec
      integer, intent(out)                   :: nalld,nallp
      integer, intent(in)                    :: nparm, ixopt,mxspaa

!     local variables
      logical, dimension(:), allocatable     :: lll,iirow
      logical, dimension(:,:), allocatable   :: iiqhq
      logical, dimension(:,:,:), allocatable :: iieei
      integer, dimension(0:neqns)            :: iwork
      integer, dimension(neqns)              :: ivec, iicol
      real(8)                                :: fvalue
      integer, dimension (:), allocatable    :: nnq
      integer, dimension (:,:), allocatable  :: ieq
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate (knext(mxspaa),stat=ii)
      if(ii>0)stop 'dxadj3 : alloc KNEXT'
      allocate (nnq(mobs),ieq(maxfix+3,mobs),stat=ii)
      if(ii>0)stop 'dxadj3 : alloc 2'
      allocate( iieei(mobs,mobs,ncomb),iiqhq(lim1+mqhq,lim1+mqhq),
     &                            lll(neqns),iirow(neqns),stat=ii)
      if(ii>0)stop 'dxadj3 : alloc 3'

      LIM0=1
      if(ixopt.eq.1)lll=.false.

c     ---------------------------------------------------------------
c     accumulate adjacency structure of lower triangle in linked list
c     ---------------------------------------------------------------

C     DETERMINE STRUCTURE OF INVERSE RESIDUAL COV. MATRICES
      kfit(5:6)=nq
      CALL DxRES3 (PARVEC,FVALUE,NPARM,IOUT,0)
      if(iout.eq.1)stop 'dxadj3'

      iieei=.false.
      where (eei .ne.0) IIEEI = .true.

C     -------------
C     ... DATA PART
C     -------------

C     INITIALIZE
      if(ixopt.eq.1)rewind(iun52)
      NALL=0
      MAXREC=MXSPAa
      kfirst=0
      NRC=0

50    READ(IUN52,END=99)ICOMB,NOBS,NR,NNQ(:NOBS),
     *    IICOL(:NR),( (IEQ(K,L),K=1,NEFF(NNQ(L)) ),L=1,NOBS)

      nrc=nrc+1
      if(mod(nrc,1000).eq.0)print *,'nrc =',nrc,nall

      LIMNR1=LIM1+NR
      iiqhq=.false.

C     ACCUMULATE CONTRIBUTIONS TO MMM FOR ONE OBSERVATION AT A TIME ...
C     ... SET UP ROW OF X'R(-1) FOR THIS OBSERVATION
      DO 515 IOBS=1,NOBS
      IQ=NNQ(IOBS)
      II=NFRST(IQ)
      iirow(:limnr1)=.false.
      DO JOBS=1,NOBS
      IF( IIEEI(JOBS,IOBS,ICOMB) )THEN
         JQ=NNQ(JOBS)
         JJ=NFRST(JQ)
         DO J=1,NFR(JQ)
         IIROW(JJ+J)=.true.
         end do
         DO J=1,NEFF(JQ)
         IIROW(IEQ(J,JOBS))=.true.
         end do
      END IF
      end do  

C     MARK FILLED OFF-DIAGONAL CELLS : COV * COV PART OF MMM
      DO K=1,NFR(IQ)
      IROW=II+K
      NN=0
      DO JROW=LIM0+1,IROW-1
      IF(IIROW(JROW) )THEN
         NN=NN+1
         IVEC(NN)=IFLAG(JROW)
      END IF
      end do
      CALL LNKIRW(IFLAG(IROW),NN,IVEC,NALL,MAXREC)
      end do

C     mark non-zero contributions due to FE/RE for this record
      DO K=1,NEFF(IQ)
      JROW=IEQ(K,IOBS)-LIM1
      do l=1,limnr1
      if( iirow(l) )iiqhq(l,jrow)=.true. !do not change!
      end do
      end do

515   CONTINUE

C     ALL RECORDS FOR THE ANIMAL PROCESSED, recode to eq.nos
      DO 529 I=1,NR
      IROW=IICOL(I)
      if(iflag(irow).eq.0)go to 529
      NN=0
c     ... regression coefficients
      DO J=lim0+1,LIM1
      IF( IIQHQ(J,I) )THEN
          NN=NN+1
          IVEC(NN)=iflag( j )
      END IF
      end do
c     ... FE/RE codes, lower triangle off-diag.s only
      DO J=1,NR
      IF((IICOL(J).lt.IROW) .AND. IIQHQ(LIM1+J,I) )THEN
          jj=iflag( IICOL(J) )
          if(jj.ne.0)then
             NN=NN+1
             IVEC(NN)=jj
          end if
      END IF
      end do

      call lnkirw(iflag(irow),nn,ivec,nall,maxrec)

c     ... contributions to rhs
      if(ixopt.eq.1)then
         nn=nn+1
         ivec(nn)=iflag(irow)
         do k=1,nn
         lll( ivec(k) )=.true.
         end do
      end if

529   CONTINUE
      GO TO 50

99    continue
      print *,'data : no. of connections in lower triangle =',nall
      if(ixopt.eq.1)then ! add contribution for rhs all at once ...
         nn=0
         do i=1,nsrow-1
         if(lll(i))then
            nn=nn+1
            ivec(nn)=i
         end if
         end do
         print *,'row for rhs',nsrow,nn,nall
         call lnkirw(nsrow,nn,ivec,nall,maxrec)
         print *,'data : no. of connections in lower triangle =',nall
      end if      
      nalld=nall

      deallocate(nnq,ieq,iieei,iiqhq,lll,iirow,stat=ii)
      if(ii>0)stop 'dxadj3 : dealloc 1'

c     -----------------
C     ... PEDIGREE PART
C     -----------------

      REWIND(IUN44)
      READ(IUN44)DETL
      MM=NQ+NQ222
      NEL=0

150   READ(IUN44,END=199)JANIM,IANIM
      II=(IANIM-1)*MM+LIM3
      JJ=(JANIM-1)*MM+LIM3
      IF(IOPCOV.NE.2)THEN
         KK=NQ
         LL=NQ+1
      ELSE
         KK=MM
         LL=1
      END IF

C     ANIMAL EFFECT
      DO  I=1,NQ
      IF(IANIM.EQ.JANIM)KK=I-1
      IVEC(:kk)=IFLAG( JJ+1:jj+kk)
      NEL=NEL+KK
      CALL LNKIRW(IFLAG(II+I),KK,IVEC,NALL,MAXREC)
      end do

C     SECOND ANIMAL EFFECT
      IF(IOPRN2.GT.0 .AND. IOPCOV.NE.4)THEN
         IF( (IOPCOV.EQ.3) .AND. (IANIM.NE.JANIM) )GO TO 150
         LL0=LL-1
         KK=MM
         DO I=NQ+1,MM
         IF(IANIM.EQ.JANIM)KK=I-1
         IVEC(ll-LL0:kk-ll0) = IFLAG( JJ+ll:jj+kk)
         NN=KK-LL0
         NEL=NEL+NN
         CALL LNKIRW(IFLAG(II+I),NN,IVEC,NALL,MAXREC)
         end do
      END IF
      GO TO 150

C     BLOCKS FOR SECOND ANIMAL EFFECT WITH ARBITRARY COVARIANCE
199   IF (IOPCOV.EQ.4)THEN
          REWIND(IUN45)
250       READ(IUN45,END=299)JANIM,IANIM
          II=(IANIM-1)*MM+LIM3+NQ
          JJ=(JANIM-1)*MM+LIM3+NQ
          KK=NQ222
          DO  I=1,NQ222
          IF(IANIM.EQ.JANIM)KK=I-1
          ivec(:kk)=Iflag( JJ+1:jj+kk)
          nel=nel+kk
          call lnkirw(iflag(ii+i),kk,ivec,nall,maxrec)
          end do
          GO TO 250
      END IF

C     COVARIANCE BLOCKS FOR ADDITIONAL RANDOM EFFECT
299   IF(IOPRN1.EQ.1)THEN
         II=LIM2-NQ111
         DO IL=1,NRAND1
         II=II+NQ111
         DO  I=1,NQ111
         nn=i-1
         ivec(:nn)=Iflag( II+1:ii+nn )
         nel=nel+nn
         call lnkirw(iflag(ii+i),nn,ivec,nall,maxrec)
         end do
         end do
      END IF
      if(ioprn3.eq.1)then
         II=LIM3a-NQ333
         DO IL=1,NRAND3
         II=II+NQ333
         DO  I=1,NQ333
         nn=i-1
         ivec(:nn)=Iflag( II+1:ii+nn )
         nel=nel+nn
         call lnkirw(iflag(ii+i),nn,ivec,nall,maxrec)
         end do
         end do
      END IF

      nallp=nall-nalld
      print *,'ped. : no. of off-diagonal elements added   =',nel
      print *,'     : no. of add. conn.s in lower triangle =',nallp
      print *,'all  : no. of connections in lower triangle =',nall
 

c     -------------------------------
c     sort linked list of connections 
c     -------------------------------

      newplc=0
      do 801 irow=1,nsrow
      iplace=kfirst(irow)
      nrow=0
 804  if(iplace.eq.0)go to 802
      newplc=newplc+1
      icol=kvcol(iplace)
 803  if( (iplace.gt.0) .and. (iplace.lt.newplc) )then
         iplace=knext(iplace)
         go to 803
      end if
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
      go to 804
 802  kfirst(irow)=nrow
 801  continue
      k=1
      do  irow=1,nsrow
      n=kfirst(irow)
      kfirst(irow)=k
      k=k+n
      end do
      kfirst(nsrow+1)=k

c     knext not needed from here onwards 
      deallocate (knext,stat=ii)
      if(ii>0)stop 'dxadj3 : de-alloc KNEXT'

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
      kfirst(nsrow+1)=nall+nall+1

c     ... move portion for one row at a time; in reverse order
      do irow=nsrow,1,-1
c     ... last element for this row
      k2=k1-1
c     ... first element for this row
      k1=kfirst(irow)
c     ... distance to be moved
      nn=nn-iwork(irow)
      do k=k1,k2
      kvcol(k+nn)=kvcol(k)
      end do
c     ... adjust pointer to first element in this row
      kfirst(irow)=k1+nn
c     ... store position of currently last element; overwrite "iwork"
      iwork(irow)=k2+nn
      end do

c     insert "lower" connections; each row only adds elements to rows
c                                                     with a lower no. 
      do irow=1,nsrow
      do  ij=kfirst(irow),iwork(irow)
      icol=kvcol(ij)
      k=iwork(icol)+1
      kvcol(k)=irow
      iwork(icol)=k
      end do
      end do

      return

      contains

C     =================================================
      SUBROUTINE     LNKIRW (irow,nn,ivec,nrczhz,maxrec)
C     =================================================

!     arguments
      integer, intent(in) :: irow,nn,maxrec
      integer, intent(inout) :: nrczhz
      integer, dimension(nn),intent(in) :: ivec

!     variables
      integer :: kk, ipre,ipcol,iplace,icol
!     -------------------------------------------------------------------      
      
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
         DO I=1,NN
         WRITE(*,*)' COLUMN   ',I,IVEC(I)
         end do
         STOP
      END IF
      if(icol.ge.irow)print *,'lnkirw : col.gt.row',irow,icol

10    IF( (IPLACE.GT.0) .AND. (KVCOL(IPLACE).LT.ICOL) )THEN
         IPRE=IPLACE
         IPLACE=KNEXT(IPLACE)
         GO TO 10
      ELSE IF( (IPLACE.GT.0) .AND. (ICOL.EQ.KVCOL(IPLACE)) )THEN
         IPRE=IPLACE
         IPLACE=KNEXT(IPLACE)
      ELSE
         NRCZHZ=NRCZHZ+1
         IF(NRCZHZ.GT.MAXREC)THEN
            PRINT *,MAXREC,IROW,ICOL,KK,NN
            STOP '"LNKIRW" : DIMENSIONS EXCEEDED !!'
         END IF
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

      END subroutine dxadj3












