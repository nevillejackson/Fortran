C===========================================================================
      SUBROUTINE DxADJ3 (nalld,nallp,ixopt,mxspaa)
C===========================================================================

      use parameters
      use units
      use sparse
      use combinations
      use levels
      use numbers
      use adjacency
      use like_components
      use residuals
      use read_iun52
      use ages

!     arguments
      integer, intent(out)                   :: nalld,nallp
      integer, intent(in)                    :: ixopt,mxspaa

!     local variables
      logical, dimension(:), allocatable     :: lll,iirow
      logical, dimension(:,:), allocatable   :: iiqhq,jjeei
      logical, dimension(:,:,:), allocatable :: iieei
      real(8)                                :: fvalue
      integer, dimension (:), allocatable    :: ivec,iwork
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate( iiqhq(mqhq,mqhq),stat=ii)
      if(ii>0)stop 'dxadj3 : alloc 3b'
      allocate( lll(neqns),stat=ii)
      if(ii>0)stop 'dxadj3 : alloc 3c'
      allocate( ivec(neqns),stat=ii)
      if(ii>0)stop 'dxadj3 : alloc 3e'
      allocate( iwork(0:neqns),stat=ii)
      if(ii>0)stop 'dxadj3 : alloc 3f'
      allocate( iirow(neqns),stat=ii)
      if(ii>0)stop 'dxadj3 : alloc 3g '

      LIM0=1
      if(ixopt.eq.1)lll=.false.

c     ---------------------------------------------------------------
c     accumulate adjacency structure of lower triangle in linked list
c     ---------------------------------------------------------------

C     DETERMINE STRUCTURE OF INVERSE RESIDUAL COV. MATRICES
      if(nq.eq.1)then
         if(ieqmod.eq.0)then
            CALL DxRES3 (XVEC,FVALUE,IOUT,0)
         else
            dete=0.d0
            CALL DxRaz3 (XVEC,FVALUE,IOUT,ioptlk)
         end if
         if(ncomb.le.max_comb)then
            allocate( iieei(mobs,mobs,ncomb),stat=ii)
            if(ii>0)stop 'dxadj3 : alloc 3a'
            iieei=.false.
            where (eei .ne.0) IIEEI = .true.
         end if
      else
         CALL DXREZ3 (XVEC,FVALUE,IOUT,IOPTLK)
         dete=0.d0
      end if
      if(iout.eq.1)stop 'dxadj3 : e**(-1)'
      allocate( jjeei(mobs,mobs),stat=ii)
      if(ii>0)stop 'dxadj3 : alloc 3b'

C     -------------
C     ... DATA PART
C     -------------

C     INITIALIZE
      if(ixopt.eq.1)rewind(iun52)
      NALL=0
      MAXREC=MXSPAa
      kfirst=0
      NRC=0
      sig=0.d0

50    READ(IUN52,END=99)icomb,NOBS,NR,NNA(:NOBS,:),nnq(:nobs),
     &     IICOL(:NR),((IEQ(K,L) ,K=1,NEFF(NNQ(L)) ),L=1,NOBS)
      nrc=nrc+1
      if(mod(nrc,1000).eq.0)write(*,'(a,2i10)')'nrc =',nrc,nall

      LIMNR1=LIM1+NR
      iiqhq=.false.
      if(nq.eq.1 .and. ncomb.le.max_comb)then
         jjeei(:nobs,:nobs)=iieei(:nobs,:nobs,icomb)
      else 
         if(nq.eq.1 .and.ieqmod.eq.0)then
            call ee_inverse(xvec,icomb,0,0)
         else if(nq.eq.1 .and.ieqmod.eq.1)then
            call gg_inverse(xvec,0,0,nobs,nna,nnq)
         else
            call ff_inverse (xvec,0,0,nobs,nna,nnq)
         end if
         jjeei=.false.
         where (sig .ne.0) jjEEI = .true.
      end if

C     ACCUMULATE CONTRIBUTIONS TO MMM FOR ONE OBSERVATION AT A TIME ...
C     ... SET UP ROW OF X'R(-1) FOR THIS OBSERVATION
      DO  IOBS=1,NOBS
      IQ=NNQ(IOBS)
      II=NFRST(IQ)

      iirow(1:limnr1)=.false.
      DO JOBS=1,NOBS
      IF( jjEEI(JOBS,IOBS) )THEN
         JQ=NNQ(JOBS)
         IIROW( nfrst(jq)+1 : nfrst(jq)+nfr(jq) )=.true.
         DO J=1,NEFF(JQ)
         IIROW(IEQ(J,JOBS))=.true.
         end do
      END IF
      end do  ! jobs

C     MARK FILLED OFF-DIAGONAL CELLS : COV * COV PART OF MMM
      DO K=1,NFR(IQ)
      IROW=nfrst(iq)+K
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
      if(iirow(l))iiqhq(l,jrow)=.true. !do not change!
      end do
      end do

      end do ! iobs

C     ALL RECORDS FOR THE ANIMAL PROCESSED, recode to eq.nos
      DO I=1,NR
      IROW=IICOL(I)
      if(iflag(irow).eq.0)cycle
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

      end do
      GO TO 50

99    continue
      write(*,'(a,i10)')'data : no. of connections in lower triangle =',
     &                                                             nall
      if(ixopt.eq.1)then ! add contribution for rhs all at once ...
         nn=0
         do i=1,nsrow-1
         if(lll(i))then
            nn=nn+1
            ivec(nn)=i
         end if
         end do
         write(*,'(a,3i10)')'row for rhs',nsrow,nn,nall
         call lnkirw(nsrow,nn,ivec,nall,maxrec)
         write(*,'(a,i10)')
     &            'data : no. of connections in lower triangle =',nall
      end if      
      nalld=nall

      deallocate(jjeei,iiqhq,lll,iirow,stat=ii)
      if(ii>0)stop 'dxadj3 : dealloc 1'
      if(iq.eq.1 .and. ncomb.le.max_comb)then
         deallocate (iieei,stat=ii)
         if(ii>0)stop 'dxadj3 : dealloc 1a'
      end if

c     -----------------
C     ... PEDIGREE PART
C     -----------------

      if(nanim>0)then
         REWIND(IUN44)
         READ(IUN44)DETL
         MM=ksfit(1)+ksfit(2)
         NEL=0

150      READ(IUN44,END=199)JANIM,IANIM
         II=(IANIM-1)*MM+LIM3
         JJ=(JANIM-1)*MM+LIM3
         IF(IOPCOV.NE.2)THEN
            KK=ksfit(1)
            LL=kk+1
         ELSE
            KK=MM
            LL=1
         END IF

C        ANIMAL EFFECT
         DO  I=1,sum(kfit(1,:nq))
         IF(IANIM.EQ.JANIM)KK=I-1
         IVEC(:kk)=IFLAG( JJ+1:jj+kk)
         NEL=NEL+KK
         CALL LNKIRW(IFLAG(II+I),KK,IVEC,NALL,MAXREC)
         end do

C        SECOND ANIMAL EFFECT
         IF(IOPRN2.GT.0 .AND. IOPCOV.NE.4)THEN
            IF( (IOPCOV.EQ.3) .AND. (IANIM.NE.JANIM) )GO TO 150
            LL0=LL-1
            KK=MM
            DO I=ksfit(1)+1,MM
            IF(IANIM.EQ.JANIM)KK=I-1
            IVEC(ll-LL0:kk-ll0) = IFLAG( JJ+ll:jj+kk)
            NN=KK-LL0
            NEL=NEL+NN
            CALL LNKIRW(IFLAG(II+I),NN,IVEC,NALL,MAXREC)
            end do
         END IF
         GO TO 150

C        BLOCKS FOR SECOND ANIMAL EFFECT WITH ARBITRARY COVARIANCE
199      IF (IOPCOV.EQ.4)THEN
             REWIND(IUN45)
250          READ(IUN45,END=299)JANIM,IANIM
             II=(IANIM-1)*MM+LIM3+ksfit(1)
             JJ=(JANIM-1)*MM+LIM3+ksfit(1)
             KK=ksfit(2)
             DO I=1,ksfit(2)
             IF(IANIM.EQ.JANIM)KK=I-1
             ivec(:kk)=Iflag( JJ+1:jj+kk)
             nel=nel+kk
             call lnkirw(iflag(ii+i),kk,ivec,nall,maxrec)
             end do
             GO TO 250
         END IF
      END IF

C     COVARIANCE BLOCKS FOR ADDITIONAL RANDOM EFFECT
299   IF(IOPRN1.EQ.1 .and. ieqmod.eq.0)THEN
         II=LIM2-ksfit(4)
         DO IL=1,NRAND1
         II=II+ksfit(4)
         DO I=1,ksfit(4)
         nn=i-1
         ivec(:nn)=Iflag( II+1:ii+nn )
         nel=nel+nn
         call lnkirw(iflag(ii+i),nn,ivec,nall,maxrec)
         end do
         end do
      END IF
      IF(IOPRN3.EQ.1)THEN
         II=LIM3a-ksfit(5)
         DO IL=1,NRAND3
         II=II+ksfit(5)
         DO I=1,ksfit(5)
         nn=i-1
         ivec(:nn)=Iflag( II+1:ii+nn )
         nel=nel+nn
         call lnkirw(iflag(ii+i),nn,ivec,nall,maxrec)
         end do
         end do
      END IF

      nallp=nall-nalld
      write(*,'(a,i10)')'ped. : no. of off-diagonal elements added   =',
     &                                                              nel
      write(*,'(a,i10)')'     : no. of add. conn.s in lower triangle =',
     &                                                            nallp
      write(*,'(a,i10)')'all  : no. of connections in lower triangle =',
     &                                                             nall
      deallocate(ivec,stat=ii)
      if(ii>0)stop 'dxadj3 : dealloc ivec'

c     -------------------------------
c     sort linked list of connections 
c     -------------------------------

      newplc=0
      do irow=1,nsrow
      iplace=kfirst(irow)
      nrow=0
 804  if(iplace.eq.0)go to 802
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
      go to 804
 802  kfirst(irow)=nrow
      end do ! irow
      k=1
      do irow=1,nsrow
      n=kfirst(irow)
      kfirst(irow)=k
      k=k+n
      end do
      kfirst(nsrow+1)=k

c     knext not needed from here onwards 

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

      deallocate(iwork,stat=ii)
      if(ii>0)stop 'dxadj3 : dealloc iwork'
      return

      contains

C     =================================================
      SUBROUTINE     LNKIRW (irow,nn,ivec,nrczhz,maxrec)
C     =================================================

!     arguments
      integer, intent(in)               :: irow,nn,maxrec
      integer, intent(inout)            :: nrczhz
      integer, dimension(nn),intent(in) :: ivec

!     variables
      integer                           :: kk,ipre,ipcol,iplace,icol
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
      if(icol.ge.irow)write(*,'(a,2i10)')' lnkirw : col.gt.row',irow,
     &                                                          icol

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
            write(*,*)MAXREC,IROW,ICOL,KK,NN
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












