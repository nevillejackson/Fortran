C============================================================================
      SUBROUTINE DF51R1 (iopmod)
C============================================================================

      use sparse
      use names
      use order
      use like
      use solve
      use means
      use levels
      use comments
      use units
      use numbers

      integer, intent(inout) :: iopmod
      
      READ(IUN51)IOPMOD
      IF(IOPMOD.NE.1)CALL DFERR2(IOPMOD,1)
      READ(IUN51)NFILL,IORDER,nrec,nq,nfix,nfix1,nfix2,ncov,nfl,nfr,
     &           lim1,lim2,lim3,ioprn1,ioprn2,iopcov,imodel,jopcov,
     &           nanim,neqns,nainv
      if(nainv>1)read(iun51)nna(:nainv)
      allocate (trait(nq),fixed(nfix2),covar(ncov),stat=ii)
      if(ii>0)stop 'alloc "names" '
      allocate(ieqnew(neqns),iflag(neqns),nlev(nfix2),npow(ncov),
     &         nsize1(nfix2),icnest(ncov),nlnest(ncov),irropt(ncov),
     &         fvlvec(nq),stat=ii)
      if(ii>0)stop 'alloc ieqnew'
      REAd(iun51)IEQNEW

      IF(IORDER.GE.3)THEN
         READ(IUN51)NSROW,MAXSUB,MAXLNZ
         READ(iun51)IFLAG
         allocate(ivperm(neqns),ixsub(maxsub),ixvec1(nsrow+1),
     &   ixvec2(nsrow+1),stat=ii)
         if(ii>0)stop 'alloc ixsub etc.'
         READ(iun51)IVPERM(:NSROW)
         READ(iun51)IXVEC1
         READ(iun51)IXVEC2
         m=maxsub/4                    ! get segmentation error when writing
         read(iun51)ixsub(:m)          ! whole vector for large maxsub,
         read(iun51)ixsub(m+1:2*m)     ! chopping it into bits helps !
         read(iun51)ixsub(2*m+1:3*m)
         read(iun51)ixsub(3*m+1:maxsub)
      END IF

      allocate(ybar(nq),ysdev(nq),cbar(ncov),csdev(ncov),stat=ii)
      if(ii>0)stop 'alloc means'
      READ(IUN51)YBAR
      READ(IUN51)YSDEV
      READ(IUN51)TRAIT
      READ(IUN51)KLINES
      IF(KLINES.GT.0)THEN
         DO I=1,KLINES
         READ(IUN51)TEXT(I)
         end do
      END IF
      IF(NFIX2.GT.0)THEN
         READ(IUN51)NLEV
         read(IUN51)nsize1
         READ(IUN51)FIXED
      END IF
      NRAND1=0
      IF(IOPRN1.GE.1)nrand1=sum( nlev(nfix+1:nfix1) )
      nrand2=0
      IF(IOPRN2>0)nrand2=nlev(nfix2)
      read(iun51)nfr1,nfr2,ncov1,ncov2,lim1a,lim1b
      IF(NCOV.GT.0)THEN
         READ(IUN51)NPOW
         READ(IUN51)CBAR
         READ(IUN51)CSDEV
         READ(IUN51)COVAR(:NCOV)
         read(iun51)icnest(:ncov)
         read(iun51)nlnest(:ncov)
      END IF
      READ(IUN51)NRZERO
      m=nrzero
      if(nrzero.eq.0)m=1
      allocate (nnvec(neqns),krzero(m),diazer(m),rhszer(m,nq),stat=ii)
      if(ii>0)stop 'err alloc solve'
      IF(NRZERO.GT.0)THEN
          READ(IUN51)KRZERO(:NRZERO)
          READ(IUN51)DIAZER(:NRZERO)
          READ(IUN51)RHSZER(:nrzero,:nq)
      END IF
      read(iun51)detll
      RETURN

C============================================================================
      ENTRY DF51W1(iopmod)
C============================================================================

      REWIND(IUN51)
      WRITE(IUN51)IOPMOD
      write(IUN51)NFILL,IORDER,nrec,nq,nfix,nfix1,nfix2,ncov,nfl,nfr,
     &           lim1,lim2,lim3,ioprn1,ioprn2,iopcov,imodel,jopcov,
     &           nanim,neqns,nainv
      if(nainv>1)write(iun51)nna(:nainv)
      WRiTe(iun51)IEQNEW(:NEQNS)
      if(iorder.ge.3)then
         WRITE(IUN51)NSROW,MAXSUB,MAXLNZ
         write(iun51)iflag(:neqns)
         write(iun51)ivperm(:nsrow)
         write(iun51)ixvec1(:nsrow+1)
         write(iun51)ixvec2(:nsrow+1)
         m=maxsub/4
         write(iun51)ixsub(:m)
         write(iun51)ixsub(m+1:2*m)
         write(iun51)ixsub(2*m+1:3*m)
         write(iun51)ixsub(3*m+1:maxsub)
      end if
      WRITE(IUN51)YBAR(:NQ)
      WRITE(IUN51)YSDEV(:NQ)
      WRITE(IUN51)TRAIT(:NQ)
      WRITE(IUN51)KLINES
      IF(KLINES.GT.0)THEN
         DO L=1,KLINES
         WRITE(IUN51)TEXT(L)
         end do
      END IF
      IF(NFIX2.GT.0)THEN
         WRITE(IUN51)NLEV(:NFIX2)
         WRITE(IUN51)nsize1(:NFIX2)
         WRITE(IUN51)FIXED(:NFIX2)
      END IF
      write(iun51)nfr1,nfr2,ncov1,ncov2,lim1a,lim1b
      IF(NCOV.GT.0)THEN
         WRITE(IUN51)NPOW(:NCOV)
         WRITE(IUN51)CBAR(:NCOV)
         WRITE(IUN51)CSDEV(:NCOV)
         WRITE(IUN51)COVAR(:NCOV)
         write(iun51)icnest(:ncov)
         write(iun51)nlnest(:ncov)
      END IF
      WRITE(IUN51)NRZERO
      IF(NRZERO.GT.0)THEN
          WRITE(IUN51)KRZERO(:NRZERO)
          WRITE(IUN51)DIAZER(:NRZERO)
          WRITE(IUN51)RHSZER(:nrzero,:nq)
      END IF
      write(iun51)detll
      CLOSE(IUN51)
      RETURN
      END subroutine df51r1










