!============================================================================
      SUBROUTINE DF51R3 (iun51)
!============================================================================

      use params
      use names
      use comments
      use sparse
      use ages
      use means
      use combinations
      use levels
      use numbers
      use order
      use like_components
      use residuals
      use dmatrices
      use zero_rows
      use mme3 

      integer, intent(in) :: iun51

c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      READ(IUN51)IOPMOD
!      IF(IOPMOD.lt.3)CALL DFERR2(IOPMOD,4)
      READ(IUN51)NOSVEC,NFILL,IORDER,NCOMB,NRZERO,mqhq
      READ(IUN51)NSROW,MAXSUB,MAXLNZ
      call set_nos

      allocate (ieqnew(neqns),iflag(neqns),stat=ii)
      if(ii>0)stop 'df51r3 : alloc IEQNEW '
      read(iun51)ieqnew
      IF(IORDER.GE.3)THEN

         allocate (ivperm(nsrow),stat=ii)
         if(ii>0)stop 'df51u3 : alloc IVPERM'
         allocate (ixvec1(nsrow+1),stat=ii)
         if(ii>0)stop 'df51u3 : alloc IXVEC1'
         allocate (ixvec2(nsrow+1),stat=ii)
         if(ii>0)stop 'df51u3 : alloc IXVEC2'
         allocate (ixsub(maxsub),stat=ii)
         if(ii>0)stop 'df51u3 : alloc IXSUB'

         read(iun51)iflag
         read(iun51)ivperm
         read(iun51)ixvec1
         read(iun51)ixvec2
         m=maxsub/4                    ! get segmentation error when writing
         read(iun51)ixsub(:m)          ! whole vector for large maxsub,
         read(iun51)ixsub(m+1:2*m)     ! chopping it into bits helps !
         read(iun51)ixsub(2*m+1:3*m)
         read(iun51)ixsub(3*m+1:maxsub)
      END IF

      call all_levels_1(nq)
      nqq=nq*(nq+1)/2
      call all_numbers(nq,nqq)
      call all_combis (nq,ncomb)
      call all_mme3(nq)

      READ(IUN51)NCOV,NFIX,NFIX1,NFIX2,nfix3,NEFF
      READ(IUN51)NFL,NFR,NFRST
      READ(IUN51)MXOBS,NXOBS,ITRAIT,NR8
      mcov=maxval(ncov)
      mfix=maxval(nfix2)
      mobs=sum(mxobs)
      call all_means (nq,mcov)
      call all_levels_2(nq,mcov,mfix)
      call all_names (nq,mxparm,mfix,mcov)
      mnfr=maxval(nfr)
      if(mnfr.eq.0)mnfr=1

      allocate(dete(ncomb),stat=ii)
      if(ii>0)stop 'df51r3 : alloc dete'
      allocate(ee(mobs,mobs),eei(mobs,mobs,ncomb),stat=ii)
      if(ii>0)stop 'df51r3 : alloc ee'

      READ(IUN51)YBAR,SDEV,CVAR
      READ(IUN51)YMIN,YMAX
      READ(IUN51)SMIN,SMAX

      READ(IUN51)TRAIT
      if(iopmod.eq.4)then
         nage=nq
         allocate(iiage(nq),nnage(nq,nq),xxage(nq,nq),stat=ii)
         if(ii>0)stop 'df51r3 : alloc ages '
         read(IUN51)iiage
      end if
      READ(IUN51)FPED,FDATA,CWDIR
      READ(IUN51)KLINES
      IF(KLINES.GT.0)THEN
         DO I=1,KLINES
         READ(IUN51)TEXT(I)
         end do
      END IF
      DO IQ=1,NQ
      IF(NFIX2(IQ).GT.0)THEN
         READ(IUN51)NLEV(:NFIX2(IQ),iq)
         READ(IUN51)FIXED(:NFIX2(IQ),iq)
      END IF
      IF(NCOV(IQ).GT.0)THEN
         READ(IUN51)NPOW(:NCOV(IQ),iq)
         READ(IUN51)CBAR(:NCOV(IQ),iq)
         READ(IUN51)CSDEV(:NCOV(IQ),iq)
         READ(IUN51)COVAR(:NCOV(IQ),iq)
      END IF
      end do
      READ(IUN51)NNCOM
      read(iun51)mmark(:nq,:ncomb)
      IF(NRZERO.GT.0)then
         allocate(krzero(nrzero),stat=ii)
         if(ii>0)stop 'alloc krzero'
         READ(IUN51)KRZERO
      end if
      NQQ=NQ*(NQ+1)/2
      NQQ2=(nq+nq)*(nq+nq+1)/2
      NQSQ=NQ*NQ
      RETURN

C============================================================================
      ENTRY DF51W3(IUN51)
C============================================================================

      REWIND(IUN51)
      WRITE(IUN51)IOPMOD
      WRITE(IUN51)NOSVEC,NFILL,IORDER,NCOMB,NRZERO,mqhq
      WRITE(IUN51)NSROW,MAXSUB,MAXLNZ
      write(iun51)ieqnew
      IF(IORDER.GE.3)THEN
         write(iun51)iflag(:neqns)
         write(iun51)ivperm(:nsrow)
         write(iun51)ixvec1(:nsrow+1)
         write(iun51)ixvec2(:nsrow+1)
         m=maxsub/4
         write(iun51)ixsub(:m)
         write(iun51)ixsub(m+1:2*m)
         write(iun51)ixsub(2*m+1:3*m)
         write(iun51)ixsub(3*m+1:maxsub)
      END IF
      WRITE(IUN51)NCOV(:NQ),NFIX(:NQ),NFIX1(:NQ),NFIX2(:NQ),nfix3(:nq),
     &                                                       NEFF(:NQ)
      WRITE(IUN51)NFL(:NQ),NFR(:NQ),NFRST(:NQ)
      WRITE(IUN51)MXOBS(:NQ),NXOBS(:NQ),ITRAIT(:NQ),NR8(:NQ)
      WRITE(IUN51)YBAR(:NQ),SDEV(:NQ),CVAR(:NQ)
      WRITE(IUN51)YMIN(:NQ),YMAX(:NQ)
      WRITE(IUN51)SMIN(:NQ),SMAX(:NQ)
      WRITE(IUN51)TRAIT(:NQ)
      if(iopmod.eq.4)WRITE(IUN51)iiage(:NQ)
      WRITE(IUN51)FPED,FDATA,CWDIR
      WRITE(IUN51)KLINES
      IF(KLINES.GT.0)THEN
         DO  L=1,KLINES
         WRITE(IUN51)TEXT(L)
         end do
      END IF
      DO IQ=1,NQ
      NNFIX2=NFIX2(IQ)
      IF(NNFIX2.GT.0)THEN
         WRITE(IUN51)NLEV(:nnfix2,IQ)
         WRITE(IUN51)FIXED(:nnfix2,IQ)
      END IF
      NNCOV=NCOV(IQ)
      IF(NNCOV.GT.0)THEN
         WRITE(IUN51)NPOW(:nncov,IQ)
         WRITE(IUN51)CBAR(:nncov,IQ)
         WRITE(IUN51)CSDEV(:nncov,IQ)
         WRITE(IUN51)COVAR(:nncov,IQ)
      END IF
      end do
      WRITE(IUN51)NNCOM(:NCOMB)
      write(iun51)mmark(:nq,:ncomb)
      IF(NRZERO.GT.0)WRITE(IUN51)KRZERO(:NRZERO)
      CLOSE(IUN51)
      RETURN
      END subroutine df51r3 







