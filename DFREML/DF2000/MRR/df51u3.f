!===========================================================================
      SUBROUTINE      DF51R3 (iun51)
!===========================================================================

      use parameters
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
      use zero_rows
      use mme3
      use read_iun52
      use phimatrix, only : irropt,irrmet

      integer, intent(in)    :: iun51
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      READ(IUN51)IOPMOD
      IF(IOPMOD.lt.3)CALL DFERR2(IOPMOD,4)
      read(iun51)fped,fdata,cwdir
      read(iun51)kfit,ksfit

      READ(IUN51)NOSVEC,NFILL,IORDER,NCOMB,NRZERO,NRNKX,kftfix,irropt,
     &                                                         irrmet
      call set_nos
      READ(IUN51)NSROW,MAXSUB,MAXLNZ,mqhq
      mqhq=mqhq+lim1

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

      READ(IUN51)NCOV(:nq)
      READ(IUN51)NFIX(:nq),nfxreg
      READ(IUN51)NFIX1(:nq)
      READ(IUN51)NFIX2(:nq)
      READ(IUN51)NEFF(:nq)
      READ(IUN51)NFL(:nq)
      READ(IUN51)NFR(:nq)
      READ(IUN51)NFRST(:NQ)
      READ(IUN51)MXOBS(:NQ)
      READ(IUN51)NXOBS(:NQ)
      READ(IUN51)ITRAIT
      READ(IUN51)NR8
      if(nq>1)read(iun51)nboth

      mcov=maxval(ncov)
      mfix=maxval(nfix1)+nfxreg
      mobs=sum(nxobs)
      call all_means (nq,mcov)
      call all_levels_2(nq,mcov,mfix)
      call all_names (nq,mxparm,mfix,mcov)
      if(iopmod.ne.5 .and. iopmod.ne.6)then
         nq2=nq
         if(ioprn2.eq.1.and.iopcov.le.2)nq2=nq222+nq
      else
         nq2=kfitmx
         if(ioprn2.eq.1.and.iopcov.le.2)nq2=nq2+kfitmx
      end if
      mnfr=maxval(nfr)
      if(mnfr.eq.0)mnfr=1
      READ(IUN51)YBAR
      READ(IUN51)SDEV
      READ(IUN51)CVAR
      READ(IUN51)YMIN
      READ(IUN51)YMAX
      READ(IUN51)SMIN
      READ(IUN51)SMAX

      READ(IUN51)TRAIT(:NQ)
      if(iopmod.eq.4)then
          read(IUN51)nq
          allocate(iiage(nq,1),stat=ii)
          if(ii>0)stop 'alloc'
          read(IUN51)iiage
         allocate(mmark(nq,ncomb),stat=ii)
      else if(iopmod.eq.5 .or. iopmod.eq.6)then
         read(iun51)nmeta,mage,manq,nanq
         m=max0(mage,mobs)
         allocate(nage(nmeta),iavec(nmeta),iiage(mage,nmeta),
     &            meage(mage),nnrec(mage,nq),mmr(m),
     &            nnage(mage,nq,nmeta),eevec(mage,nq*(nq+1)/2),
     &            astar(mage),stat=ii)
         if(ii>0)stop 'df51r3 : alloc ages '
         read(iun51)nage,ialog
         read(IUN51)iiage
         do j=1,nmeta
         do i=1,nq
         read(iun51)nnage(:nage(j),i,j)
         end do
         end do
         allocate(mmark(mage,ncomb),stat=ii)
         read(iun51)nnrec
         read(iun51)mmr
      else
         allocate(mmark(nq,ncomb),stat=ii)
      end if

      READ(IUN51)KLINES
      IF(KLINES.GT.0)THEN
         DO  I=1,KLINES
         READ(IUN51)TEXT(I)
         END DO
      END IF
      DO IQ=1,NQ
      IF(NFIX2(IQ).GT.0)THEN
         READ(IUN51)NLEV(:NFIX2(IQ),IQ)
         READ(IUN51)FIXED(:NFIX2(IQ),IQ)
      END IF
      IF(NCOV(IQ).GT.0)THEN
         READ(IUN51)NPOW(:NCOV(IQ),IQ)
         READ(IUN51)CBAR(:NCOV(IQ),IQ)
         READ(IUN51)CSDEV(:NCOV(IQ),IQ)
         READ(IUN51)COVAR(:NCOV(IQ),IQ)
      END IF
      END DO
      READ(IUN51)NNCOM

      do i=1,ncomb
      read(iun51)mmark(:,i)
      end do 
      IF(NRZERO.GT.0)then
         allocate(krzero(nrzero),stat=ii)
         if(ii>0)stop 'alloc krzero'
         READ(IUN51)KRZERO
      end if
      read(iun51)detl
      NQQ=kfitmx*(kfitmx+1)/2
      NQQ2=(kfitmx+kfitmx)*(kfitmx+kfitmx+1)/2
      NQSQ=kfitmx*kfitmx

      call all_read52( neff(1), mnfr,mobs,mqhq,nmeta)
      allocate(dete(ncomb),stat=ii)
      if(ii>0)stop 'df51r3 : alloc dete'
      allocate(ee(mobs,mobs),work(mobs*(mobs+1)/2),sig(mobs,mobs),
     &                                                   stat=ii)
      if(ii>0)stop 'df51r3 : alloc ee'
      if(ncomb.le.max_comb)then                
         allocate(eei(mobs,mobs,ncomb),stat=ii) ! this can be pretty big
         if(ii>0)stop 'df51r3 : alloc eei'
      end if

      RETURN

C============================================================================
      ENTRY DF51W3(IUN51)
C============================================================================

      REWIND(IUN51)
      WRITE(IUN51)IOPMOD
      write(iun51)fped,fdata,cwdir
      write(iun51)kfit,ksfit
      WRITE(IUN51)NOSVEC,NFILL,IORDER,NCOMB,NRZERO,NRNKX,kftfix,irropt,
     &                                                          irrmet
      print *,'nrnkx=',nrnkx
      WRITE(IUN51)NSROW,MAXSUB,MAXLNZ,mqhq
      write(iun51)ieqnew(:neqns)
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
      WRITE(IUN51)NCOV(:NQ)
      WRITE(IUN51)NFIX(:NQ),nfxreg
      WRITE(IUN51)NFIX1(:NQ)
      WRITE(IUN51)NFIX2(:NQ)
      WRITE(IUN51)NEFF(:NQ)
      WRITE(IUN51)NFL(:NQ)
      WRITE(IUN51)NFR(:NQ)
      WRITE(IUN51)NFRST(:NQ)
      WRITE(IUN51)MXOBS(:NQ)
      WRITE(IUN51)NXOBS(:NQ)
      WRITE(IUN51)ITRAIT
      WRITE(IUN51)NR8
      if(nq>1)write(iun51)nboth(:nqq)

      WRITE(IUN51)YBAR(:NQ)
      WRITE(IUN51)SDEV(:NQ)
      WRITE(IUN51)CVAR(:NQ)
      WRITE(IUN51)YMIN(:NQ)
      WRITE(IUN51)YMAX(:NQ)
      WRITE(IUN51)SMIN(:NQ)
      WRITE(IUN51)SMAX(:NQ)
      WRITE(IUN51)TRAIT(:NQ)
      if(iopmod.eq.4)then
          WRITE(IUN51)nq
          WRITE(IUN51)iiage(:NQ,1)
      else if(iopmod.eq.5 .or. iopmod.eq.6)then
          write(iun51)nmeta,mage,manq,nanq
          WRITE(IUN51)nage(:nmeta),ialog
          WRITE(IUN51)iiage(:mage,:nmeta)
          do j=1,nmeta
          do i=1,nq
          write(iun51)nnage(:nage(j),i,j)
          end do
          end do
          write(iun51)nnrec
          write(iun51)mmr
      end if

      WRITE(IUN51)KLINES
      IF(KLINES.GT.0)THEN
         DO L=1,KLINES
         WRITE(IUN51)TEXT(L)
         end do
      END IF
      DO  IQ=1,NQ
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
      do i=1,ncomb
      write(iun51)mmark(:,i)
      end do
      IF(NRZERO.GT.0)WRITE(IUN51)KRZERO(:NRZERO)
      write(iun51)detl
      CLOSE(IUN51)
      RETURN
      END subroutine df51r3

