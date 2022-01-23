!===========================================================================
      SUBROUTINE      DxRES3 (PARVEC,FVALUE,NPARM,IOUT,iopt)
!===========================================================================

      use params
      use names
      use units
      use phimatrix
      use parmap
      use combinations
      use numbers
      use traces
      use dmatrices
      use like_components, only : dete
      use residuals

!     arguments
      real(8), dimension(mxparm), intent(in) :: parvec
      real(8), intent(out)                   :: fvalue
      integer, intent(in)                    :: nparm, iopt
      integer, intent(out)                   :: iout

!     local variables
      real(8)                                :: detee,dett      
      real(8), dimension(:,:), allocatable   :: sigr,www
      logical, dimension(:,:), allocatable   :: de
      real(8), dimension(:), allocatable     :: work
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate(work(mobs*(mobs+1)/2),www(mobs,mobs),
     *         de(mobs*(mobs+1)/2,nparm),sigr(nq,nq),stat=ii)
      if(ii>0)stop 'dxres3 : alloc'

C     SET UP RESIDUAL COVARIANCE MATRIX FOR THE NQ TRAITS
C     ... ERROR COMPONENTS
      work=0.d0
      ij=0
      do i=1,nq
      do j=i,nq
      ij=ij+1
      k=nparno(j,i,5)
      if(k.gt.0)work(ij)=parvec(k)
      end do
      end do

C     ... EQUIVALENT MODEL FOR REPEATED RECORDS
      sigr=0.d0
      IF(IRPMOD.EQ.2)THEN
         ij=0
         do i=1,nq
         do j=i,nq
         ij=ij+1
         k=nparno(j,i,6)
         if(k.gt.0)then
             work(ij)=work(ij)+parvec(k)
             sigr(i,j)=parvec(k)
             sigr(j,i)=parvec(k)
         end if
         end do
         end do
      END IF

c     measurement errors
      if(iomease.eq.1)then
         do i=1,nq
         if(parvec(nparm+i).lt.0)then
            fvalue=big
            return
         end if
         work(ihmii(i,nq))=work(ihmii(i,nq))+parvec(nparm+i)
         end do
      end if

c     CHECK RESIDUAL COVARIANCE MATRIX (single records/trait)
      iout=0
      CALL DXCOVM(NPARM,0,NQ,NQQ,IOUT,-1,MOBS,EE,PARVEC,WORK,FVALUE,
     *                                                        DETEE)
      IF(IOUT.EQ.1)return
c     ...  ee now contains residual covariance matrix for the nq traits 

C     NEED INVERSE OF RESIDUALS FOR ALL COMBINATIONS OF RECORDS/TRAITS

      eei=0.d0
      if(iopt>0)rd=0.d0

      DO ICOMB=1,NCOMB

      IJ=0
      DO IQ=1,NQ
      DO K=1,mmark(iq,icomb)
      IJ=IJ+1
      WORK(IJ)=EE(IQ,IQ)

C     ... OTHER RECORDS, SAME TRAIT
      DO L=K+1,mmark(iq,icomb)
      IJ=IJ+1
      WORK(IJ)=SIGR(IQ,IQ)
      end do

C     ... COVARIANCES WITH RECORDS FOR OTHER TRAITS
c     nb : this assigns sig eij to all pairs of records on traits i and j;
c          this might lead to errors -> assume there's only one pair of
c          records (i,j) taken at the same time, i.e. expressing sig eij ? !!!
      DO JQ=IQ+1,NQ
      DO J=1,MMARK(JQ,ICOMB)
      ij=ij+1
      WORK(IJ)=EE(JQ,IQ)
      end do
      end do

      END DO
      END DO

c     check that matrix is positive definite & invert
      nr=0
      do i=1,nq
      nr=nr+mmark(i,icomb)
      end do
c      NR=sum( mmark(:nq,icomb) )
      NRR=NR*(NR+1)/2
      CALL DXCOVM(NPARM,0,NR,NRR,IOUT,0,MOBS,WWW,PARVEC,WORK,FVALUE,
     *                                                           DETT)
      IF(IOUT.EQ.1)THEN
         WRITE(*,*)'SUBROUTINE "DVRES3" '
         WRITE(*,*)'RESIDUAL COVARIANCE MATRIX OUT OF BOUNDS !'
         WRITE(*,*)'COMBINATION OF TRAITS NO. =',ICOMB
         DO IQ=1,NQ
         WRITE(*,*)'TRAIT NO.',IQ,'  NO. OF RECORDS ',MMARK(IQ,ICOMB)
         END DO
         DO I=1,NR
         WRITE(*,'(I4,(6G12.5))')I,(WORK(IHMSSF(I,J,NR)),J=1,I)
         END DO
         return
      END IF
      DETE(ICOMB)=DETT
      eei(:nr,:nr,icomb)=www(:nr,:nr)

      if(iopt.eq.0)cycle

c     -------------------------------------------------------------------------

c     set up d-matrices
      de=.false.
      IJ=0
      DO IQ=1,NQ
      ipare=nparno(iq,iq,5)
      iparr=nparno(iq,iq,6)
      DO K=1,mmark(iq,icomb)
      IJ=IJ+1
      de(ij,ipare-ngparm)=.true.
      if(iparr.gt.0)de(ij,iparr-ngparm)=.true.
      DO  L=K+1,mmark(iq,icomb)
      IJ=IJ+1
      if(iparr.gt.0)de(ij,iparr-ngparm)=.true.
      end do
      DO JQ=IQ+1,NQ
      jpare=nparno(jq,iq,5)
      jparr=nparno(jq,iq,6)
      DO  J=1,MMARK(JQ,ICOMB)
      ij=ij+1
      if(jpare.gt.0)de(ij,jpare-ngparm)=.true.
      if(jparr.gt.0)de(ij,jparr-ngparm)=.true.
      end do
      end do
      END DO
      END DO

c     premultiply d-matrices with inverse residual cov. matrix

      do ll=1,nrparm

      do k=1,nr
      do j=1,nr
      if( de(ihmssf(k,j,nr),ll) )rd(:nr,j,ll,icomb)= rd(:nr,j,ll,icomb)
     *                                                      +www(:nr,k)
      end do
      end do

c     calculate trace r(-1)d(ij)
      trres1(ngparm+ll,icomb)=sum( (/ (rd(i,i,ll,icomb),i=1,nr) /) )

c     .. post-multiply with inverse residual cov. matrix
      work(:nrr)=0.d0
      do k=1,nr
      do i=1,nr
      dd=rd(i,k,ll,icomb)
      if(dd.ne.0)then
         do j=i,nr
         ij=ihmssf(i,j,nr)
         work(ij)=work(ij)+dd*www(j,k)
         end do
      end if
      end do
      end do
      rdr(:nrr,ll,icomb)=work(:nrr)
 
      end do

      end do

      deallocate(work,www,de,sigr,stat=ii)
      if(ii>0)stop 'dxres3 : de alloc'
      RETURN
      END subroutine dxres3
