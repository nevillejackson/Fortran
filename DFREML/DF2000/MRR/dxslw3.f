!=============================================================================
      SUBROUTINE DxSLW3 (iopslv,iinput,iun666)
!=============================================================================

      use params
      use names
      use units
      use ages
      use means
      use levels
      use numbers
      use order
      use solutions
      use phimatrix
      implicit none

!     arguments
      integer, intent(in)                     :: iinput,iun666
      integer, intent(inout)                  :: iopslv

!     local variables
      integer                                 :: iun55,ii,iorth,krand1,
     &                                           iq,lim0=1,i,j,k,m,meff,
     &                                           ifit,l,im,kk0,iopibr,
     &                                           kanim, nrec2,mm2,mnfl,
     &                                           kmx
      real(8)                                 :: qq
      integer, dimension(:),allocatable       :: nnvec,idvec
      integer, dimension (:,:,:), allocatable :: idfix,nnfix
      real(8), dimension(:), allocatable      :: xvec,rhsfix,uvec,work,
     &                                           reg
      integer, dimension(:), allocatable      :: nlrnd1
      integer, dimension(:,:), allocatable    :: idrnd1
      integer, dimension(:,:), allocatable    :: nna
      real(8), dimension(:,:), allocatable    :: y1
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      kmx=maxval(kftfix)
      allocate(nnvec(neqns),xvec(neqns),rhsfix(neqns),uvec(nanim),
     *         idvec(nanim),work(kfitmx*2),reg(kmx), stat=ii)
      if(ii>0)stop 'dxslw3 : alloc'

      mnfl=maxval(nfl)
      allocate(idfix(mnfl,mfix,nq),nnfix(mnfl,mfix,nq),stat=ii)
      if(ii>0)stop 'dxslw3 : alloc 2'

C     INTERACTIVE INPUT FOR RUN OPTION
      if(iinput.eq.1)then
         write(*,*)' '
         write(*,*)'write out solutions for ...'
         write(*,9)'1  ...  fixed effects only'
         write(*,9)'2  ...  all effects '
         CALL OPTDEF(IOPSLV,1,2,1)
      end if

C     COUNT NO. OF RECORDS PER EFFECT & ACCUMMULATE RAW TOTALS
      if(iopslv.eq.2)call raw_means (iun52)

c     read lsq solutions etc. from unit "13"
      rewind(iun13)
      read(iun13)lim2
      read(iun13)nnvec(:lim2)
      read(iun13)rhsfix(:lim2)
      read(iun13)xvec(:lim2)

C     INPUT FROM UNIT "11"
      call read_iun11

      WRITE(IUN666,*)' '
      WRITE(IUN666,*)'------------------------------------------'
      WRITE(IUN666,*)'SOLUTION FOR FIXED EFFECTS AND COVARIABLES'
      WRITE(IUN666,*)'------------------------------------------'

C     COVARIABLES
      k=lim0
      iorth=0
      do iq=1,nq
      if(nq>1)write(Iun666,'(1x,a,i4/)')'Trait No. =',iq
      if(kftfix(iq)>0)then
         write(iun666,*)'orthogonal polynomials - fixed'
         k=nfrst(iq)
         do j=1,kftfix(iq)
         k=k+1
         WRITE(IUN666,911)K,'ORDER',J,IQ,SOLNS(IEQNEW(K)),xvec(k)
         reg(j)=solns(ieqnew(k))
         end do   
         if(iq.eq.1 .and. iinput.eq.1)then
            write(*,*)'calculate fixed regression for ages in data ? '
            call yndef(iorth,1)
         end if
         if(iorth.eq.1)then
            if(iq.eq.1)then
               open(1,file='Orth_reg_fix',status='unknown',form=
     *                                                    'formatted')
               allocate(nna(mage,nq),y1(mage,nq),stat=ii)
               if(ii>0)stop 'alloc orth_fix'
               read(iun13)qq
               read(iun13)nna
               read(iun13)y1
            end if
            call orthreg_fix
            if(iq.eq.nq)then
               close(1)
               write(*,*)'    Output file = "Orth_reg_fix" - Done ! '
               deallocate(nna,y1,stat=ii)
               if(ii>0)stop 'dealloc orth_fix'
            end if
         end if
      end if

      IF(NCOV(IQ).gt.0)then
          WRITE(IUN666,913)
          k=nfrst(iq)+kftfix(iq)
          DO  I=1,NCOV(IQ)
          WRITE(IUN666,909)'COVARIABLE NO.',I,COVAR(I,IQ)
          DO  J=1,NPOW(I,IQ)
          K=K+1
          WRITE(IUN666,911)K,'ORDER',J,IQ,SOLNS(IEQNEW(K)),xvec(k)
          end do
          end do
      end if

c     fixed effects
      IF(NFIX(IQ).gt.0)then
          WRITE(IUN666,913)
          DO I=1,NFIX(IQ)
          WRITE(IUN666,909)'FIXED EFFECT NO.',I,FIXED(I,IQ)
          DO  J=1,NLEV(I,IQ)
          K=K+1
          WRITE(IUN666,912)K,'LEVEL',J,IDFIX(J,I,IQ),IQ,nnvec(k),
     &                      rhsfix(k),SOLNS(IEQNEW(K)),xvec(k)
          end do
          end do
      end if

      end do   ! iq
      IF(iopslv.eq.1)RETURN ! fixed effects only required

c     additional random effect
      mm2=0
      IF(IOPRN1.EQ.1 .and.ieqmod.eq.0 )THEN
         mm2=1
         WRITE(IUN666,*)' '
         WRITE(IUN666,*)'-------------------------------------'
         WRITE(IUN666,*)'SOLUTION FOR PERMANENT ENVIRON.EFFECT'
         WRITE(IUN666,*)'-------------------------------------'
         WRITE(IUN666,914)
         DO I=1,NRAND1
         K=LIM2+(i-1)*ksfit(4)
         DO ifit=1,ksfit(4)
         K=K+1
         if(ifit.eq.1)then
            WRITE(IUN666,912)K,FIXED(NFIX1(1),1),I,IDRND1(I,1),IFIT,
     &                      NNVEC(K),RHSFIX(K),SOLNS(IEQNEW(K))
         ELSE
            WRITE(IUN666,9121)K,IFIT,SOLNS(IEQNEW(K))
         END IF
         END DO
         END DO
      END IF

      IF(IOPRN3.EQ.1)THEN
         WRITE(IUN666,*)' '
         WRITE(IUN666,*)'-------------------------------------'
         WRITE(IUN666,*)'SOLUTION FOR ADDITIONAL RANDOM EFFECT'
         WRITE(IUN666,*)'-------------------------------------'
         WRITE(IUN666,914)
         DO I=1,NRAND3
         K=LIM3a+(i-1)*ksfit(5)
         DO ifit=1,ksfit(5)
         K=K+1
         if(ifit.eq.1)then
            WRITE(IUN666,912)K,FIXED(NFIX1(1),1),I,IDRND1(I,mm2+1),
     &                      IFIT,NNVEC(K),RHSFIX(K),SOLNS(IEQNEW(K))
         ELSE
            WRITE(IUN666,9121)K,IFIT,SOLNS(IEQNEW(K))
         END IF
         END DO
         END DO
      END IF

c     animal effect(s)
      if(nanim.eq.0)return
      WRITE(IUN666,*)' '
      WRITE(IUN666,*)'-----------------------------'
      WRITE(IUN666,*)'SOLUTION FOR ANIMAL EFFECT(S)'
      WRITE(IUN666,*)'-----------------------------'
      WRITE(IUN666,914)

      call dx_open55 (iun55)
      write(iun55)ksfit(1),ksfit(2),nanim
      DO  I=1,NANIM
      K=LIM3+(i-1)*(ksfit(1)+ksfit(2))
      DO ifit=1,ksfit(1)
      K=K+1
      IF(Ifit.EQ.1)THEN
         WRITE(IUN666,912)K,'ADD.GENETIC',I,IDVEC(I),ifit,nnvec(k),
     *                     rhsfix(k),Solns(IEQNEW(K)),UVEC(i)
      ELSE
         WRITE(IUN666,9121)K,ifit,Solns(IEQNEW(K))
      END IF
      work(ifit)=solns(ieqnew(k))
      end do
c     ... second animal effect
      IF( IOPRN2.EQ.1 )THEN
         k=lim3+(i-1)*(ksfit(1)+ksfit(2))+ksfit(1)
         DO ifit=1,ksfit(2)
         K=K+1
         IF(ifit.eq.1)THEN
            WRITE(IUN666,912)K,FIXED( NFIX2(1),1),I,IDVEC(I),Ifit,
     *                      nnvec(k),rhsfix(k),Solns(IEQNEW(K))
         ELSE
            WRITE(IUN666,9121)K,ifit,solns(ieqnew(k))
         END IF
         work(ksfit(1)+ifit)=solns(ieqnew(k))
         end do
      END IF
      write(iun55)idvec(i),work( :ksfit(1)+ksfit(2) )
      end do
      write(*,*)'File DF55#DAT (binary) with estimates of genetic ',
     *'regression coefficients written !'
      if(irropt(1,1).ne.1)return ! calc EBVs only for legendre polynomials
      if(iinput.eq.1)then
         write(*,*)' '
         write(*,*)'Calculate breeding values for selected ages ?'
         call yndef(ii,0)
         if(ii.eq.1)call calc_ebvs
      end if
 
      RETURN

913   FORMAT(/' EQ.NO.',T30,'ORIG.ID.',T40,'TRAIT',2X,'NREC',6X,'MEAN'
     *,                                10X,'SOLUTION',11X,'LSQ-SOL.N')
 914  FORMAT(/' EQ.NO.',T30,'ORIG.ID.',T40,'TRAIT',2X,'NREC',6X,'MEAN'
     *,                                10X,'SOLUTION',10X,'INBREEDING')
911   FORMAT(I5,2X,A,T21,I5,    T40,I4,16X,         2G16.8)
912   FORMAT(I5,2X,A,T21,I5,I12,T40,I4, I6 ,F10.4, 2F16.6)
9121  FORMAT(I5,                T40,I4, 16x     , 2F16.6)
909   FORMAT(/1X,A,I4,4X,A12)
9     FORMAT(8X,A)

      contains

!     =====================
      subroutine read_iun11
!     =====================

      integer, dimension(8)                   :: nnped
      integer, dimension(:),allocatable       :: kint,kcov,knsert,kfix,
     &                                           kfix1
      integer, dimension(:,:), allocatable    :: nl
      integer                                 :: llq,ii,iopt,k,l

!      REWIND(IUN11)
      close(iun11)
      open(iun11,file='DF11#DAT',status='old',form='unformatted')
      read(iun11)fped,fdata,cwdir
      read(iun11)nnped
      READ(IUN11)IOPT,LLQ
      IF(IOPT.lt.3.or.iopt.gt.6)STOP 'MISMATCH WITH "DFPREP" !'

      allocate(kcov(llq),kfix(llq),kfix1(llq),kint(llq),knsert(llq),
     &                                                      stat=ii)
      if(ii>0)stop 'alloc read 11'

      READ(IUN11)KCOV
      READ(IUN11)KFIX,nfxreg
      READ(IUN11)KFIX1
      READ(IUN11)KINT
      READ(IUN11)KNSERT
      meff=maxval(kfix)+nfxreg
      allocate(nl(meff,llq),stat=ii)
      if(ii>0)stop 'alloc read_11 : nl'
      READ(IUN11)((NL(L,K),L=1,KFIX(K)+nfxreg), K=1,LLQ)

      READ(IUN11)(((IDFIX(M,L,K),M=1,NL(L,K)), L=1,KFIX(K)+nfxreg),
     *                                                    K=1,LLQ)
      READ(IUN11)(((NNFIX(M,L,K),M=1,NL(L,K)), L=1,KFIX(K)+nfxreg), 
     &                                                    K=1,LLQ)

      read(iun11)krand1
      IF(krand1>0)THEN
          allocate(nlrnd1(krand1),stat=ii)
          if(ii>0)stop 'read_iun11 : alloc 2'
          read(iun11)nlrnd1
          m=maxval(nlrnd1)
          allocate(idrnd1(m,krand1),stat=ii)
          if(ii>0)stop 'read_iun11 : alloc 2b'
          read(iun11)((idrnd1(m,l),m=1,nlrnd1(l)),l=1,krand1)
      END IF

      if(iopt.ge.4)then
         read(iun11)nmeta
         read(iun11)nage
         read(iun11)iiage
         read(iun11)nnage
      end if
      if(nanim>0)then
         READ(IUN11)KANIM,NREC2,IOPIBR
         read(iun11)idvec(:kanim)
         read(iun11)uvec(:kanim)
      end if
      CLOSE(IUN11)

      deallocate(kcov,kfix,kint,knsert,nl,stat=ii)
      if(ii>0)stop 'de-all iun11'

      end subroutine read_iun11

!     =======================================
      subroutine raw_means (iun52)
!     =======================================

      use read_iun52
      integer, intent(in)                     :: iun52

      integer, dimension (:), allocatable     :: ivec
      integer                                 :: nobs,nr,iobs,k,
     &                                           kk,ii

      allocate (ivec(neqns), stat=ii)
      if(ii>0)stop 'dxslw3 : alloc 3'

      xvec=0.d0
      ivec=0
      REWIND(IUN52)

2500  READ(IUN52,END=2599)II,NOBS,NR,NNA(:NOBS,:),nnq(:nobs),
     *   IICOL(:NR),
     *   ( (IEQ(K,L) ,K=1,NEFF(NNQ(L)) ),L=1,NOBS),
     *   ( (XCOV(K,L), K=kftfix(nnq(l))+1,NFR(NNQ(L)) ),L=1,NOBS),
     &    YVEC(:NOBS)

      DO IOBS=1,NOBS
      iq=nnq(iobs)
      DO K=1,NEFF(iq)
      KK=IICOL( IEQ(K,IOBS)-lim1 )
      IF(KK.GT.0)THEN
         IVEC(KK)=IVEC(KK)+1
         XVEC(KK)=XVEC(KK)+YVEC(IOBS)
      END IF
      end do
      end do
      GO TO 2500

2599  continue

!     class means ...
      nnvec=0
      rhsfix=0.d0
      do i=lim2+1,neqns
      k=ieqnew(i)
      if(k.gt.0.and.ivec(k).gt.0)then
         nnvec(i)=ivec(k)
         rhsfix(i)=ybar(1)+xvec(k)/float(ivec(k))
      end if
      end do

      deallocate(ivec,stat=ii)
      if(ii>0)stop 'dealloc dxslw3'

      end subroutine raw_means

!     ============================
      subroutine dx_open55 (iun55)
!     ============================

      integer, intent(out) :: iun55
      character(len=25)    :: fstand='DF55#DAT',UNFOR='UNFORMATTED',
     *                        UNKN='unknown'

      iun55=55
      CALL FCONCT(IUN55,FSTAND,UNFOR,UNKN)
      return
      END subroutine dx_open55

!     ======================
      subroutine orthreg_fix
!     ======================
      
      real(8)                              :: qq,pp
      integer                              :: i,ii

      if(nq>1)write(1,*)'Trait No.',iq,'  ',trait(iq)
!     evaluate fixed orthogonal regression for ages in the data
      do i=1,nage(1)
      if(nna(i,iq)<1)cycle 
!     ... GLS estimates
      qq=dot_product(reg(:kftfix(iq)),phi(i,:kftfix(iq),0,iq))+ybar(iq)
!     ... LSQ estimates (ignoring random effects)
      pp=dot_product(xvec(nfrst(iq)+1:nfrst(iq)+kftfix(iq)),
     &                                phi(i,:kftfix(iq),0,iq))+ybar(iq)
      write(1,'(i4,i7,3g12.5)')iiage(i,irrmet(0,iq)), nna(i,iq),
     &                                         ybar(iq)+y1(i,iq), pp,qq
      end do

      return
      end subroutine orthreg_fix

!     ====================
      subroutine calc_ebvs
!     ====================

      use legendre

      real(8), dimension(:), allocatable   :: ebv,aastar
      real(8), dimension(:,:), allocatable :: pphi
      integer                              :: k1,k2,kan,na,ia,kk,ii,ian
      real(8)                              :: xlow=-1.d0,xupp=1.d0,aamin
     &,                                       aamax,aa,pp

      open(1,file='EBVs_for_ages',status='unknown',form='formatted')

      rewind(iun55)
      read(iun55)k1,k2,kan
      kk=k1 
      kk0=0
      if(k2>0)then
         write(*,*)'estimates for 1st or 2nd animal effect ? (1/2) '
         call optdef(ii,1,2,1)
         if(ii.eq.2)then
            kk=k2
            kk0=k1
            write(1,'(1x,a/)')'estimates for "2nd" animal effect !'
         end if
      end if
      write(1,*)'order of polynomial fit =',kk

      write(*,*)'No. of ages to be evaluated ?'
      call optdef(na,1,100,1)
      allocate(aastar(na),ebv(na),pphi(na,kk),stat=ii)

      iq=1              ! use meta-meter for trait 1 -> assume same for
      im=irrmet(ii,iq)  ! all traits though no restriction in program !!
      write(*,'(1x,a,i6,a,i6)')'Valid range of ages is',iiage(1,im),
     &                         ' to',iiage(nage(im),im)
      aamin=iiage(1,im)
      aamax=iiage(nage(im),im)
      aa=(xupp-xlow)/(aamax-aamin)
      do i=1,na
      write(*,'(1x,a,i3,a)')'Value for age no.',i,'?    '
      call option(ia,iiage(1,im),iiage(nage(im),im))
      aastar(i)=xlow+aa*(ia-aamin)
      write(1,'(1x,a,i3,a,i6,a,f7.3)')'age no.',i,'  value =',ia,
     &                                     '  standardized =',aastar(i)
      end do
      write(1,*)' '

c     evaluate legendre polynomials at given ages
      do ifit=1,kk
      do i=1,na
      aa=aastar(i)
      pp=clgndr(1,ifit)
      if(aa.ne.0.d0)then
         do j=2,ifit
         if(clgndr(j,ifit).ne.0)pp=pp+clgndr(j,ifit)*aa**(j-1)
         end do
      end if
      pphi(i,ifit)=pp
      end do
      end do

      do i=1,kan
      read(iun55)ian,work(:k1+k2)
      do j=1,na
      ebv(j)=dot_product( pphi(j,:kk),work(kk0+1:kk0+kk) )
      end do
      write(1,'(i12,(t14,6f12.5))')ian,ebv
      end do
      
      close(1)
      write(*,*)'    Output file = "EBVs_for_ages" - Done ! '
      return
      end subroutine calc_ebvs

      END subroutine dxslw3
















