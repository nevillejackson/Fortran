!=============================================================================
      subroutine dxslw3 (iopslv,iinput)
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

!     arguments
      integer, intent(in)                     :: iinput
      integer, intent(inout)                  :: iopslv

!     local variables
      integer                                 :: lim0=1,ii,nobs,nr
      real(8)                                 :: xx
      integer, dimension(8)                   :: nnped
      integer, dimension(:),allocatable       :: kfix
      integer, dimension(:),allocatable       :: ivec,nnvec,icol,idvec
      real(8), dimension(:),allocatable       :: xvec,rhsfix,uvec
      real(8), dimension (:), allocatable     :: yvec
      integer, dimension (:), allocatable     :: nnq,nlrnd1
      integer, dimension(:,:), allocatable    :: ieq,idrnd1,nl
      integer, dimension (:,:,:), allocatable :: idfix
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate(ivec(neqns),icol(neqns),nnvec(neqns),xvec(neqns),
     *         rhsfix(neqns),uvec(nanim),
     *         idvec(nanim),stat=ii)
      if(ii>0)stop 'dxslw3 : alloc'

      allocate (yvec(mobs),nnq(mobs),ieq(mfix+3,mobs),stat=ii)
      if(ii>0)stop 'dxslw3 : alloc 3'

c     set to 1 if obs. have been scaled by phen.sd
      iscale=0

C     INTERACTIVE INPUT FOR RUN OPTION
      if(iinput.eq.1)then
         write(*,'(/a)')' write out solutions for ...'
         write(*,9)'1  ...  regression coeff.s/fixed effects only'
         write(*,9)'2  ...  all effects '
         CALL OPTDEF(IOPSLV,1,2,1)
      end if

C     COUNT NO. OF RECORDS PER EFFECT & ACCUMMULATE RAW TOTALS
      if(iopslv.eq.2)then
         ivec=0
         xvec=0.d0
         REWIND(IUN52)

2500     READ(IUN52,END=2599)ii,NOBS,NR,NNQ(:NOBS),
     *      ICOL(:NR),( (IEQ(K,L),K=1,NEFF(NNQ(L)) ),L=1,NOBS),
     *    ( ( xx ,K=1,NFR(NNQ(L)) ),L=1,NOBS),YVEC(:NOBS)

         DO IOBS=1,NOBS
         DO K=1,NEFF( NNQ(IOBS) )
         KK=ICOL( IEQ(K,IOBS)-lim1 )
         IF(KK.GT.0)THEN
            iVEC(KK)=iVEC(KK)+1
            xvec(KK)=xvec(KK)+YVEC(IOBS)
         END IF
         end do
         end do
         GO TO 2500

2599     continue
         do i=lim2+1,neqns
         k=ieqnew(i)
         if(k.gt.0)then
           nnvec(i)=ivec(k)
           rhsfix(i)=xvec(k)
         else
           nnvec(i)=0
           rhsfix(i)=0.d0
         end if
         end do
      end if

c     read lsq solutions etc. from unit "13"
      read(iun13)lim2
      read(iun13)nnvec(1:lim2)
      read(iun13)rhsfix(1:lim2)
      read(iun13)xvec(1:lim2)

C     INPUT FROM UNIT "11"
      
      REWIND(IUN11)
      read(iun11)fped,fdata,cwdir
      read(iun11)nnped
      READ(IUN11)IOPT,LLQ
!      IF(IOPT.lt.3.or.iopt.gt.4)STOP 'MISMATCH WITH "DFPREP" !'
      allocate(kfix(llq), stat=ii)
      READ(IUN11) (ii, i=1,llq)
      READ(IUN11)KFIX
      READ(IUN11) (ii, i=1,llq)
      READ(IUN11) (ii, i=1,llq)
      READ(IUN11) (ii, i=1,llq)
      allocate(nl(maxval(kfix),llq),stat=ii)
      if(ii>0)stop 'alloc NL'
      READ(IUN11)((NL(L,K),L=1,KFIX(K)), K=1,LLQ)

      mnfl=maxval(nl)
      allocate(idfix(mnfl,mfix,nq),stat=ii)
      if(ii>0)stop 'dxslw3 : alloc 2'

      READ(IUN11)(((IDFIX(M,L,K),M=1,NL(L,K)), L=1,KFIX(K)), K=1,LLQ)
      READ(IUN11)((( ii ,M=1,NL(L,K)), L=1,KFIX(K)), K=1,LLQ)

      read(iun11)krand1
      IF(krand1>0)THEN
         allocate(nlrnd1(krand1),stat=ii)
         read(iun11)nlrnd1
         mm=maxval(nlrnd1)
         allocate(idrnd1(mm,krand1),stat=ii)
         if(ii>0)stop 'read_iun11 : alloc 2'
         read(iun11)((idrnd1(m,l),m=1,nlrnd1(l)),l=1,krand1)
      END IF

      if(iopt.eq.4)then
         read(iun11)nage
         read(iun11)iiage(:nage)
         read(iun11)((nnage(m,k),m=1,nage), k=1,llq)
      end if
      READ(IUN11)KANIM !,nrec2,IOPIBR
      read(iun11)idvec(:kanim)
      read(iun11)uvec(:kanim)
      CLOSE(IUN11)

      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'------------------------------------------'
      WRITE(IUN66,*)'SOLUTION FOR FIXED EFFECTS AND COVARIABLES'
      WRITE(IUN66,*)'------------------------------------------'
      SD=1.D0
      CSD=1.d0

C     COVARIABLES
      K=LIM0
      DO  IQ=1,NQ
      IF(NCOV(IQ).gt.0)WRITE(IUN66,913)
      if(iscale.eq.1)SD=SDEV(IQ)
      DO  I=1,NCOV(IQ)
      WRITE(IUN66,909)'COVARIABLE NO.',I,COVAR(I,IQ)
      IF(ISCALE.EQ.1)CSD=CSDEV(I,IQ)
      DO  J=1,NPOW(I,IQ)
      K=K+1
      UU=SOLNS(IEQNEW(K))*SD/CSD
      WRITE(IUN66,911)K,'ORDER',J,IQ,UU,xvec(k)
      end do
      end do
      end do

c     fixed effects
      DO  IQ=1,NQ
      if(iscale.eq.1)SD=SDEV(IQ)
      IF(NFIX(IQ).gt.0)WRITE(IUN66,913)
      DO I=1,NFIX(IQ)
      WRITE(IUN66,909)'FIXED EFFECT NO.',I,FIXED(I,IQ)
      DO J=1,NLEV(I,IQ)
      K=K+1
      n=nnvec(k)
      xx=rhsfix(k)
      KK=IEQNEW(K)
      UU=SOLNS( KK )*SD
      WRITE(IUN66,912)K,'LEVEL',J,IDFIX(J,I,IQ),IQ,N,XX,UU,xvec(k)
      end do
      end do
      end do

      IF(iopslv.eq.1)RETURN ! fixed effects only required

c     additional random effect
      IF(IOPRN1.EQ.1)THEN
         WRITE(IUN66,*)' '
         WRITE(IUN66,*)'-------------------------------------'
         WRITE(IUN66,*)'SOLUTION FOR ADDITIONAL RANDOM EFFECT'
         WRITE(IUN66,*)'-------------------------------------'
         WRITE(IUN66,914)
         K=LIM2
         DO I=1,NLRND1(1)
         DO IQ=1,NQ
         IF( NFIX1(IQ).GT.NFIX(IQ)) THEN
            if(iscale.eq.1)SD=SDEV(IQ)
            K=K+1
            N=NNVEC(K)
            XX=RHSFIX(K)*SD
            IF(N.GT.0)XX=YBAR(IQ)+ XX/DBLE(N)
            KK=IEQNEW(K)
            UU=Solns(KK)*SD
            WRITE(IUN66,912)K,FIXED(NFIX1(IQ),IQ),I,IDRND1(I,1),
     &                                                     IQ,N,xx,uu
         END IF
         end do
         end do
      END IF

      IF(IOPRN3.EQ.1)THEN
         WRITE(IUN66,*)' '
         WRITE(IUN66,*)'-----------------------------------------'
         WRITE(IUN66,*)'SOLUTION FOR 2nd ADDITIONAL RANDOM EFFECT'
         WRITE(IUN66,*)'-----------------------------------------'
         WRITE(IUN66,914)
         K=LIM3a
         DO I=1,NRAND3
         DO IQ=1,NQ
         IF( NFIX3(IQ).GT.NFIX1(IQ)) THEN
            if(iscale.eq.1)SD=SDEV(IQ)
            K=K+1
            N=NNVEC(K)
            XX=RHSFIX(K)*SD
            IF(N.GT.0)XX=YBAR(IQ)+ XX/DBLE(N)
            KK=IEQNEW(K)
            UU=Solns(KK)*SD
            WRITE(IUN66,912)K,FIXED(NFIX3(IQ),IQ),I,IDRND1(I,2),IQ,N,
     &                                                         xx,uu
         end if
         end do
         end do
      END IF

c     animal effect(s)
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'-----------------------------'
      WRITE(IUN66,*)'SOLUTION FOR ANIMAL EFFECT(S)'
      WRITE(IUN66,*)'-----------------------------'
      WRITE(IUN66,914)
      K=LIM3
      DO  I=1,NANIM
      DO IQ=1,NQ
      if(iscale.eq.1)SD=SDEV(IQ)
      K=K+1
      N=NNVEC(K)
      XX=RHSFIX(K)*SD
      IF(N.GT.0)XX=YBAR(IQ)+ XX/DBLE(N)
      KK=IEQNEW(K)
      if(kk.gt.0)then
         UU=Solns(KK)*SD
      else
         uu=0.d0
      end if
      IF(IQ.EQ.1)THEN
         WRITE(IUN66,915)K,'ADD.GENETIC',I,IDVEC(I),IQ,N,XX,UU,UVEC(i)
      ELSE
         WRITE(IUN66,9121)K,IQ,N,XX,UU
      END IF
      end do
c     ... second animal effect
      IF( IOPRN2.EQ.1 )THEN
         DO IQ=1,NQ
         L=K+1
         IF(NFIX2(IQ).GT.NFIX3(IQ))THEN
            if(iscale.eq.1)SD=SDEV(IQ)
            K=K+1
            XX=RHSFIX(K)*SD
            IF(Nnvec(k).GT.0)XX=YBAR(IQ)+ XX/DBLE(Nnvec(k))
            KK=IEQNEW(K)
            if(kk.gt.0)then
              UU=Solns(KK)*SD
            else
              uu=0.d0
            end if
            IF(K.EQ.L)THEN
               WRITE(IUN66,915)K,FIXED( NFIX2(IQ),IQ),I,IDVEC(I),IQ,
     *                                               NNVEC(K),XX,UU
            ELSE
               WRITE(IUN66,9121)K,IQ,NNVEC(K),XX,UU
            END IF
         END IF
         end do
      END IF
      end do

      write(*,*)'Calculate residuals ?'
      call yndef(ii,1)
      if(ii.eq.1)call residuals     

      RETURN

913   FORMAT(/' EQ.NO.',T30,'ORIG.ID.',T40,'TRAIT',2X,'NREC',5X,'MEAN'
     *,                                8X,'SOLUTION',9X,'LSQ-SOL.N')
 914  FORMAT(/' EQ.NO.',T30,'ORIG.ID.',T40,'TRAIT',2X,'NREC',5X,'MEAN'
     *,                                8X,'SOLUTION',7X,'INBREEDING')
911   FORMAT(I5,2X,A,T21,I5,    T40,I4,16X,         2G16.8)
912   FORMAT(I5,2X,A,T21,I5,I12,T40,I4, I6 ,F10.4, 2F16.6)
 915  FORMAT(I5,2X,A,T21,I5,I12,T40,I4, I6 ,F10.4, F16.6,f16.2)
9121  FORMAT(I5,                T40,I4, I6 ,F10.4, 2F16.6)
909   FORMAT(/1X,A,I4,4X,A12)
9     FORMAT(8X,A)

      contains

!     ====================
      subroutine residuals
!     ====================

      use mme3
      real(8), dimension(:), allocatable    :: rvec,adj
      integer, dimension(:), allocatable    :: ifix,nfsub1,nfsub2,nfsub3
      integer, dimension (:,:), allocatable :: iposfx
      real(8)                               :: yy,res, c,cc
      integer                               :: mr8,madj,ii,n,iq,mfix,i,
     &                                         irow, ianim, j,k,jq
      character(len=25)                     :: fstand, old='old',
     &                                         unfor='unformatted',
     &                                         unkn='unknown'

      fstand='DF22#DAT'
      call fconct(iun22,fstand,unfor,old) 
      fstand='DF55#DAT'
      call fconct(iun55,fstand,unfor,unkn) 

      mr8=maxval(nr8)
      mfix=maxval(nfix2)
      madj=mfix+maxval(nfr)+3
      allocate(ifix(mfix),iposfx(mfix,nq),nfsub1(nq),nfsub2(nq),
     &           nfsub3(nq),rvec(mr8),adj(madj),stat=ii)
      if(ii>0)stop 'dxslw3 : residuals '

      K=LIM1
      DO IQ=1,NQ
      DO I=1,NFIX(IQ)
      IPOSFX(I,IQ)=K
      K=K+NLEV(I,IQ)
      END do
      end do

      NQ2=NQ+NQ222
      nfsub1=0
      if(ioprn1.eq.1)then
         do iq=1,nq
         do jq=1,iq-1
         if(nfix(jq).eq.nfix1(jq))nfsub1(iq)=nfsub1(iq)+1
         end do
         end do
      end if
      nfsub3=0
      if(ioprn3.eq.1)then
         do iq=1,nq
         do jq=1,iq-1
         if(nfix1(jq).eq.nfix3(jq))nfsub3(iq)=nfsub3(iq)+1
         end do
         end do
      end if

      nfsub2=0
      if(ioprn2.eq.1)then
         do iq=1,nq
         do jq=1,iq-1
         if(nfix3(jq).eq.nfix2(jq))nfsub2(iq)=nfsub2(iq)+1
         end do
         end do
      end if
      nn=0
50    READ(IUN22,END=99)IQ, ianim, IFIX(:NFIX2(IQ) ),RVEC(:NR8(IQ) )
      if(nquni>0.and.iq.ne.nquni)go to 50
      nn=nn+1

!     observation
      YY= RVEC (ITRAIT(IQ))! -YBAR(IQ) 
      n=0

!     adjust for covariables
      irow=nfrst(iq)
      DO I=1,NCOV(IQ)
      C= RVEC(I)-CBAR(I,IQ) 
      CC=1.D0
      DO J=1,NPOW(I,IQ)
      CC=CC*C
      irow=irow+1
      n=n+1
      adj(n)=cc*solns( ieqnew(irow) )
      end do
      end do

!     fixed effects
      do i=1,nfix(iq)
      irow = ieqnew( IPOSFX(i,IQ)+IFIX(i) )
      n=n+1
      adj(n)= solns( irow )
      end do

!     additional random effect(s)
      if(ioprn1.eq.1)then
         M=NFIX1(IQ)
         IF(M>NFIX(IQ))then
            irow=lim2+(ifix(m)-1)*nq111+iq-nfsub1(iq)
            n=n+1
            adj(n)=solns( ieqnew(irow) )
         end if
         if(ioprn3.eq.1)then
            m=nfix3(iq)
            if(m>nfix1(iq))then
              irow=LIM3a+(IFIX(M)-1)*NQ333+IQ-nfsub3(iq)
              n=n+1
              adj(n)=solns( ieqnew(irow) )
            end if
         end if
      end if

C     RANDOM EFFECT(S) FOR ANIMALS
      N2=NFIX2(IQ)
      IF(N2>nfix3(iq))then
         irow=LIM3+(IFIX(N2)-1)*NQ2+NQ+IQ-nfsub2(iq)
         n=n+1
         adj(n)=solns( ieqnew(irow) )
      end if
      irow=LIM3+(IANIM-1)*NQ2+IQ
      n=n+1
      adj(n)=solns( ieqnew(irow) )
      res=yy-sum(adj(:n))
      write(iun55)IQ,ianim,yy,res,n,adj(:n),rvec(:ncov(iq)),
     &                                           ifix(:nfix2(iq))

      GO TO 50

 99   write(*,*)'No. of records processed =',nn
      write(*,*)'Residuals written to unformatted file : ',fstand
      close (iun55)
      return
      end subroutine residuals

      END subroutine dxslw3









