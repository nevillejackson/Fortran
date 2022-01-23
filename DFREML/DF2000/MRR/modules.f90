!     modules for "DxMRR"

!----------------------------Customize as required -------------------------
      MODULE params

!     these parameters set limits/array sizes in dfpre3 and dflsq3 (run option
!     -1) only, actual no.s are used later (i.e. can use oversize here)

      integer, parameter ::  MAXNQ=3       ! MAX. NO. OF TRAITS
      integer, parameter ::  MAXFIX=50     ! MAX. NO. OF FIX EFFECTS
      integer, parameter ::  MXCOMB=5210   ! MAX. NO. OF COMBINATIONS
      integer, parameter ::  MAXCOV=30     ! MAX. NO. OF COVARIABLES
      integer, parameter ::  MAXSPA=2000000
      integer, parameter ::  MXXZHZ=9999999 ! MAX. NO. FOR "MAXZHZ"
      integer, parameter ::  MAXXHX =500   ! MAX. NO. ROWS FOR "XHX"


!     this parameter determines how many likelihoods are stored in memory
      integer, parameter ::  MXFUNC=500 

!---------------------end of customizable section---------------------------

      integer, parameter ::  MAXNQQ = MAXNQ*(MAXNQ+1)/2
      integer, parameter ::  maxnq2=maxnq+maxnq
      integer, parameter ::  mxnqq2=maxnq2*(maxnq2+1)/2
      integer, parameter ::  maxage=maxnq

      END MODULE params

      MODULE sparse
         integer, save                            :: maxsub,maxlnz,iorder,&
     &                                               nsrow
         integer, dimension(:), allocatable, save :: ixvec1,ixvec2
         integer, dimension(:), allocatable, save :: ixsub
         integer, dimension(:), allocatable, save :: ivperm
      END MODULE sparse

      MODULE xsprse
         real(8), dimension (:), allocatable, save :: dia,xspars
      contains
         subroutine all_xsprse(nsrow,maxlnz)
         integer :: ii,nsrow,maxlnz
         allocate (dia(nsrow),stat=ii)
         if(ii>0)stop 'alloc : dia'
         allocate (xspars(maxlnz),stat=ii)
         print *,'xspars : maxlnz =',maxlnz,'  allocated'
         if(ii>0)stop 'alloc : xsprse'
         end subroutine all_xsprse
      END MODULE xsprse
      
      MODULE fmatrix
         real(8), dimension (:), allocatable       :: fdia,foff,fwrk
         contains
         subroutine all_fmatrix(nsrow,maxlnz)
         integer, intent(in) :: nsrow,maxlnz
         allocate (fdia(nsrow),fwrk(nsrow),foff(maxlnz),stat=ii)
         if(ii>0)stop 'FAIL : alloc fmatrix'
         end subroutine all_fmatrix
      END MODULE fmatrix

      MODULE names
         character(len=12), dimension (:), allocatable, save   ::  TRAIT
         character(len=14), dimension (:), allocatable, save   ::  PARAM
         character(len=14), dimension (:), allocatable, save   ::  PARVAR
         character(len=3),  dimension (:), allocatable, save   ::  REGTYP
         character(len=12), dimension (:,:), save, allocatable ::  FIXED
         character(len=12), dimension (:,:), save, allocatable ::  COVAR
         character(len=25), save                               ::  fped,fdata
         CHARACTER(len=50), save                               ::  cwdir
      contains
         subroutine all_names (nq,nparm,mfix,mcov)
         integer :: ii,nq,nparm,mfix,mcov
         allocate (trait(nq),param(nparm),parvar(nparm),FIXED(mfix+2,nq) &
     &,            covar(mcov,nq),regtyp(nparm),stat=ii)
         if(ii>0)stop 'alloc names'
         end subroutine all_names
      END MODULE names

      MODULE comments
         integer, save                         :: klines
         CHARACTER(len=80), dimension(:), save :: TEXT(6)
         CHARACTER(len=80)                     :: TTEXT
      END MODULE comments
   
      MODULE units
         integer, save :: iun11,iun12,iun13,iun14,iun22,iun23,iun44,iun45, &
     &                    iun47,iun49,iun51,iun52,iun53,iun54,iun59, &
     &                    iun66,iun67,iun16,iun17,iun18,iun19,iun15,iun28, &
     &                    iun88
         real(8), save :: ZERO,ONE,BIG,EIGZER,zz0=1.d-10, &
     &                    pi=3.1415926535898d0
         contains
         subroutine set_unitnos
         IUN11=11
         IUN12=12
         IUN13=13
         IUN14=14
         IUN15=15
         IUN16=16
         IUN17=17
         IUN18=18
         IUN19=19
         IUN22=22
         IUN23=23
         IUN28=28
         IUN44=44
         IUN45=45
         IUN47=47
         IUN49=49
         IUN51=51
         IUN52=52
         IUN53=53
         IUN54=54
         IUN59=59
         IUN66=66
         IUN67=67
         iun88=88
         return
         end subroutine set_unitnos
      END MODULE units

      MODULE grid
         real(8), save ::   TAU1,XLIK1,TAU2,XLIK2,TAU3,XLIK3
      END MODULE grid

      MODULE times
         integer, dimension(2),save :: ncalls,ntime
         real(8),dimension(2)       :: ATIME,BTIME
      END MODULE times


      MODULE derivs
         real(8),dimension(:),allocatable,save :: devc,dypy1,devl1,der1
         real(8),dimension(:),allocatable,save :: dypy2,devl2,der2
      contains
         subroutine all_derivs (nparm)
         integer, intent(in) :: nparm
         integer             :: ii,npp
         allocate(devc(nparm),dypy1(nparm),devl1(nparm),der1(nparm),stat=ii)
         if(ii>0)stop 'all_derivs : 1st'
         npp=nparm*(nparm+1)/2
         allocate(dypy2(npp),devl2(npp),der2(npp),stat=ii)
         if(ii>0)stop 'all_derivs : 2nd'
         end subroutine all_derivs
      END MODULE derivs

      MODULE iindex
         integer, save                               :: nvars
         integer, dimension (:), allocatable, save   :: nvec, nrvec, nvv
         integer, dimension (:,:), allocatable, save :: index
      END MODULE iindex

      MODULE legendre
         real(8),dimension(:,:), allocatable, save :: clgndr
         integer, save                             :: ioplgd=0
      contains
         subroutine all_legendre (nq)
         integer, intent(in) :: nq
         integer             :: ii
         if(ioplgd>0)return
         allocate(clgndr(nq,nq),stat=ii)
         if(ii>0)stop 'alloc : legendre'
         ioplgd=9
         end subroutine all_legendre 
      END MODULE legendre

      MODULE phimatrix
         use params
         real(8),dimension(:,:,:,:), allocatable, save :: phi
         integer, dimension(0:7,maxnq), save           :: irropt,irrmet
      END MODULE phimatrix

      MODULE sps_lists
         integer, save                          :: len1,len2
         integer, dimension(:),allocatable,save :: list1,list2
         real(8), save                          :: zero,small,logzero
      END MODULE sps_lists

      MODULE parmap
         integer, save                                 :: kparm,lparm,nparm
         integer, save                                 :: nrparm,iopeps, &
     &                                                    ilgeps,icorrf, &
     &                                                    kvfpe, kfoupe, &
     &                                                    kfoume,        &
     &                                                    ncpno, ilgpe, &
     &                                                    ngparm,ngpar1,ngpar2
         integer, dimension (:), allocatable, save     :: ntok,kton,ipm
         integer, dimension (:,:,:), allocatable, save :: nparno
         real(8), dimension (:), allocatable, save     :: ffvec,corrvec
         real(8), save                                 :: omegape, omegame,&
     &                                                    period
      END MODULE parmap

      MODULE ages
         integer, save                                 :: nmeta,mage,nanq, &
     &                                                    manq, ialog=-1
         integer, dimension (:), allocatable, save     :: nage,iavec,meage
         integer, dimension (:,:), allocatable, save   :: iiage
         integer, dimension (:,:,:), allocatable, save :: nnage
         real(8), dimension (:), allocatable, save     :: astar
      END MODULE ages

      MODULE sigmas
         real(8), dimension (:), allocatable, save   :: eiga,eigm,eigam,eigq,&
     &                                                  eigc,eige,eigr,eigp
         real(8), dimension (:,:), allocatable, save :: siga,sigm,sigam,sigq,& 
     &                                                  sigc,sige,sigr,sigp
         contains
         subroutine all_sigmas(n1,n3)
         integer, intent(in) :: n1,n3
         integer             :: ii,n2
         n2=n3
         if(n2.eq.0)n2=1
         allocate(siga(n1,n1),sigm(n1,n1),sigam(n1+n1,n1+n1),sigq(n1,n1),&
     &          sigc(n1,n1),sige(n2,n2),sigr(n1,n1),sigp(n1,n1),eiga(n1),&
     &          eigm(n1),eigam(n1+n1),eigc(n1),eigr(n1),eige(n2),eigp(n1),&
     &          eigq(n1),                                          stat=ii)
         if(ii>0)stop 'alloc sigmas'
         return
         entry deall_sigmas(n1,n3)
         deallocate (siga,sigm,sigam,sigc,sige,sigr,sigp,sigq,eiga,eigm, &
     &               eigam,eigc,eigr,eige,eigp,eigq,stat=ii)
         if(ii>0)stop 'dealloc sigmas'
         return
         end subroutine all_sigmas
      END MODULE sigmas

      MODULE correlations
         real(8), dimension (:), allocatable, save   :: rra,rrm,rrr,rrq, &
     &                                                  rrc,rre,rrp
         real(8), dimension (:,:), allocatable, save :: rram
      END MODULE correlations

      MODULE means
         real(8), dimension(:), allocatable, save   :: ybar,ymin,ymax, &
     &                                                 sdev,smin,smax,cvar
         real(8), dimension(:,:), allocatable, save :: cbar,csdev
      contains
         subroutine all_means (nq,ncov)
         allocate (ybar(nq),ymin(nq),ymax(nq),sdev(nq),smin(nq),smax(nq), &
     &             cvar(nq),cbar(ncov,nq),csdev(ncov,nq),stat=ii)
         if(ii>0)stop 'alloc : means '
         end subroutine all_means
      END MODULE means

      MODULE combinations
        integer, save                               :: ncomb
        integer, dimension (:), allocatable, save   :: nbrep,nxobs,nncom, &
     &                                                 mxobs
        integer, dimension (:,:), allocatable, save :: mmark

      contains

         subroutine all_combis (nq,mcomb)
         integer, intent(in) :: nq,mcomb
         integer             :: nqq
         nqq=nq*(nq+1)/2
         if(nq>1)then
            allocate (nbrep(nqq),nxobs(nq),nncom(1), &
     &                                           mxobs(nq), stat=ii)
         else
            allocate (nbrep(nqq),nxobs(nq),nncom(mcomb), &
     &                                           mxobs(nq), stat=ii)
         end if
         if(ii>0)stop 'alloc : combis '
         end subroutine all_combis

      END MODULE combinations


      MODULE likelihoods
         integer,save  :: LROUND,LCALL,LPSOUT,NROUND,MROUND,isecs
         real(8), dimension (:,:), allocatable, save ::  feval
      END MODULE likelihoods
     
      MODULE levels
         integer, dimension(:), save, allocatable   :: nfl,ncov,nfr,nfix, &
     &                                                 nfix1,nfix2,neff
         integer, dimension(:,:), save, allocatable :: npow,nlev
      contains
         subroutine all_levels_1 (nq)
         integer :: nq,ii
         allocate (nfl(nq),ncov(nq),nfr(nq),nfix(nq),nfix1(nq),nfix2(nq), &
     &             neff(nq),stat=ii)
         if(ii>0)stop 'levels_1 : alloc'
         end subroutine all_levels_1

         subroutine all_levels_2 (nq,mcov,mfix)
         integer :: nq,mcov,mfix,ii
         allocate (npow(mcov,nq),nlev(mfix+2,nq),stat=ii)
         if(ii>0)stop 'levels_2 : alloc'
         end subroutine all_levels_2 
      END MODULE levels

      MODULE numbers
         use params
         integer, save                           :: NTRAIT,NCALL,IPVRUN, &
     &                                              KRCZHZ, NQQ,NQQ2, &
     &                                              NQSQ,NRCZHZ
         integer, save                           :: mcov,mfix,mobs,mnfr,mqhq   
         integer, dimension(5), save             :: istrt
         integer, dimension(7,maxnq), save       :: kfit
         integer, dimension(7), save             :: ksfit
         integer, dimension(31), save            :: nosvec, kosvec
         integer, dimension(:), allocatable,save :: nfrst, nboth,mmr
         integer, dimension(:,:),allocatable,save:: nnrec
         integer, dimension(maxnq)               :: kftfix

         integer, save                           :: NREC,NDATA,nq,NQ111, &
     &                                              NQ222,NANIM,NFLM,NFRM,&
     &                                              IRPMOD,LIM1,LIM2,LIM3,&
     &                                              NEQNS,IOPRN1,IOPRN2,lim3a,&
     &                                              IOPCOV,IMODEL,NRAND1, &
     &                                              jparam,iosrch,iopmod, &
     &                                              kfitmx,kopt,mfitmx,&
     &                                              ionrm1,ioprn3,nq333, &
     &                                              nrand3,ieqmod,nfxreg
      contains

         subroutine all_numbers (kq,kqq)
         integer, intent(in) :: kq,kqq
         integer             :: ii
         allocate (nfrst(kq),nboth(kqq),stat=ii)
         if(ii>0)stop 'numbers : alloc'
         end subroutine all_numbers

         subroutine set_nos
         NREC=NOSVEC(1)
         NDATA=NOSVEC(2)
         nq=nosvec(3)
         NQ111=NOSVEC(4)
         NQ222=NOSVEC(5)
         NANIM=NOSVEC(6)
         NFLM=NOSVEC(7)
         NFRM=NOSVEC(8)
         IRPMOD=NOSVEC(9)
         LIM1=NOSVEC(10)
         LIM2=NOSVEC(11)
         LIM3=NOSVEC(12)
         NEQNS=NOSVEC(13)
         IOPRN1=NOSVEC(14)
         IOPRN2=NOSVEC(15)
         IOPCOV=NOSVEC(16)
         IMODEL=NOSVEC(17)
         NRAND1=NOSVEC(18)
         NRAND3=NOSVEC(19)
         jparam=nosvec(22)
         iosrch=nosvec(23)
         iopmod=nosvec(24)
         kfitmx=nosvec(25)
         kopt=nosvec(26)
!         ionrm1=nosvec(27)
         nq333=nosvec(28)
         lim3a=nosvec(29)
         ioprn3=nosvec(30)
         ieqmod=nosvec(31)
         end subroutine set_nos

         subroutine set_nosvec
         nosvec=0
         NOSVEC(1)=NREC
         NOSVEC(2)=NDATA
         nosvec(3)=nq
         NOSVEC(4)=NQ111
         NOSVEC(5)=NQ222
         NOSVEC(6)=NANIM
         NOSVEC(7)=NFLM
         NOSVEC(8)=NFRM
         NOSVEC(9)=IRPMOD
         NOSVEC(10)=LIM1
         NOSVEC(11)=LIM2
         NOSVEC(12)=LIM3
         NOSVEC(13)=NEQNS
         NOSVEC(14)=IOPRN1
         NOSVEC(15)=IOPRN2
         NOSVEC(16)=IOPCOV
         NOSVEC(17)=IMODEL
         NOSVEC(18)=NRAND1
         nosvec(19)=nrand3
         nosvec(22)=jparam
         nosvec(23)=iosrch
         nosvec(24)=iopmod
         nosvec(25)=kfitmx
         nosvec(26)=kopt
!         nosvec(27)=ionrm1
         nosvec(28)=nq333
         nosvec(29)=lim3a
         nosvec(30)=ioprn3
         nosvec(31)=ieqmod
         end subroutine set_nosvec

      END MODULE numbers

      MODULE adjacency
         integer, dimension(:), allocatable, save :: kfirst,kvcol,knext
      END MODULE adjacency

      MODULE order
         integer, save                            :: nfill
         integer, dimension(:), allocatable, save :: ieqnew
      END MODULE order

      MODULE traces
         real(8), dimension (:), save, allocatable   :: trran1
         real(8), dimension (:,:), save, allocatable :: trres1
      END MODULE traces
        
      MODULE dmatrices
         real(8), dimension (:,:,:,:), save, allocatable :: rd
         real(8), dimension (:,:,:), save, allocatable   :: rdr,rd1,td,tdt,rd2
         real(8), dimension (:),  allocatable            :: dr
         logical, dimension(:,:), allocatable            :: de,d
      contains
!        ===============================================================
         subroutine all_dmatrices (nq,nq2,mobs,nrparm,ngparm,ncomb,mcomb)
!        ===============================================================
         use numbers, only : ieqmod
         use parmap, only : ngpar1, ngpar2
         integer, intent(in) :: nq,nq2,mobs,nrparm,ngparm,ncomb,mcomb
         integer             :: ii,mm,nqq2
         mm=mobs*(mobs+1)/2
         nqq2=nq2*(nq2+1)/2
         allocate(de(mobs*(mobs+1)/2,nrparm),d(nqq2,ngparm),stat=ii)
         if(ii>0)stop 'all_dmatrices : d '
         allocate (rd1(mobs,mobs,nrparm),stat=ii)
         if(ii>0)stop 'all_dmatrices : rd1'
         if(ieqmod.eq.1)then
            allocate (dr(mm),rd2(mobs,mobs,ngpar2-ngpar1),stat=ii)
            if(ii>0)stop 'all_dmatrices : rd2'
         end if
         if(nq.eq.1 .and. ncomb.le.mcomb)then
            allocate (rd(mobs,mobs,nrparm,ncomb),stat=ii)
            if(ii>0)stop 'all_dmatrices : rd'
            allocate (rdr(mm,nrparm,ncomb),stat=ii)
            if(ii>0)stop 'all_dmatrices : rdr'
         end if
         allocate (td(nq2,nq2,ngparm),tdt(ngparm,nq2,nq2),stat=ii)
         if(ii>0)stop 'all_dmatrices : td'
         end subroutine all_dmatrices 

      END MODULE dmatrices

      MODULE like_components
         real(8),save                             :: yry,vvy,ypy,det,detl
         real(8),save                             :: deta,detm,detam,detc,detq
         real(8), dimension(:), allocatable, save :: dete
         integer, save                            :: nrank,nrnkx,nneg,npsout
         integer, dimension(:), allocatable, save :: iflag
      END MODULE like_components

      MODULE residuals
         integer, parameter                            :: max_comb=50
         real(8), save                                 :: zigeps
         real(8), dimension (:,:), allocatable, save   :: ee
         real(8), dimension (:,:,:), allocatable, save :: eei
         real(8), dimension(:,:), allocatable, save    :: sig
         real(8), dimension(:), allocatable,save       :: work,rxkk,pevec,&
     &                                                    rhomin,rhomax,ww
         real(8), dimension(:,:), allocatable,save     :: eevec,pe1vec,ee1vec
      END MODULE residuals

      MODULE current
         real(8),dimension(:),save,allocatable :: currnt
         integer, save                         :: ilkpre,nreuse
      END MODULE current

      MODULE solutions
         real(8),dimension(:),allocatable, save :: solns,savsol
         real(8), save                          :: ffmin
      END MODULE solutions

      MODULE zero_rows
         integer, save                            :: NRZERO
         integer, dimension(:), allocatable, save :: KRZERO
      END MODULE zero_rows

      MODULE eigen_decomp
         integer, save                            :: kkparm,kparmold
         integer, dimension(7), save              :: kfteig
         integer, dimension(:,:),allocatable,save :: indx,kindx
      END MODULE eigen_decomp

      MODULE mme3
         integer, dimension(:), allocatable, save :: itrait,nr8
      contains
         subroutine all_mme3 (nq)
         integer :: nq,ii
         allocate( itrait(nq),nr8(nq),stat=ii)
         if(ii>0)stop 'all_mme3'
         end subroutine all_mme3
      END MODULE mme3

      MODULE eigen_wrk
         real(8), dimension (:), allocatable, save   :: eig, ework
         real(8), dimension (:,:), allocatable, save :: vv
         contains
         subroutine all_eigen(nq)
         integer, intent(in) :: nq
         allocate(eig(nq),ework(nq*(nq+1)/2),vv(nq,nq),stat=ii)
         if(ii>0)stop 'alloc eigen'
         return
         entry deall_eigen
         deallocate(eig,ework,vv,stat=ii)
         if(ii>0)stop 'de-alloc eigen'
         return
         end subroutine all_eigen
      END MODULE eigen_wrk

      MODULE hess_wrk
         integer, dimension (:),allocatable     :: iw
         real(8), dimension (:), allocatable    :: hess, w1,w2
         contains
         subroutine all_hessian(kparm)
         allocate(hess(kparm*(kparm+1)/2),w1(kparm),w2(kparm),iw(kparm), &
     &                                                         stat=ii)
         if(ii>0)stop 'alloc hessian_wrk'
         return
         entry deall_hessian(kparm)
         deallocate(hess,w1,w2,iw,stat=ii)
         if(ii>0)stop 'dealloc hessian_wrk'
         return
         end subroutine all_hessian
      END MODULE hess_wrk

      MODULE parameters
         integer, save                            :: mxparm
         real(8), dimension(:), allocatable, save :: xvec, start,save,save1
      END MODULE parameters

      MODULE read_iun52
         real(8), dimension (:,:),allocatable    :: xcov
         real(8), dimension (:), allocatable     :: yvec
         INTEGER, DIMENSION (:), ALLOCATABLE     :: nnq
         integer, dimension (:,:), allocatable   :: ieq,nna
         integer, dimension(:), allocatable      :: iicol
         integer, save                           :: iall52
         contains
         subroutine all_read52(nneff,nnfr,mobs,mqhq,nmeta)
         integer, intent(in) :: nneff,nnfr,mobs,mqhq
         allocate (xcov(nnfr,mobs),yvec(mobs),ieq(nneff,mobs),nnq(mobs), &
     &             nna(mobs,nmeta),iicol(mqhq),stat=ii)
         if(ii>0)stop 'alloc  read_iun52'
         iall52=1
         end subroutine all_read52
      END MODULE read_iun52
