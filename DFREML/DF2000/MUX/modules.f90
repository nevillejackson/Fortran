!     Modules for DXMUX

!-----------------------------Customize as required -------------------------
      MODULE params

!     these parameters set limits/array sizes in dfpre3 and dflsq3 (run option
!     -1) only, actual no.s are used later (i.e. can use oversize here)
      integer, parameter ::  mxparm=200   ! Max. no. of parameters
      integer, PARAMETER ::  MAXNQ=12    ! MAX. NO. OF TRAITS
      integer, parameter ::  MAXFIX=25   ! MAX. NO. OF FIXED EFFECTS
      integer, parameter ::  MXCOMB=599   ! MAX. NO. OF COMBINATIONS OF TRAITS
      integer, parameter ::  MAXCOV=15   ! MAX. NO. OF COVARIABLES
      integer, parameter ::  MAXSPA=250000

!     this parameter determines how many likelihoods are stored in memory
      integer, parameter ::  MXFUNC=300 

!---------------------end of customizable section---------------------------

      integer, parameter ::   MAXNQQ = MAXNQ*(MAXNQ+1)/2
      integer, parameter ::   maxnq2=maxnq+maxnq
      integer, parameter ::   mxnqq2=maxnq2*(maxnq2+1)/2
      integer, parameter ::   maxage=maxnq
      integer, parameter ::   MXPAR1=MXPARM+1
      integer, parameter ::   mxxpar=mxparm*(mxparm+1)/2 

      END MODULE params

      MODULE sparse
         integer, save                            :: maxsub,maxlnz,iorder,nsrow
         integer, dimension(:), allocatable, save :: ixvec1,ixvec2
         integer, dimension(:), allocatable, save :: ixsub
         integer, dimension(:), allocatable, save :: ivperm
      END MODULE sparse

      MODULE xsprse
         real(8), dimension (:), allocatable, save :: dia,xspars
      contains
         subroutine all_xsprse(nsrow,maxlnz)
         integer                                   :: ii,nsrow,maxlnz
         allocate (dia(nsrow),stat=ii)
         if(ii>0)stop 'alloc : dia'
         allocate (xspars(maxlnz),stat=ii)
         print *,'xspars : maxlnz =',maxlnz,'  allocated'
         if(ii>0)stop 'alloc : xsprse'
         end subroutine all_xsprse
      END MODULE xsprse
      
      MODULE names
         character(len=12), dimension (:), allocatable, save   ::  TRAIT
         character(len=12), dimension (:), allocatable, save   ::  PARAM
         character(len=12), dimension (:), allocatable, save   ::  PARVAR
         character(len=12), dimension (:,:), save, allocatable ::  FIXED
         character(len=12), dimension (:,:), save, allocatable ::  COVAR
         CHARACTER(len=25), save                               :: fped,fdata
         CHARACTER(len=50), save                               :: cwdir
      contains
         subroutine all_names (nq,nparm,mfix,mcov)
         integer :: ii,nq,nparm,mfix,mcov
         allocate (trait(nq),param(nparm),parvar(nparm),          &
     &             FIXED(mfix+2,nq),COVAR(mcov,nq),stat=ii)
         IF(ii>0)STOP 'all_names'
         end subroutine all_names
      END MODULE names

      MODULE comments
         INTEGER, save                         ::  klines
         CHARACTER(len=80), dimension(:), save ::  TEXT(6)
         CHARACTER(len=80)                     ::  TTEXT
      END MODULE comments
   
      MODULE units
         integer, save ::iun11,iun12,iun13,iun14,iun22,iun23,iun44,iun45, &
     &                   iun47,iun49,iun51,iun52,iun53,iun54,iun55,iun59, &
     &                   iun66,iun67,iun16,iun17,iun18,iun19
         real(8), save :: ZERO,ONE,EIGZER,BIG,zz0=1.d-10
         contains
         subroutine set_unitnos
         IUN11=11; IUN12=12; IUN13=13; IUN14=14; IUN16=16
         IUN17=17; IUN18=18; IUN19=19; IUN22=22; IUN23=23
         IUN44=44; IUN45=45; IUN47=47; IUN49=49; IUN51=51
         IUN52=52; IUN53=53; IUN54=54; IUN59=59; IUN66=66
         IUN67=67; iun55=55
         return
         end subroutine set_unitnos
      END MODULE units

      MODULE grid
         real(8), save ::   TAU1,XLIK1,TAU2,XLIK2,TAU3,XLIK3
      END MODULE grid

      MODULE times
         integer, dimension(2),save  :: ncalls,ntime
         real(8), dimension(2),save  :: BTIME
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
         integer, dimension (:), allocatable, save   :: nvec, nrvec
         integer, dimension (:,:), allocatable, save :: index
      contains
         subroutine all_iindex (nq)
         integer                                     :: ii,nq4
         nq4=nq+4
         allocate(nvec(nq4),nrvec(nq4),index(nq,nq4),stat=ii)
         if(ii>0)stop 'all_iindex'
         end subroutine all_iindex 
      END MODULE iindex

      MODULE legendre
         real(8), dimension(:,:), allocatable, save :: clgndr
      contains
         subroutine all_legendre (nq)
         integer                                    :: ii
         allocate(clgndr(nq,nq),stat=ii)
         if(ii>0)stop 'alloc : legendre'
         end subroutine all_legendre 
      END MODULE legendre

      MODULE phimatrix
         real(8), dimension(:,:), allocatable, save :: phi,phi2
      contains
         subroutine all_phimatrix (nq)
         integer                                    :: ii,nqq
         nqq=nq*(nq+1)/2
         allocate(phi(nq,nq),phi2(nqq,nqq),stat=ii)
         if(ii>0)stop 'alloc : phimatrix'
         end subroutine all_phimatrix
      END MODULE phimatrix

      MODULE sps_lists
         integer, save                          :: len1,len2
         integer, dimension(:),allocatable,save :: list1,list2
         real(8), save                          :: zero,small,logzero
      END MODULE sps_lists

      MODULE parmap
         integer, save                                 :: kparm,iomease, &
     &                                                    nrparm,ngparm, &
     &                                                    ngpar1,ngpar2
         integer, dimension (:), allocatable, save     :: ntok,kton,ipm
         integer, dimension (:,:,:), allocatable, save :: nparno
         real(8), dimension (:), allocatable, save     :: ffvec
      END MODULE parmap

      MODULE ages
         integer, save                               :: nage
         integer, dimension (:), allocatable, save   :: iiage
         integer, dimension (:,:), allocatable, save :: nnage
         real(8), dimension (:,:), allocatable, save :: xxage
      END MODULE ages

      MODULE sigmas
         real(8), dimension (:), allocatable, save :: eiga,eigm,eigam, &
     &                          eigc,eige,eigr,eigp,eigq
         real(8), dimension (:,:), allocatable, save :: siga,sigm,sigam,& 
     &                          sigc,sige,sigr,sigp,sigq
      END MODULE sigmas

      MODULE correlations
         real(8), dimension (:), allocatable, save   :: rra,rrm,rrr, &
     &                                                  rrc,rre,rrp,rrq
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
         integer :: nq,nqq,mcomb
         nqq=nq*(nq+1)/2
         allocate (nbrep(nqq),nxobs(nq),nncom(mcomb),mmark(nq,mcomb), &
     &                                           mxobs(nq), stat=ii)
         if(ii>0)stop 'alloc : combis '
         end subroutine all_combis
      END MODULE combinations

      MODULE likelihoods
         integer,save                                :: NROUND,MROUND,isecs
         real(8), dimension (:,:), allocatable, save :: feval
      END MODULE likelihoods
     
      MODULE levels
         integer, dimension(:), save, allocatable   :: nfl,ncov,nfr,nfix, &
     &                                                 nfix1,nfix2,nfix3,neff,&
     &                                                 ncov1,ncov2,lim1a,lim1b&
     &,                                                nfr1,nfr2
         integer, dimension(:,:), save, allocatable :: npow,nlev,icnest,nlnest
      contains
         subroutine all_levels_1 (nq)
         integer :: nq,ii
         allocate (nfl(nq),ncov(nq),nfr(nq),nfix(nq),nfix1(nq),nfix2(nq), &
     &             neff(nq),ncov1(nq),ncov2(nq),lim1a(nq),lim1b(nq),  &
     &             nfr1(nq),nfr2(nq),nfix3(nq),stat=ii)
         if(ii>0)stop 'levels_1 : alloc'
         end subroutine all_levels_1

         subroutine all_levels_2 (nq,mcov,mfix)
         integer :: nq,mcov,mfix,ii
         allocate (npow(mcov,nq),nlev(mfix,nq),icnest(mcov,nq),          &
     &             nlnest(mcov,nq),stat=ii)
         if(ii>0)stop 'levels_2 : alloc'
         end subroutine all_levels_2 
      END MODULE levels

      MODULE numbers
         integer, parameter                      :: MAXNOS=28
         integer, save                           :: NTRAIT,NCALL,IPVRUN, &
     &                                              KRCZHZ, NQQ,NQQ2, &
     &                                              NQSQ,NRCZHZ,nquni
         integer, save                           :: mcov,mfix,mobs,mnfr,mqhq
         integer, dimension(7), save             :: istrt,kfit
         integer, dimension(maxnos), save        :: nosvec ! length maxnos
         integer, dimension(:), allocatable,save :: nfrst,nboth

         integer, save                           :: NREC,NDATA,NQ, nq111, &
     &                                              NQ222,NANIM,NFLM,NFRM,&
     &                                              IRPMOD,LIM1,LIM2,LIM3,&
     &                                              NEQNS,IOPRN1,IOPRN2, &
     &                                              IOPCOV,IMODEL,NRAND1, &
     &                                              JPARAM,IOSRCH,IOPMOD, &
     &                                              KFITMX,KOPT,nq333,ioprn3, &
     &                                              lim3a,nrand3

         CONTAINS

         subroutine all_numbers (kq,kqq)
         integer, intent(in) :: kq,kqq
         integer             :: ii
         allocate (nfrst(kq),nboth(kqq),stat=ii)
         if(ii>0)stop 'numbers : alloc'
         end subroutine all_numbers

         subroutine set_nos
         NREC=NOSVEC(1); NDATA=NOSVEC(2);  nq=nosvec(3) ; nq111=nosvec(4)
         NQ222=NOSVEC(5); NANIM=NOSVEC(6); NFLM=NOSVEC(7); NFRM=NOSVEC(8)
         IRPMOD=NOSVEC(9); LIM1=NOSVEC(10); LIM2=NOSVEC(11); LIM3=NOSVEC(12)
         NEQNS=NOSVEC(13); IOPRN1=NOSVEC(14); IOPRN2=NOSVEC(15)
         IOPCOV=NOSVEC(16); IMODEL=NOSVEC(17); NRAND1=NOSVEC(18)
         jparam=nosvec(22); iosrch=nosvec(23)
         ioprn3=nosvec(24); nq333=nosvec(25); lim3a=nosvec(26)
         nrand3=nosvec(27); nquni=nosvec(28)
         end subroutine set_nos

         subroutine set_nosvec
         nosvec=0
         NOSVEC(1)=NREC; NOSVEC(2)=NDATA; NOSVEC(3)=NQ
         nosvec(4)=nq111; NOSVEC(5)=NQ222
         NOSVEC(6)=NANIM; NOSVEC(7)=NFLM; NOSVEC(8)=NFRM
         NOSVEC(9)=IRPMOD
         NOSVEC(10)=LIM1; NOSVEC(11)=LIM2; NOSVEC(12)=LIM3
         NOSVEC(13)=NEQNS
         NOSVEC(14)=IOPRN1; NOSVEC(15)=IOPRN2; NOSVEC(16)=IOPCOV
         NOSVEC(17)=IMODEL
         NOSVEC(18)=NRAND1; nosvec(22)=jparam
         nosvec(25)=nq333; nosvec(24)=ioprn3; nosvec(26)=lim3a
         nosvec(27)=nrand3; nosvec(28)=nquni
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
         real(8), dimension (:), save, ALLOCATABLE   :: trran1
         real(8), dimension (:,:), save, ALLOCATABLE :: trres1
      END MODULE traces
        
      MODULE dmatrices
         real(8), dimension (:,:,:,:), save, allocatable :: rd
         real(8), dimension (:,:,:), save, allocatable   :: rdr,td,tdt
      contains
         subroutine all_dmatrices (nq2,mobs,nparm,ncomb)
         integer, intent(in) :: nq2,mobs,nparm,ncomb
         integer             :: ii,mm
         allocate (rd(mobs,mobs,nparm,ncomb),stat=ii)
         if(ii>0)stop 'all_dmatrices : rd'
         mm=mobs*(mobs+1)/2
         allocate (rdr(mm,nparm,ncomb),stat=ii)
         if(ii>0)stop 'all_dmatrices : rdr'
         allocate (td(nq2,nq2,nparm),stat=ii)
         if(ii>0)stop 'all_dmatrices : td'
         allocate (tdt(nparm,nq2,nq2),stat=ii)
         if(ii>0)stop 'all_dmatrices : tdt'
         end subroutine all_dmatrices 

      END MODULE dmatrices

      MODULE like_components
         real(8),save                             :: yry,vvy,ypy,det,detl
         real(8),save                             :: deta,detm,detam,detc,detq
         real(8), dimension(:), allocatable, save :: dete
         integer, save                            :: NRANK,NNEG
         integer, dimension(:), allocatable, save :: iflag
      END MODULE like_components

      MODULE residuals
         real(8), dimension (:,:), allocatable, save   :: ee
         real(8), dimension (:,:,:), allocatable, save :: eei
      END MODULE residuals

      MODULE fixcor
         real(8),dimension(:),save,allocatable         :: corfix
         integer,dimension(:,:),save,allocatable       :: ivarvc
      END MODULE fixcor

      MODULE current
         real(8),dimension(:),save,allocatable         :: currnt
         integer, save                                 :: ILKPRE,NREUSE
      END MODULE current

      MODULE solutions
         real(8), dimension(:), allocatable, save      :: solns,savsol
         REAL(8), SAVE                                 :: ffmin
      END MODULE solutions

      MODULE zero_rows
         integer, save                                 :: NRZERO
         integer, dimension(:), allocatable, save      :: KRZERO
      END MODULE zero_rows

      MODULE mme3
         integer, dimension(:), allocatable, save      :: itrait, nr8
         contains
         subroutine all_mme3(nq)
         integer, intent(in)                           :: nq
         integer                                       :: ii
         allocate( nr8(nq), itrait(nq), stat=ii)
         if(ii>0)stop 'alloc mme3'
         end subroutine all_mme3
      END MODULE mme3














