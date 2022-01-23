!      modules for "DFUNI"

!----------------------------Customize as required -------------------------
      MODULE parameters
!        max. no. of likelihood point stored
         INTEGER, parameter                        :: mxfunc=500
!        max. no. of NRM inverse matrices
         integer, parameter                        :: mxainv=5
!-----------------------------end of customizable section---------------------
         real(8), dimension(:), allocatable, save :: xvec, start
         integer, dimension(:), allocatable, save :: ipm
      END MODULE parameters

      MODULE like
         real(8), save                            :: yry,ypy,detc,detl
         integer, save                            :: ndf1,ndf2
         integer, dimension(:), allocatable, save :: iflag
         real(8), dimension(:), allocatable       :: fvlvec
      END MODULE like

      MODULE order
         integer, save                            :: nfill
         integer, dimension(:), allocatable, save :: ieqnew
      END MODULE order

      MODULE sparse
         integer, dimension(:),allocatable, save :: ixvec1,ixvec2,ixsub,ivperm
         integer, save :: MAXSUB,MAXLNZ,IORDER,NSROW
      END MODULE sparse

      MODULE xsprse
         real(8), dimension(:),allocatable,save   :: xspars
         real(8), dimension(:,:),allocatable,save :: rhs
      end module xsprse

      MODULE diagonal
         real(8), dimension(:), allocatable, save   :: dia
         real(8), dimension(:,:), allocatable, save :: xhx
      END MODULE diagonal

      module kzhz
         integer, dimension(:), allocatable, save :: kfirst,kvcol,knext
      end module kzhz

      module lnk
         real(8), dimension(:), allocatable, save :: zhz
         integer, dimension(:), allocatable, save :: inext,ivcol,ifirst
         integer, save                            :: maxrec,nrczhz,mxrow
      end module lnk

      module likelihoods
         real(8),dimension(:,:), allocatable, save :: feval
      end module likelihoods

      module names
         character(len=12), dimension(:), allocatable, save :: trait, param, &
     &                                                         fixed, covar
         CHARACTER(len=25), save                            :: fped,fdata
         character(len=50), save                            :: cwdir
      end module names

      module simplex
         real(8), save                              :: xconv, fss
      end module simplex

      module powell
         real(8), save                              :: escale, fconv, flevel
      end module powell

      module spasol
         real(8), save                              :: ffmin
         real(8), dimension(:),   allocatable, save :: savsol
      end module spasol

      MODULE rows
         integer, dimension(:),   allocatable, save :: ivec
         real(8), dimension(:),   allocatable, save :: row
      END MODULE rows

      MODULE works
         integer, dimension(:),   allocatable :: iwork
         real(8), dimension(:),   allocatable :: work
      END MODULE works

      MODULE SOLVE
         real(8), dimension(:,:), allocatable, save :: rhszer
         real(8), dimension(:), allocatable, save   :: diazer
         integer, dimension(:), allocatable, save   :: krzero,nnvec
      END MODULE SOLVE

      MODULE means
        real(8),dimension(:),allocatable,save :: ybar,cbar,ysdev,csdev, &
     &                                           ycv,ccv,ymin,cmin,ymax,cmax
      END MODULE means

      MODULE levels
         integer, dimension(:), allocatable, save :: nlev, npow,nsize1, &
     &                                               icnest,nlnest,irropt
         integer, save                            :: nfr1,nfr2,ncov1, &
     &                                               ncov2,lim1a,lim1b
         real(8), save                            :: sige
      END MODULE levels

      MODULE comments
         CHARACTER(len=80), dimension(6), save :: TEXT
         CHARACTER(len=80)                     :: TTEXT
         integer, save                         :: klines
      END MODULE comments

      MODULE variance_ratios
         real(8), dimension(:), allocatable, save :: gamma,xlamb
         real(8), save                            :: xkappa, alpha
      END MODULE variance_ratios

      MODULE iterates
         real(8), save :: atime, btime
         integer, save :: ntime,nround,mround,isecs
      END MODULE iterates

      MODULE constants
         real(8), save :: big=1.d37, zero, one
      END MODULE constants

      MODULE units
         integer, save :: iun11=11,iun13=13,iun14=14,iun22=22,iun23=23, &
     &                    iun44=44,iun45=45,iun47=47,iun49=49,iun51=51, &
     &                    iun52=52,iun54=54,iun59=59,iun66=66,iun67=66, &
     &                    iun28=28
      END MODULE units

      MODULE numbers
         use parameters, only : mxainv
         Integer, save :: NTRAIT,NCALL,IPVRUN,KRCZHZ,NRAND1,NRAND2, &
     &                    NRZERO,NQ,IMODEL,NFIX,NFIX1,NCOV,NANIM,NFL,NFR &
     &,                   NEQNS,NFIX2,IOPRN1,IOPRN2,LIM1,LIM2,LIM3,IOPCOV &
     &,                   JOPCOV,NREC,nainv
         real(8), save :: detll
         integer, dimension(mxainv), save :: nna
      END MODULE numbers





















