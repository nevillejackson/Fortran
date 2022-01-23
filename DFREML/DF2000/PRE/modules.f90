!     Modules for "DFPREP"

!---------------------- Customise this section --------------------------------
      module parameters
         IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!        max. no. of levels per fixed effect
         integer, parameter                        :: maxnfl=20000
!        max. no. of levels for add. random effects
         integer, parameter                        :: mxrnd1=60000
!        max. no. of ages (for cov. function analysis)
         integer, parameter                        :: maxage=8000
!        max. no. of animals
         integer, parameter                        :: maxan=250000
!        max. no. or real*8 variables per record
         integer, parameter                        :: maxr8=100
!        max. no. of integer variables per record
         integer, parameter                        :: maxint=100
!        max. no. of levels for which mean inbreeding is to be calculated
         integer, parameter                        :: maxlev=500
!        max. no. of right hand sides
         integer, parameter                        :: maxnq=50   
!        max. no. of NRM inverse matrices
         integer, parameter                        :: mxainv=5
!        max. animal ID (largest integer)
         integer, save                             :: ibig=2147483646
!        operational zero
         real(8), save                             :: zero=1.d-10  
!        value to be regarded as outlier (in multiples of st.devition)
         real, save                                :: outlie=8.d0
      end module parameters
!---------------------End of customisable section----------------------------
      module pedigrees
         integer, dimension (:), allocatable, save :: idsire,iddam,nown, &
     &                                                isex,igvec,ifxmrk
         integer, dimension (:), allocatable, save :: nn,npro
      end module pedigrees

      module list_of_ids
         integer, dimension(:), allocatable, save :: idvec,knext,kvcol,kfirst
         integer, save                            :: mxrec2, nanim2, ifac, &
     &                        idmax, mrow2
      end module list_of_ids

      module form_of_inputs
         CHARACTER(len=25), save :: fped,fdata
         CHARACTER(len=50), save :: cwdir
         CHARACTER(len=60), save :: INFMTP,INFMTD
         integer, save           :: IOFFLD,IOFFLP
      end module form_of_inputs

      module nrm_inverse
         real(8), dimension(:), allocatable, save :: zhz
         integer, dimension(:), allocatable, save :: ifirst,inext,ivcol,nna
         integer, save                            :: mxrec1,nrec1,mrow1,nainv
      end module nrm_inverse

      module c_numbers
         real(8), dimension(:), allocatable, save :: xvec
         integer, dimension(:), allocatable, save :: kint,kreal8,kfix,kfix1, &
     &                                               kcov,knsert,jvec
         integer, save                            :: nq,mq,mint,mnr8, nmeta, &
     &                                               iospec,iopsm1,lfxreg,   &
     &                                               nfxreg
      end module c_numbers

      module c_fixed
         integer, dimension(:), allocatable, save     :: nlrnd1
         integer, dimension(:,:), allocatable, save   :: nl,idrnd1
         integer, dimension(:,:,:), allocatable, save :: idfix,nnfix,nnrnd1
         real(8), dimension(:,:,:), allocatable, save :: xxfix,xxrnd1
      end module c_fixed

      module ages
         integer, dimension (:,:), allocatable, save   :: iiage
         integer, dimension (:,:,:), allocatable, save :: nnage
         real(8), dimension (:,:,:), allocatable, save :: xxage
         integer, dimension(:), allocatable, save      :: nage,iavec
      end module ages

      module gen_groups
         CHARACTER(len=12), save                  :: GNAME
         integer, save                            :: llev, igfix
         integer, dimension(:), allocatable, save :: ng
         real(8), dimension(:), allocatable, save :: sg
      end module gen_groups
         
      module ped_nos
         integer, dimension (8), save             :: NNPED
      end module ped_nos

      module linkrow
         integer, dimension(:), allocatable, save :: ivec
         real(8), dimension(:), allocatable, save :: rvec
      end module linkrow

      module work
         real(8), dimension(:), save, allocatable :: u
      end module work

      module units
         integer, save   :: iun11=11,iun22=22,iun23=23,iun33=33,iun34=34, &
     &                      iun44=44,iun66=66,iun30=30
      end module units





















