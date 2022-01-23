!     ============
      PROGRAM DF17
!     ============

      integer, dimension(:), allocatable   :: iiage,jjage,ipm
      integer                              :: nq,nqq,nage,kq
      real(8), dimension(:), allocatable   :: astar,pvec
      real(8), dimension(:,:), allocatable :: phi,clgndr
      real(8)                              :: aamin,aamx,xlow=-1.d0,
     &                                        xupp=1.d0,aa,xx,pp,
     &                                        zz0=1.d-10
      logical                              :: lex
!------------------------------------------------------------------------
      print *,'order of fit ?'
      read *,kq
      me=1


!     DETERMINE NO. OF AGES IN THE DATA
      call get_ages

!     SET UP MATRIX OF LEGENDRE COEFFICIENTS
      call alloc_spaces
      call legendre(kq)

!     evaluate legendre polynomials for ages
      do ifit=1,kq
      do i=1,nage
      aa=astar(i)
      pp=clgndr(1,ifit)
      if(dabs(aa)>zz0)then
         do j=2,ifit
         if(dabs(clgndr(j,ifit))>zz0)pp=pp+clgndr(j,ifit)*aa**(j-1)
         end do
      end if
      phi(i,ifit)=pp
      end do
      end do
 
!     write out selected coefficients
      nn=0
      ipm=0
      do i=1,kq
      print *,'write out coeff no.',i,'  ? (1=yes)'
      read *,ii
      if(ii.eq.1)then
         nn=nn+1
         ipm(nn)=i
      end if
      end do
      print *,'nn =',nn
      print *,ipm

!      open(21,file='DF21#DAT',form='formatted',status='unknown')
       print *,'nage',nage
      do ia=1,nage
      print *,ia
      do j=1,nn
      pvec(j)=phi(ia,ipm(j))
      end do
      write(21,'(10g15.7)')pvec(:nn),astar(ia)
      print *,ia,pvec(:nn)
      end do

      contains

!     =======================
      subroutine alloc_spaces
!     =======================

      allocate(phi(nage,kq),clgndr(kq,kq),pvec(kq),ipm(kq), stat=ii)
      if(ii>0)stop 'alloc'

      return
      end subroutine alloc_spaces

!     ===================
      subroutine get_ages
!     ===================

      integer :: jj

      nage=0
      inquire(file='DF20#DAT', exist=lex)

      if(lex)then
         write(*,*)'File "DF20#DAT" found - determine ages from it'
         open(1,file='DF20#DAT')
         do 
            read(1,*,iostat=jj)ii
            if(jj.ne.0)exit
            nage=nage+1
         end do
         write(*,*)'No. of ages =',nage   
         allocate(iiage(nage),jjage(nage),astar(nage),stat=ii)
         if(ii>0)stop 'alloc age'
         rewind(1)
         do i=1,nage
         if(me>1)then
            read(1,*)ii,jj,iiage(i)
         else
            read(1,*)ii,iiage(i)
         end if

         end do
         write(*,*)'Age range found =',iiage(1),iiage(nage)

      else
         write(*,*)'File "DF20#DAT" does not exist'
         stop
      end if
      
!     standardised ages
      astar(:nage)=iiage(:nage)
      aamin=astar(1)
      aamax=astar(nage)
      aa=(xupp-xlow)/(aamax-aamin)
      astar(:nage)=xlow+aa*(astar(:nage)-aamin)

      return
      end subroutine get_ages

!     ========================
      subroutine legendre (nq)
!     ========================
 
      integer, intent(in) :: nq
      integer             :: iord, jj, m, kk
      real(8)             :: c1, c2,cc, binfac

      clgndr=0.d0
      do iord=0,nq-1
      c1=dfloat(2**iord)
      c2=iord+0.5d0
      cc=dsqrt(c2)/c1
      jj=iord/2
      do m=jj,0,-1
      i=1+iord-2*m
      j=2*(iord-m)
      kk=1
      if(m.gt.0)kk=(-1)**m
      clgndr(i,iord+1)=kk*cc*binfac(iord,m)*binfac(j,iord)
      end do
      end do

      return
      end subroutine legendre

      end program df17

!=============================================================================
      double precision function binfac (n,m)
!=============================================================================
      integer, intent(in) :: n,m
      real(8)             :: factrl,b1,b2,b3

      if(m.eq.0.or.m.eq.n)then
         binfac=1.d0
      else
         b1=factrl(n)
         n1=n-m
         b2=factrl(n1)
         b3=factrl(m)
         binfac=b1/(b2*b3)
      end if
      return
      end function binfac

c=============================================================================
      double precision function factrl (n)
c=============================================================================
      integer, intent(in) :: n
      real(8)             :: ff

      ff=1.d0
      do i=2,n
      ff=ff*i
      end do
      factrl=ff
      return
      end function factrl






















