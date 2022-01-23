!=========================================================================
      SUBROUTINE  DFSPA1 (kkopt)
!=========================================================================

      use sparse
      use xsprse
      use diagonal
      use order
      use rows
      use works
      use like
      use constants
      use numbers

!     arguments
      integer, intent(inout)              :: kkopt
      real(8)                             :: dd

!     ---------------------------------
!     carry out symmetric factorisation
!     ---------------------------------
 
      if(kkopt.eq.1)then
        ierror=0
        call gsfct(nsrow,ixvec1,xspars,ixvec2,ixsub,dia,ivec,iwork,
     *                                                 row,ierror)

         if(ierror.gt.0)then
         write(*,*)'coefficient matrix is not positive definite !!! '
         write(*,*)'found zero or negative root during factorisation'
            kkopt=2
            return
         end if

      else
         call dffct
      end if

c     calculate log determinant of coefficient matrix
      dd=0.d0
      do  i=1,nsrow
      if(dia(i)>zero)dd=dd+dlog(dia(i))
      end do
      detc=dd+dd

c     -------------------------
c     solve system of equations
c     -------------------------

c     for each trait (=rhs) ...
      do iq=nq,1,-1
      row(:nsrow)=rhs(iq,:nsrow)

c     obtain solutions
      call gsslv(nsrow,ixvec1,xspars,ixvec2,ixsub,dia,row)

c     calculate SS/CP of solutions x right hand sides
      yry=xhx(ntrait,ntrait)
      work(:iq)=0.d0
      do i=1,nsrow
      work(:iq)=work(:iq)+rhs(:iq,i)*row(i)
      end do
      rhs(iq,:nsrow)=row(:nsrow)
      xhx(:iq,iq)=xhx(:iq,iq)-work(:iq)
      end do

      dia(:nq)=(/ (xhx(i,i),i=1,nq) /)

      kkopt=kkopt+10
      return

      contains

!     ================
      subroutine dffct
!     ================

      real(8), dimension(:),allocatable   :: work
      logical, dimension (:), allocatable :: lll
      real(8)                             :: piv,xx,xxs,tt
      integer                             :: krow,ksub,nn,j,jrow,ii,i
     &,                                      irow

      allocate (work(nsrow),lll(nsrow),stat=ii)
      if(ii>0)stop 'dfspa1 : alloc'

      work=0.d0
      lll=.false.

      DO krow = 1, nsrow 

      piv=dia(krow)

      if(piv.lt.zero)cycle
      piv=dsqrt(piv)
      xx=1.d0/piv
      dia(krow)=piv
      ksub=ixvec2(krow)
      nn=0
      do j=ixvec1(krow),ixvec1(krow+1)-1
      jrow=ixsub(ksub)
      xxs=xspars(j)*xx
      xspars(j)=xxs
      work(jrow)=xxs
      lll(jrow)=.true.
      nn=nn+1
      iwork(nn)=jrow
      ksub=ksub+1
      end do
!     xspars(ixvec1(krow):ixvec1(krow+1)-1)= 
!     &     xspars(ixvec1(krow):ixvec1(krow+1)-1)/xx
      do  ii=1,nn
      jrow=iwork(ii)
      tt=work(jrow)
      dia(jrow)=dia(jrow)-tt*tt
      ksub=ixvec2(jrow)
      do i=ixvec1(jrow),ixvec1(jrow+1)-1
      irow=ixsub(ksub)
      if( lll(irow) )xspars(i)=xspars(i)-work(irow)*tt
      ksub=ksub+1
      end do
      work(jrow)=0.d0
      lll(jrow)=.false.
      end do
      end do

      deallocate(work,lll)
      return
      end subroutine dffct

      END subroutine dfspa1
