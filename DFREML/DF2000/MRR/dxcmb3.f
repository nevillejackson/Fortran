c============================================================================
      subroutine dxcmb3 (mark,n,nq,icomb)
c============================================================================
c     like dfcmb3, but collects combinations of records as it goes
c     -> does not give an 'ordered' list like dfcmb3 did !
c----------------------------------------------------------km--2/96---------

      use params
      use combinations

!     arguments
      integer, dimension (n), intent(in)  :: mark
      integer, intent(in)                 :: n,nq
      integer, intent(out)                :: icomb

!     local variables
      integer                             :: j

      do 1 icomb=1,ncomb
      do j=1,n
      if(mark(j).ne.mmark(j,icomb))go to 1
      end do
c     ... combination found
      nncom(icomb)=nncom(icomb)+1
      return
 1    continue

c     ... new combi
      ncomb=ncomb+1
      CALL CHKLEV(NCOMB,MXCOMB,'MXCOMB',
     *                         'COMBINATIONS OF RECORDS & TRAITS',32)
      mmark(:,ncomb)=mark
      nncom(ncomb)=1
      icomb=ncomb

      return
      end subroutine dxcmb3


