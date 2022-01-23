C=========================================================================
      SUBROUTINE      DxPBLK (ii,jj0,nq,mq,ipar,aa,sig)
C=========================================================================

      use params
      use xsprse
      use order
      use dmatrices, only : tdt
      use units, only : zero

!     arguments
      integer, intent(inout)               :: ii,jj0
      integer, intent(in)                  :: nq,mq,ipar
      real(8), intent(in)                  :: aa
      real(8), dimension(mq,mq),intent(in) :: sig

!     local variables
      real(8)                              :: tt
      integer                              :: i,j,jj,irow,jrow,kk
      integer, external                    :: isploc
      real(8), dimension (nq,nq)           :: ZIG
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if(ipar.eq.0)then
         zig=sig(:nq,:nq)
      else
         zig= - tdt(ipar,:nq,:nq)
      end if

C     ADD OFF-DIAGONAL BLOCK
      DO  I=1,NQ
      II=II+1
      IROW=IEQNEW( II )
      JJ=JJ0
      DO  J=1,NQ
      if(sig(j,j).eq.0.d0)cycle
      JJ=JJ+1
      tt=zig(j,i)
      IF(dabs(tt)<zero)cycle
      JROW=IEQNEW(JJ)
      kk=isploc(irow,jrow)
      xspars(kk)=xspars(kk) + aa*tt
      end do
      end do
      RETURN

C==========================================================================
      ENTRY DxPDIA (ii,jj0,nq,mq,ipar,aa,sig)
C==========================================================================

C     ADD DIAGONAL BLOCK OF SIG*AA
      if(ipar.eq.0)then
         zig=sig(:nq,:nq)
      else
         zig= - tdt(ipar,:nq,:nq)
      end if
      JJ0=II
      DO I=1,NQ
      II=II+1
      IROW=IEQNEW(II)
      dia(irow)=dia(irow) + zig(i,i)*aa
      JJ=JJ0
      DO J=1,I-1
      if(sig(j,j).eq.0.d0)cycle
      JJ=JJ+1
      tt=zig(j,i)
      IF(tt.NE.0.D0)THEN
          JROW=IEQNEW(JJ)
          kk=isploc(irow,jrow)
          xspars(kk)=xspars(kk)+ aa*tt
      END IF
      end do
      end do
      RETURN

c==========================================================================
      ENTRY DxPDII (ii,jj0,nq,mq,ipar,aa,sig)
C==========================================================================

C     ADD DIAGONAL BLOCK OF SIG FOR AA=1.D0
      if(ipar.eq.0)then
         zig=sig(:nq,:nq)
      else
         zig= - tdt(ipar,:nq,:nq)
      end if
      JJ0=II
      DO I=1,NQ
      II=II+1
      IROW=IEQNEW(II)
      dia(irow)=dia(irow) + zig (i,i)
      JJ=JJ0
      DO J=1,I-1
      if(sig(j,j).eq.0.d0)cycle
      JJ=JJ+1
      tt=zig(j,i)
      IF(tt.NE.0.D0)THEN
          JROW=IEQNEW( JJ)
          kk=isploc(irow,jrow)
          xspars(kk)=xspars(kk) + tt
      END IF
      end do
      end do
      RETURN
      END subroutine dxpblk








