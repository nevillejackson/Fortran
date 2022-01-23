C=========================================================================
      SUBROUTINE DxPBLK (ii,jj0,nq,mq,ipar,aa,sig)
C=========================================================================

      use params
      use xsprse
      use order
      use units, only : zero
      use dmatrices, only : tdt

!     arguments
      integer, intent(inout)               :: ii,jj0
      integer, intent(in)                  :: nq,mq,ipar
      real(8), intent(in)                  :: aa
      real(8), dimension(mq,mq),intent(in) :: sig

!     local variables
      integer                              :: i,j,jj,irow,kk
      real(8), dimension (nq,nq)           :: ZIG
      integer, external                    :: isploc
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C     ADD OFF-DIAGONAL BLOCK
      if(ipar.eq.0)then
         zig=sig(:nq,:nq)
      else
         zig= - tdt(ipar,:nq,:nq)
      end if

      DO I=1,NQ
      if(sig(i,i).eq.0.d0)cycle
         II=II+1
         IROW=IEQNEW( II )
         JJ=JJ0
         DO  J=1,NQ
         IF(SIG(J,J).ne.0.D0)JJ=JJ+1
         IF(dabs(zig(j,i))<zero)cycle
         kk=isploc( irow,IEQNEW(JJ) )
         xspars(kk)=xspars(kk)+ aa*zig(j,i)
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
      if(sig(i,i).eq.0.d0)cycle
         II=II+1
         IROW=IEQNEW(II)
         dia(irow)=dia(irow)+zig(i,i)*aa
         JJ=JJ0
         DO J=1,I-1
         IF(SIG(J,J).ne.0.D0)JJ=JJ+1
         IF(dabs(zig(j,i))<zero)cycle
         kk=isploc( irow,IEQNEW(JJ) )
         xspars(kk)=xspars(kk) + aa*zig(j,i)
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
      if(sig(i,i).eq.0.d0)cycle
         ii=ii+1
         IROW=IEQNEW(II)
         dia(irow)=dia(irow) +zig(i,i)
         JJ=JJ0
         DO J=1,I-1
         IF(SIG(J,J).NE.0.D0)JJ=JJ+1
         IF(dabs(zig(j,i))<zero)cycle
             kk=isploc( irow,IEQNEW( JJ) )
             xspars(kk)=xspars(kk)+ zig(j,i)
         end do
      end do
      RETURN

      END subroutine dxpblk











