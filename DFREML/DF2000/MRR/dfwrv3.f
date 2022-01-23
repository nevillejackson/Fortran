!============================================================================
      SUBROUTINE  dfwrv3 
!============================================================================

      use parameters
      use names
      use units
      use parmap
      use sigmas
      use correlations
      use combinations
      use numbers
      use ages
      use phimatrix, only : irrmet

!     local variables
      integer                               :: i,j
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      iq=1
      call write_iun19

      call wr_grid(1,rra,siga,'siga')
      if(ioprn2.eq.1)then
         call wr_grid(1,rrm,sigm,'sigm')
         call wr_grid(2,rrm,sigm,'rrm ')
      end if
      if(ioprn1.eq.1)then
         call wr_grid(1,rrc,sigc,'sigc')
         call wr_grid(2,rrc,sigc,'rrc ')
      end if
      if(ioprn3.eq.1)then
         call wr_grid(1,rrq,sigq,'sigq')
         call wr_grid(2,rrq,sigq,'rrq ')
      end if
      call wr_grid(1,rrp,sigp,'sigp')
      call wr_grid(4,rrp,sigp,'sgpp')
      call wr_grid(2,rra,siga,'rra ')
      call wr_grid(3,rra,siga,'hsq ')
      call wr_grid(2,rrp,sigp,'rrp ')

      WRITE(IUN66,908)'-------------------------------------------'
      WRITE(IUN66,*)  'ESTIMATES OF VARIANCES & GENETIC PARAMETERS'
      WRITE(IUN66,*)  '-------------------------------------------'
      WRITE(IUN66,*)' '

      WRITE(IUN66,908)'ADDITIV-GENETIC (DIRECT) COVARIANCE MATRIX'
      call write_mat (1,manq,eiga,siga)

      IF(IOPRN2.EQ.1)THEN
         WRITE(IUN66,908)'SECOND "ANIMAL" COVARIANCE MATRIX'
         call write_mat (2,manq,eigm,sigm)
      END IF

      IF(IOPCOV.EQ.2)THEN  ! not yet adapted for nq>1 !!!
         WRITE(IUN66,908)'COVARIANCE MATRIX BETWEEN ANIMAL EFFECTS'
         DO  I=1,Nage(irrmet(3,iq))
         WRITE(IUN66,911)iiage(i,irrmet(3,iq)),SIGAM(I,:)
         end do
      END IF

      IF(IOPRN1.EQ.1)THEN
         WRITE(IUN66,908)'COVARIANCE MATRIX FOR PERM. ENV. EFFECT'
         call write_mat (4,manq,eigc,sigc)
      END IF

      IF(IOPRN3.EQ.1)THEN
         WRITE(IUN66,908)'COVARIANCE MATRIX FOR ADD. RANDOM EFFECT'
         call write_mat (5,manq,eigq,sigq)
      END IF

      WRITE(IUN66,908)'Measurement errors'
      ij=0
      do iq=1,nq
      do jq=iq,nq
      ij=ij+1
      if(iq.eq.jq)then
         if(nq>1)write(iun66,*)'For trait  ',trait(iq)
         do ia=1,nage(1)
         i=(iq-1)*nage(1)+ia
         write(iun66,911)iiage(ia,1),xvec(istrt(4)+(meage(ia)-1)*
     &                  nq*(nq+1)/2+ij), sigp(i,i),rra(ihmii(i,nanq))
         end do

      else
         write(iun66,*)'For traits  ',trait(iq),' and ',trait(jq)
         do ia=1,nage(1)
         j=(jq-1)*nage(1)+ia
         write(iun66,911)iiage(ia,1),xvec(istrt(4)+(meage(ia)-1)*
     &                 nq*(nq+1)/2+ij), sigp(i,j),rra(ihmssf(i,j,nanq))
         end do
      end if
      end do
      end do

      WRITE(IUN66,908)'PHENOTYPIC COVARIANCE MATRIX'
      call write_mat (1,manq,eigp,sigp)

      WRITE(IUN66,908)'HERITABILITIES & ADDITIV-GENETIC CORRELATIONS'
      call write_cor (1,nanq,rra)

      IF(IOPRN2.EQ.1)THEN
         WRITE(IUN66,908)'MATRIX OF "M-SQUARED" VALUES & CORRELATIONS'
         call write_cor (2,nanq,rrm)
         IF(IOPCOV.EQ.2)THEN
            WRITE(IUN66,908)'CORRELATION MATRIX BETWEEN ANIMAL EFFECTS'
            call write_cor (3,nanq,rram)
         END IF
      END IF

      IF(IOPRN1.EQ.1)THEN
         WRITE(IUN66,908)'MATRIX OF "C-SQUARED" VALUES & CORRELATIONS'
         call write_cor (4,nanq,rrc )
      END IF

      IF(IOPRN3.EQ.1)THEN
         WRITE(IUN66,908)'MATRIX OF "Q-SQUARED" VALUES & CORRELATIONS'
         call write_cor (5,nanq,rrq )
      END IF

      WRITE(IUN66,908)'MATRIX OF PHENOTYPIC CORRELATIONS'
      call write_cor (1,nanq,rrp )

      RETURN
908   FORMAT(/1X,A)
!910   FORMAT(4x,(t5,6G13.4))
911   FORMAT(I4,(t5,6G13.4))

      contains

!     ======================
      subroutine write_iun19
!     ======================
      integer :: i, j, iq,jq
      character(len=25) :: unkn='unknown',forma='formatted',fstand=
     &                                                    'DF19#DAT'

      call fconct(iun19,fstand,forma,unkn)
      do i=1,nanq
      write(iun19,913)(siga(i,j),'siga',i,j,j=i,nanq)
      end do
      if(ioprn2.eq.1)then
         do i=1,nanq
         write(iun19,913)(sigm(i,j),'sigm',i,j, j=i,nanq)
         end do
         if(iopcov.eq.2)then
            do i=1,nanq
            write(iun19,913)(sigam(i,j),'sigam',i,j, j=1,nanq)
            end do
         end if
      end if
      if(ioprn1.eq.1)then
         do i=1,nanq
         write(iun19,913)(sigc(i,j),'sigc',i,j, j=i,nanq)
         end do
      end if
      if(ioprn3.eq.1)then
         do i=1,nanq
         write(iun19,913)(sigq(i,j),'sigq',i,j, j=i,nanq)
         end do
      end if
      ij=istrt(4)
      do i=1,kfit(7,1)
      do iq=1,nq
      do jq=iq,nq
      ij=ij+1
      write(iun19,914)xvec(ij),'sigme',i,iq,jq
      end do
      end do
      end do

      return
913   format((g20.8,10x,a,2i3))
914   format((g20.8,10x,a,3i3))

      end subroutine write_iun19

!     ====================================
      subroutine wr_grid(iopt,ssig,sig,fn)
!     ====================================

      integer, intent(in)                              :: iopt
      real(8), dimension (nanq*(nanq+1)/2),intent(in)  :: ssig
      real(8), dimension (manq,manq), intent(in)       :: sig
      character(len=4),intent(in)                      :: fn

      open(20,file=fn,status='unknown',form='formatted')

      if(iopt.eq.1)then
         i=0
         do iq=1,nq
         do ia=1,nage(1)
         i=i+1
         j=0
         do jq=1,nq
         do ja=1,nage(1)
         j=j+1
         if(j<nanq)then
            write(20,'(2(i6,i3),2i5,G20.10)')iiage(ia,1),iq,iiage(ja,1),
     *                                                   jq,i,j,sig(i,j)
         else
            write(20,'(2(i6,i3),2i5,G20.10/)')iiage(ia,1),iq,iiage(ja,1)
     *,                                                  jq,i,j,sig(i,j)
         end if 
         end do
         end do
         end do
         end do

      else if(iopt.eq.2)then ! correlation matrix
         i=0
         do iq=1,nq
         do ia=1,nage(1)
         i=i+1
         j=0
         do jq=1,nq
         do ja=1,nage(1)
         j=j+1
         xx=ssig( ihmssf(i,j,nanq) )
         if(j<nanq)then
            write(20,'(2(i6,i3),2i5,f8.3)')iiage(ia,1),iq,iiage(ja,1),
     *                                                   jq,i,j,xx
         else
            write(20,'(2(i6,i3),2i5,f8.3/)')iiage(ia,1),iq,iiage(ja,1),
     *                                                   jq,i,j,xx
         end if
         end do
         end do
         end do
         end do

      else if(iopt.eq.3)then ! heritabilities, etc.
         i=0
         do iq=1,nq
         do ia=1,nage(1)
         i=i+1
         write(20,*)iiage(ia,1),iq,ssig( ihmii(i,nanq) )
         end do
         end do

      else if(iopt.eq.4)then
         i=0
         do iq=1,nq
         do ia=1,nage(1)
         i=i+1
         write(20,'(i6,i3,2g20.10)')iiage(ia,1),iq,sig(i,i),
     &                                              sqrt(sig(i,i))
         end do
         end do
      end if

      close(20)
      return
      end subroutine wr_grid

!     ========================================
      subroutine write_mat (ipar,isig,eig,sig)
!     ========================================

      integer, intent(in)                        :: ipar, isig
      real(8), dimension(nanq), intent(in)       :: eig
      real(8), dimension(isig,isig), intent(in)  :: sig
      integer                                    :: i1,j1,iq,jq

      i1=0
      do iq=1,nq
      j1=i1
      do jq=iq,nq
      if(iq.eq.jq)then
         write(iun66,*)'Covariances between ages for ',trait(iq)
         DO  I=1,Nage(irrmet(ipar,iq))
         WRITE(IUN66,911)iiage(i,irrmet(ipar,iq)),SIG(i1+1:i1+i,i1+I)
         end do
      else
         write(iun66,*)'Covariances between ages for ',trait(iq),
     &                                          '  and  ',trait(jq)
         DO  I=1,Nage(irrmet(ipar,iq))
         WRITE(IUN66,911)iiage(i,irrmet(ipar,iq)),
     &                     SIG(j1+1:j1+nage(irrmet(ipar,jq)),i1+I)
         end do
      end if
      j1=j1+nage(irrmet(ipar,jq))
      end do
      i1=i1+nage(irrmet(ipar,iq))
      end do
!      WRITE(IUN66,*)'... WITH EIGENVALUES '
!      WRITE(IUN66,910)EIG
      return
911   FORMAT(I4,(t5,6G13.4))
      end subroutine write_mat

!     ===================================
      subroutine write_cor (ipar,isig,rr)
!     ===================================

      integer, intent(in)                              :: ipar, isig
      real(8), dimension(isig*(isig+1)/2), intent(in)  :: rr
      integer                                          :: i1,j1,iq,jq

      i1=0
      do iq=1,nq
      j1=i1
      do jq=iq,nq
      if(iq.eq.jq)then
         write(iun66,*)'Correlations between ages for ',trait(iq)
         DO  I=1,Nage(irrmet(ipar,iq))
         WRITE(IUN66,911)iiage(i,irrmet(ipar,iq)),
     &                    rr( (/ (ihmssf(i1+k,i1+i,nanq), k=1,i) /))
         end do
      else
         write(iun66,*)'Correlations between ages for ',trait(iq),
     &                                          '  and  ',trait(jq)
         DO  I=1,Nage(irrmet(ipar,iq))
         WRITE(IUN66,911)iiage(i,irrmet(ipar,iq)),
     &   (rr(ihmssf(j1+k,i1+i,nanq)), k=1,nage(irrmet(ipar,jq)) )
         end do
      end if
      j1=j1+nage(irrmet(ipar,jq))
      end do
      i1=i1+nage(irrmet(ipar,iq))
      end do

      return
911   FORMAT(I4,(t5,6G13.4))
      end subroutine write_cor

      END subroutine dfwrv3





















