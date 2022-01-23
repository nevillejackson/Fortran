!===========================================================================
      SUBROUTINE DFSPA3 (kkopt)
!===========================================================================

      use params
      use units
      use sparse
      use xsprse
      use parmap
      use numbers
      use order
      use like_components

!     arguments
      integer, intent(inout)              :: kkopt

!     local variables
      real(8), dimension (:), allocatable :: work
      integer, dimension (:), allocatable :: iwork,ivec
      logical, dimension (:), allocatable :: lll
      real(8)                             :: piv,tt
      integer                             :: krow,ksub,nn,j,jrow,ii,i,
     &                                       irow

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c     ---------------------------------
c     carry out symmetric factorisation
c     ---------------------------------

      allocate(iwork(nsrow),work(nsrow),ivec(nsrow),stat=ii)
      if(ii>0)stop 'dfspa3 : alloc'

      if(kkopt.eq.1)then

         ierror=0
         call ksfct(nsrow,ixvec1,xspars,ixvec2,ixsub,dia,ivec,iwork,
     *                                                work,ierror)
!         call ggsfct(ierror)
!         original routine, compiled with f77 runs faster than
!         f90 rewrite - at least on DEC machines !!

         if(ierror.gt.0)then
            write(*,*)'routine "gsfct"  : '
            write(*,*)'coefficient matrix is not positive definite !!!'
            write(*,*)'found zero/negative root during factorisation'
            write(*,*)'try again using fact. skipping small diag.s ...'
            kkopt=2
            go to 99
         end if

      else

         print *,'try afaon'
         allocate (lll(nsrow),stat=ii)
         if(ii>0)stop 'dfspa3 : alloc'
c        initialise
         work=0.d0
         lll=.false.

c        for each row/column ...
         DO krow = 1, nsrow 

c        pivot
         piv=dia(krow)

         if(piv.lt.zero)cycle

         piv=dsqrt(piv)
         dia(krow)=piv
c        adjust lead column
         ksub=ixvec2(krow)
         nn=0
         do j=ixvec1(krow),ixvec1(krow+1)-1
         jrow=ixsub(ksub)
         xspars(j)=xspars(j)/piv
         work(jrow)=xspars(j)
         lll(jrow)=.true.
         nn=nn+1
         iwork(nn)=jrow
         ksub=ksub+1
         end do

c        row operations
         do ii=1,nn
         jrow=iwork(ii)
         tt=work(jrow)
         dia(jrow)=dia(jrow)-tt*tt  !  diagonal elements
         ksub=ixvec2(jrow)
         do i=ixvec1(jrow),ixvec1(jrow+1)-1
         irow=ixsub(ksub)
         if( lll(irow) )xspars(i)=xspars(i)-work(irow)*tt ! off-diagonals
         ksub=ksub+1
         end do
         work(jrow)=0.d0
         lll(jrow)=.false.
         end do
         end do

         deallocate(lll)

      end if

c     calculate determinant & res. SS
      det=0.d0
      do i=1,nsrow-1
      if(dia(i).gt.zero)det=det+dlog( dia(i) )
      end do
      det=det+det

      ypy=dia(nsrow)*dia(nsrow)
      kkopt=kkopt+10

99    deallocate(iwork,work,ivec,stat=ii)
      if(ii>0)stop 'dfspa3 : dealloc'

      return

      contains

c     ==========================
      SUBROUTINE  gGSFCT (iflag)
c     ==========================

!     arguments
      integer, intent(out)                :: iflag

      real(8)                             :: diagj, ljk          
      integer                             :: i,j,newk,k,kfirst,istrt,
     &                                       istop,isub
      integer, dimension (:), allocatable :: iwork2

      allocate(iwork2(neqns),stat=ii)
      if(ii>0)stop 'ggsfct : alloc'
 
      iwork=0
      work=0.d0

      DO  J = 1, NSROW

      diagj = 0.0d0  
      newK  = IWORK(J)    
      k=newk
      do while (k.ne.0)
          newk=iwork(k)
          KFIRST = IWORK2(K)   
          LJK    = XSPARS(KFIRST)
          DIAGJ = DIAGJ + LJK*LJK  
          ISTRT = KFIRST + 1  
          ISTOP = IXVEC1(K+1) - 1 
          IF ( ISTOP .ge. ISTRT )then
              IWORK2(K) = ISTRT
              I = IXVEC2(K) + (KFIRST-IXVEC1(K)) + 1
              ISUB = IXSUB(I)                 
              IWORK(K) = IWORK(ISUB)            
              IWORK(ISUB) = K                  
              DO  II = ISTRT, ISTOP  
              ISUB = IXSUB(I)              
              WORK(ISUB) = WORK(ISUB) + XSPARS(II)*LJK 
              I = I + 1 
              end do
          end if
          k= newk
       end  do  ! end do while
       DIAGJ = DIA(J) - DIAGJ
       if ( diagj .le. 0.d0 )then
           IFLAG = 1
           print *,'row',j,diagj,dia(j),ivperm(j)
           deallocate(iwork2,stat=ii)
           if(ii>0)stop 'ggsft : dealloc'
           RETURN   
       end if

       DIAGJ = DSQRT(DIAGJ)               
       DIA(J) = DIAGJ 
       ISTRT = IXVEC1(J)        
       ISTOP = IXVEC1(J+1) - 1  
       IF ( ISTOP .ge. ISTRT )then
            IWORK2(J) = ISTRT  
            I = IXVEC2(J)     
            ISUB = IXSUB(I)   
            IWORK(J) = IWORK(ISUB)
            IWORK(ISUB) = J      
            DO II = ISTRT, ISTOP
               ISUB = IXSUB(I)  
               XSPARS(II) = ( XSPARS(II)-WORK(ISUB) ) / DIAGJ  
               work(isub) = 0.0d0
               I = I + 1                                 
            end do
      end if
      end do        ! end j=1,nsrow

      deallocate(iwork2,stat=ii)
      if(ii>0)stop 'ggsft : dealloc'

      RETURN 
      END  subroutine ggsfct

      END subroutine dfspa3



















c=============================================================================
      SUBROUTINE  kSFCT ( NEQNS, XLNZ, LNZ, XNZSUB, NZSUB, DIAG, 
     *                    LINK, FIRST, TEMP, IFLAG )             
c=============================================================================

      double precision  lnz(*)
      INTEGER  NZSUB(*),XLNZ(*), XNZSUB(*)
      integer, intent(in)                      :: neqns
      integer, intent(out)                     :: iflag
      integer, intent(inout), dimension(neqns) :: link,first
      real(8), intent(inout), dimension(neqns) :: diag, temp
      real(8)                                  :: diagj, ljk          
      integer :: i,j,k,isub,newk,kfirst,istrt,istop,ii
      iflag=0
      link(:neqns)=0
      temp(:neqns)=0.d0

      DO  J = 1, NEQNS
      diagj = 0.0d0  
      newK  = LINK(J)    
      k=newk
      do while (k.ne.0)
          newk=link(k)
          KFIRST = FIRST(K)   
          LJK    = LNZ(KFIRST)
          DIAGJ = DIAGJ + LJK*LJK  
          ISTRT = KFIRST + 1  
          ISTOP = XLNZ(K+1) - 1 
          IF ( ISTOP .ge. ISTRT )then
              FIRST(K) = ISTRT
              I = XNZSUB(K) + (KFIRST-XLNZ(K)) + 1
              ISUB = NZSUB(I)                 
              LINK(K) = LINK(ISUB)            
              LINK(ISUB) = K                  
              DO  II = ISTRT, ISTOP  
              ISUB = NZSUB(I)              
              TEMP(ISUB) = TEMP(ISUB) + LNZ(II)*LJK 
              I = I + 1 
              end do
          end if
          k= newk
          end  do

          DIAGJ = DIAG(J) - DIAGJ
          if ( diagj .le. 0.d0 )then
              IFLAG = 1
              print *,'row',j,diagj,diag(j)
              RETURN   
          end if
          diagj = dsqrt(diagj)               
          DIAG(J) = DIAGJ 
          ISTRT = XLNZ(J)        
          ISTOP = XLNZ(J+1) - 1  
          IF ( ISTOP .ge. ISTRT )then
               FIRST(J) = ISTRT  
               I = XNZSUB(J)     
               ISUB = NZSUB(I)   
               LINK(J) = LINK(ISUB)
               LINK(ISUB) = J      
               DO II = ISTRT, ISTOP
                  ISUB = NZSUB(I)  
                  LNZ(II) = ( LNZ(II)-TEMP(ISUB) ) / DIAGJ  
                  temp(isub) = 0.0d0
                  I = I + 1                                 
               end do
          end if
      end do
      RETURN 
      END subroutine ksfct








