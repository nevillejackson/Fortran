c=============================================================================
      integer function isploc(irow,icol)
c=============================================================================
c     access element(irow,icol) in compressed storage ...

      use params
      use sparse
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      isploc=0
      if(irow.eq.icol)then
         write(*,'(a,2i6)')'isploc : diag',irow,icol
         stop
      end if
      II=MAX0(IROW,ICOL)
      JJ=MIN0(IROW,ICOL)
      KSUB=IXVEC2(JJ)
      DO K=IXVEC1(JJ),IXVEC1(JJ+1)-1
      IF(IXSUB(KSUB).EQ.II)then
        isploc=k
        return
      end if
      KSUB=KSUB+1
      end do
      write(*,'(a,2i12)') 'ERR : KSUB, K ',KSUB,K
      write(*,'(a,2i10,2(a,i10))')'irow, icol',irow,icol,'  ii=',ii,
     &                          '  jj=',jj
      write(*,'(a,2i12)')'orig',ivperm(ii),ivperm(jj)
      STOP
      end
!===========================================================================
      integer function isplok(irow,icol)
!===========================================================================
!     access element(irow,icol) in compressed storage ...

      use params
      use sparse

      II=MAX0(IROW,ICOL)
      JJ=MIN0(IROW,ICOL)
      KSUB=IXVEC2(JJ)
      DO K=IXVEC1(JJ),IXVEC1(JJ+1)-1
      IF(IXSUB(KSUB).EQ.II)then
        isplok=k
        return
      end if
      KSUB=KSUB+1
      end do
      isplok=0
      return
      end
