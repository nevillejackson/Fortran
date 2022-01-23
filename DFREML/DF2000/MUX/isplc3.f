c=============================================================================
      integer function isploc(irow,icol)
c=============================================================================
c     access element(irow,icol) in compressed storage ...

      use params
      use sparse
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
      PRINT *,'ERR : KSUB, K',KSUB,K
      print *,'irow, icol',irow,icol,'  ii=',ii,'  jj=',jj
      print *,'range',ixvec1(jj),ixvec1(jj+1)-1
      print *,'orig',ivperm(ii),ivperm(jj)
      STOP
      end
c=============================================================================
      integer function isplok(irow,icol)
c=============================================================================
c     access element(irow,icol) in compressed storage ...

      use params
      use sparse

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
