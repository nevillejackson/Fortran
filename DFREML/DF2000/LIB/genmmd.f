C***********************************************************************
      SUBROUTINE  GGENMMD ( NSROW, KFIRST, KVCOL, INVPRM, IVPERM,IDELTA,
     &                                            IBIG, NOFSUB, mkvcol )
C***********************************************************************

!     input ...
      integer, intent(in) :: nsrow   ! no. of equations
      integer, intent(in) :: ibig    ! large integer for marking nodes
      integer, intent(in) :: idelta  ! tol. value for multiple eliminations
      integer, intent(in) :: mkvcol
      integer, dimension(nsrow+1), intent(inout) :: kfirst ! adjacency
      integer, dimension(mkvcol), intent(inout) :: kvcol      ! structure
!     output
      integer, dimension(nsrow), intent(out) :: invprm ! inverse perm. vector
      integer, dimension(nsrow), intent(out) :: ivperm ! permutation vector
      integer,  intent(out) :: nofsub               ! no. of subscripts used

!     local variables
      integer, dimension (:), allocatable :: dhead, llist, marker,qsize
      integer :: EHEAD,I,MDEG,MDLMT, MDNODE, NEXTMD, NUM,TAG

      IF  ( NSROW .LE. 0 )  RETURN

      allocate (dhead(nsrow),llist(nsrow),marker(nsrow),qsize(nsrow),
     &                                                        stat=i)
      if(i>0)stop 'alloc genmmd'

!     initialize
      mmdcal=0
      NOFSUB = 0
      CALL  MMDINT (INVPRM, IVPERM)

      NUM = 1
      NEXTMD = DHEAD(1)
      do while ( NEXTMD .gt. 0 )
         MDNODE = NEXTMD
         NEXTMD = INVPRM(MDNODE)
         MARKER(MDNODE) = IBIG
         INVPRM(MDNODE) = - NUM
         NUM = NUM + 1
      end do
      IF  ( NUM .GT. NSROW )  GO TO 1000
      TAG = 1
      DHEAD(1) = 0
      MDEG = 2
  300 IF  ( DHEAD(MDEG) .le. 0 )then
          MDEG = MDEG + 1
          GO TO 300
      end if
      MDLMT = MDEG + IDELTA
      EHEAD = 0
  500 MDNODE = DHEAD(MDEG)
      IF  ( MDNODE .le. 0 )then
         MDEG = MDEG + 1
         IF  ( MDEG .GT. MDLMT )  GO TO 900
         GO TO 500
      end if
      NEXTMD = INVPRM(MDNODE)
      DHEAD(MDEG) = NEXTMD
      IF  ( NEXTMD .GT. 0 )  IVPERM(NEXTMD) = - MDEG
      INVPRM(MDNODE) = - NUM
      NOFSUB = NOFSUB + MDEG + QSIZE(MDNODE) - 2
      IF  ( NUM+QSIZE(MDNODE) .GT. NSROW )  GO TO 1000
      TAG = TAG + 1
      IF  ( TAG .ge. IBIG )then
          TAG = 1
          DO  I = 1, NSROW
          IF  ( MARKER(I) .LT. IBIG )  MARKER(I) = 0
          end do
      end if
      mmdcal=mmdcal+1
      if(mod(mmdcal,250).eq.0)print *,'genmmd',mmdcal,mdnode,mdeg
      CALL  MMDELM ( MDNODE, INVPRM, IVPERM,TAG )
      NUM = NUM + QSIZE(MDNODE)
      LLIST(MDNODE) = EHEAD
      EHEAD = MDNODE
      IF  ( IDELTA .GE. 0 )  GO TO 500

  900 IF ( NUM .le. NSROW )then
         CALL  MMDUPD ( EHEAD,  MDEG, INVPRM, IVPERM, TAG )
         GO TO 300
      end if

 1000 CALL  MMDNUM ( NSROW, IVPERM, INVPRM, QSIZE )

      deallocate(dhead,llist,marker,qsize,stat=i)
      if(i>0)stop 'alloc genmmd'
      RETURN

      contains

C***************************************************************
      SUBROUTINE  MMDELM ( MDNODE, DFORW, DBAKW, tag)
C***************************************************************

      integer, intent(in) :: mdnode,tag
      INTEGER, dimension(nsrow), intent(inout) :: DBAKW, DFORW

!     local variables
      INTEGER ::  ELMNT,I,ISTOP,ISTRT,J,JSTOP , JSTRT , LINK  ,
     1              NABOR , NODE  , NPV   , NQNBRS, NXNODE,
     1              PVNODE, RLMT  , RLOC  , RNODE ,  XQNBR

      MARKER(MDNODE) = TAG
      ISTRT = KFIRST(MDNODE)
      ISTOP = KFIRST(MDNODE+1) - 1
      ELMNT = 0
      RLOC = ISTRT
      RLMT = ISTOP
      DO  200  I = ISTRT, ISTOP
      NABOR = KVCOL(I)
      IF  ( NABOR .EQ. 0 )  GO TO 300
      IF  ( MARKER(NABOR) .lt. TAG )then
          MARKER(NABOR) = TAG
          IF  ( DFORW(NABOR) .ge. 0 )then
              KVCOL(RLOC) = NABOR
              RLOC = RLOC + 1
              GO TO 200
          end if
          LLIST(NABOR) = ELMNT
          ELMNT = NABOR
      end if
  200 CONTINUE

  300 IF  ( ELMNT .LE. 0 )  GO TO 1000
      KVCOL(RLMT) = - ELMNT
      LINK = ELMNT
  400 JSTRT = KFIRST(LINK)
      JSTOP = KFIRST(LINK+1) - 1
      DO  800  J = JSTRT, JSTOP
      NODE = KVCOL(J)
      LINK = - NODE
      IF  ( NODE<0 )go to  400
      IF  ( NODE.eq.0 )go to 900
  500 IF (MARKER(NODE).GE.TAG .OR.DFORW(NODE).LT.0)GO TO 800
      MARKER(NODE) = TAG
  600 IF  ( RLOC .ge. RLMT )then
          LINK = - KVCOL(RLMT)
           RLOC = KFIRST(LINK)
           RLMT = KFIRST(LINK+1) - 1
           GO TO 600
      end if
      KVCOL(RLOC) = NODE
      RLOC = RLOC + 1
  800 CONTINUE
  900 CONTINUE
      ELMNT = LLIST(ELMNT)
      GO TO 300
 1000 CONTINUE
      IF  ( RLOC .LE. RLMT )  KVCOL(RLOC) = 0

      LINK = MDNODE
 1100 ISTRT = KFIRST(LINK)
      ISTOP = KFIRST(LINK+1) - 1
      DO  1700  I = ISTRT, ISTOP
      RNODE = KVCOL(I)
      LINK = - RNODE
      IF  ( RNODE<0 )go to  1100
      IF  ( RNODE.eq.0 )go to 1800

 1200 PVNODE = DBAKW(RNODE)
      IF  ( PVNODE .EQ. 0  .OR. PVNODE .EQ. (-IBIG) )  GO TO 1300
      NXNODE = DFORW(RNODE)
      IF  ( NXNODE .GT. 0 )  DBAKW(NXNODE) = PVNODE
      IF  ( PVNODE .GT. 0 )  DFORW(PVNODE) = NXNODE
      NPV = - PVNODE
      IF  ( PVNODE .LT. 0 )  DHEAD(NPV) = NXNODE
 1300 CONTINUE

      JSTRT = KFIRST(RNODE)
      JSTOP = KFIRST(RNODE+1) - 1
      XQNBR = JSTRT
      DO  J = JSTRT, JSTOP
      NABOR = KVCOL(J)
      IF  ( NABOR .EQ. 0 )  GO TO 1500
      IF  ( MARKER(NABOR) .lt. TAG )then
      KVCOL(XQNBR) = NABOR
      XQNBR = XQNBR + 1
      end if
      end do

C     IF NO ACTIVE NABOR AFTER THE PURGING ...
 1500 NQNBRS = XQNBR - JSTRT
      IF  ( NQNBRS .le. 0 )then
         QSIZE(MDNODE) = QSIZE(MDNODE) + QSIZE(RNODE)
         QSIZE(RNODE) = 0
         MARKER(RNODE) = IBIG
         DFORW(RNODE) = - MDNODE
         DBAKW(RNODE) = - IBIG
      else
         DFORW(RNODE) = NQNBRS + 1
         DBAKW(RNODE) = 0
         KVCOL(XQNBR) = MDNODE
         XQNBR = XQNBR + 1
         IF  ( XQNBR .LE. JSTOP )  KVCOL(XQNBR) = 0
      end if
 1700 CONTINUE
 1800 CONTINUE

      RETURN
      END subroutine mmdelm

C***************************************************************
      SUBROUTINE  MMDUPD ( EHEAD, MDEG,DFORW, DBAKW, TAG )
C***************************************************************

      integer, intent(in)    :: ehead
      integer, intent(inout) :: tag,mdeg
      integer, dimension(nsrow), intent(inout) ::   DBAKW, DFORW 

      INTEGER ::  DEG, DEG0,ELMNT, ENODE ,FNODE , I , IQ2 , 
     1              ISTRT , J,  LINK  ,
     1               MDEG0 , MTAG  , NABOR , NODE, Q2HEAD, QXHEAD

         MDEG0 = MDEG + IDELTA
         ELMNT = EHEAD
 100     IF  ( ELMNT .LE. 0 )  RETURN
             MTAG = TAG + MDEG0
             IF  ( MTAG .ge. IBIG )then
                 TAG = 1
                 DO I = 1, NSROW
                 IF  ( MARKER(I) .LT. IBIG )  MARKER(I) = 0
                 end do
                 MTAG = TAG + MDEG0
             end if
             Q2HEAD = 0
             QXHEAD = 0
             DEG0 = 0
             LINK = ELMNT
 400         DO  700  I = KFIRST(LINK),KFIRST(LINK+1) - 1
             ENODE = KVCOL(I)
             LINK = - ENODE

             IF  ( ENODE.lt.0 )then
                 go to 400
             else IF  ( ENODE.eq.0 )then
                 go to 800
             end if

 500         IF  ( QSIZE(ENODE) .ne. 0 )then
                  DEG0 = DEG0 + QSIZE(ENODE)
                  MARKER(ENODE) = MTAG
                  IF  ( DBAKW(ENODE) .eq. 0 )then
                      IF  ( DFORW(ENODE) .ne. 2 )then
                          LLIST(ENODE) = QXHEAD
                          QXHEAD = ENODE
                       else
                          LLIST(ENODE) = Q2HEAD
                          Q2HEAD = ENODE
                       end if
                  end if
             end if

  700        CONTINUE
  800        ENODE = Q2HEAD
             IQ2 = 1
  900        IF  ( ENODE .LE. 0 )  GO TO 1500
             IF  ( DBAKW(ENODE) .NE. 0 )  GO TO 2200
                     TAG = TAG + 1
                     DEG = DEG0
                     ISTRT = KFIRST(ENODE)
                     NABOR = KVCOL(ISTRT)
                     IF  ( NABOR .EQ. ELMNT )  NABOR = KVCOL(ISTRT+1)
                     LINK = NABOR
                     IF  ( DFORW(NABOR) .ge. 0 )  then
                         DEG = DEG + QSIZE(NABOR)
                         GO TO 2100
                     end if

 1000                CONTINUE
                     DO  1400  I = KFIRST(LINK), KFIRST(LINK+1) - 1
                          NODE = KVCOL(I)
                          LINK = - NODE
                          IF  ( NODE .EQ. ENODE )  GO TO 1400
                          IF  ( NODE )  1000, 2100, 1100
 1100                        CONTINUE
                             IF  ( QSIZE(NODE) .EQ. 0 )  GO TO 1400
                             IF  ( MARKER(NODE) .lt. TAG )then
                                 MARKER(NODE) = TAG
                                 DEG = DEG + QSIZE(NODE)
                                 GO TO 1400
                             end if
                             IF  ( DBAKW(NODE) .NE. 0 )  GO TO 1400
                             IF  ( DFORW(NODE) .eq. 2 )then
                                 QSIZE(ENODE)=QSIZE(ENODE)+QSIZE(NODE)
                                 QSIZE(NODE) = 0
                                 MARKER(NODE) = IBIG
                                 DFORW(NODE) = - ENODE
                                 DBAKW(NODE) = - IBIG
                                 GO TO 1400
                             end if
                         IF  ( DBAKW(NODE).EQ.0 )DBAKW(NODE)=-IBIG
 1400                    CONTINUE
                         GO TO 2100
 1500            CONTINUE
                 ENODE = QXHEAD
                 IQ2 = 0
 1600            CONTINUE
                     IF  ( ENODE .LE. 0 )  GO TO 2300
                     IF  ( DBAKW(ENODE) .NE. 0 )  GO TO 2200
                         TAG = TAG + 1
                         DEG = DEG0
                         DO  2000  I =KFIRST(ENODE),KFIRST(ENODE+1) - 1
                             NABOR = KVCOL(I)
                             IF  ( NABOR .EQ. 0 )  GO TO 2100
                             IF  ( MARKER(NABOR) .GE. TAG )  GO TO 2000
                                 MARKER(NABOR) = TAG
                                 LINK = NABOR
                                 IF  ( DFORW(NABOR) .ge. 0 )then
                                     DEG = DEG + QSIZE(NABOR)
                                     GO TO 2000
                                 end if
 1700                            DO  J = KFIRST(LINK),KFIRST(LINK+1) - 1
                                         NODE = KVCOL(J)
                                         LINK = - NODE
                                         IF  ( NODE<0 )go to  1700
                                         IF  ( NODE.eq.0 )go to 2000
 1800                                IF  ( MARKER(NODE) .lt. TAG )then
                                          MARKER(NODE) = TAG
                                          DEG = DEG + QSIZE(NODE)
                                     end if
                                     end do
 2000                    CONTINUE
 2100                CONTINUE
                     DEG = DEG - QSIZE(ENODE) + 1
                     FNODE = DHEAD(DEG)
                     DFORW(ENODE) = FNODE
                     DBAKW(ENODE) = - DEG
                     IF  ( FNODE .GT. 0 )  DBAKW(FNODE) = ENODE
                     DHEAD(DEG) = ENODE
                     IF  ( DEG .LT. MDEG )  MDEG = DEG
 2200                CONTINUE
                     ENODE = LLIST(ENODE)
                     IF  ( IQ2 .EQ. 1 )  GO TO 900
                         GO TO 1600
 2300        CONTINUE
             TAG = MTAG
             ELMNT = LLIST(ELMNT)
             GO TO 100
      END subroutine mmdupd

C***************************************************************
      SUBROUTINE  MMDINT ( DFORW, DBAKW )
C***************************************************************

      integer, dimension(nsrow), intent(out) :: dbakw,dforw
      INTEGER ::  FNODE , NDEG  , NODE

      dhead=0
      marker=0
      llist=0        
      qsize=1

      DO  NODE = 1, NSROW
      NDEG = KFIRST(NODE+1) - KFIRST(NODE) + 1
      FNODE = DHEAD(NDEG)
      DFORW(NODE) = FNODE
      DHEAD(NDEG) = NODE
      IF  ( FNODE .GT. 0 )  DBAKW(FNODE) = NODE
      DBAKW(NODE) = - NDEG
      end do

      RETURN
      END subroutine mmdint

      END subroutine ggenmmd

C***************************************************************
      SUBROUTINE  MMDNUM ( NSROW, PERM, INVP, QSIZE )
C***************************************************************
C
      INTEGER, dimension(nsrow) ::  INVP , PERM  , QSIZE
      INTEGER :: FATHER, NSROW , NEXTF , NODE  , NQSIZE, NUM, ROOT

      DO  NODE = 1, NSROW
          NQSIZE = QSIZE(NODE)
          IF  ( NQSIZE .LE. 0 )  PERM(NODE) = INVP(NODE)
          IF  ( NQSIZE .GT. 0 )  PERM(NODE) = - INVP(NODE)
      end do

      DO   NODE = 1, NSROW
      IF  ( PERM(NODE) .le. 0 ) then
           FATHER = NODE
 200       IF  ( PERM(FATHER) .le. 0 )then
               FATHER = - PERM(FATHER)
               GO TO 200
           end if
           ROOT = FATHER
           NUM = PERM(ROOT) + 1
           INVP(NODE) = - NUM
           PERM(ROOT) = NUM
           FATHER = NODE
 400       NEXTF = - PERM(FATHER)
           IF  ( NEXTF .gt. 0 )then
               PERM(FATHER) = - ROOT
               FATHER = NEXTF
               GO TO 400
           end if
      end if
      end do

      DO  NODE = 1, NSROW
      NUM = - INVP(NODE)
      INVP(NODE) = NUM
      PERM(NUM) = NODE
      end do

      RETURN
      END subroutine mmdnum
