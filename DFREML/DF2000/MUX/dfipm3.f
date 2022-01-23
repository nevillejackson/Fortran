C==========================================================================
      SUBROUTINE      DFIPM3 (ipartm,nparm,xvec)
C==========================================================================

      use params
      use names
      use units
      use parmap, only : ipm, ffvec
      use fixcor

!     arguments
      integer, intent(in)                       :: ipartm,nparm
      real(8), dimension(nparm), intent(inout)  :: xvec

!     local variables
      integer, dimension(2)                     :: jv
      character(len=12)                         :: para, pp
      integer                                   :: nc,m,i,iifix,kk,ii,ll
      real(8)                                   :: xx,xx1

!     fix (co)variance component(s)
    
      IF(IPARTM.EQ.1)THEN
         Write(*,*)'NO. OF PARAMETERS IN TOTAL    =',NPARM
         WRITE(*,*)' '
         WRITE(*,*)'NO. OF PARAMETERS TO BE FIXED ?'
         M=NPARM-1
         CALL OPTDEF(NC,0,M,1)
         WRITE(*,*)' '

         DO  I=1,NC
         WRITE(*,*)'RUNNING NO. OF PARAMETER TO BE FIXED ?'
         CALL OPTION(N,1,NPARM)
         WRITE(*,*)'PARAMETER TO BE FIXED =  ',PARAM(N)
         WRITE(*,991)'CURRENT VALUE =',XVEC(N),XVEC(N)*FFVEC(N)
         WRITE(*,*)'GIVE OPTION :'
         WRITE(*,*)'        0  ...  FIX AT CURRENT VALUE '
         WRITE(*,*)'        1  ...  READ IN NEW VALUE    '
         WRITE(*,*)'        2  ...  ADD CONSTANT TO CURRENT VALUE '
         CALL OPTDEF(IIFIX,0,2,0)
         IF(IIFIX.EQ.1)THEN
            WRITE(*,*)'NEW VALUE FOR ',PARAM(N),'  ?'
            call rvldef(xx,-1.d6,1.d6,0.d0)
            XVEC(N)=XX
         ELSE IF(IIFIX.EQ.2)THEN
            WRITE(*,*)'CURRENT VALUE FOR  ',PARAM(N),'   =',XVEC(N)
            WRITE(*,*)'CONSTANT TO BE ADDED ?'
            CALL RVLDEF(XX,-1.D6,1.D6,0.001D0)
            XVEC(N)=XVEC(N)+XX
         END IF
         WRITE(*,992)N,PARAM(N),XVEC(N),XVEC(N)*FFVEC(N)
         IPM(N)=1
         end do

!     fix correlation(s)

      ELSE IF(IPARTM.EQ.2)THEN
         WRITE(*,*)'NO. OF CORRELATIONS TO BE FIXED ?'
         CALL OPTDEF(NC1,0,NPARM,1)
         DO I=1,NC1
         WRITE(*,*)'RUNNING NO. OF PERTAINING COVARIANCE ?'
         CALL OPTION(II,2,NPARM-1)
         para=param(ii)
         read(para,'(a8,2i2)')pp(1:8),jv
         WRITE(*,*)'PARAMETER TO BE FIXED =  ',PARA,ii
         write(*,*)'value of "fixed" correlation ?'
         if(xvec(ii).gt.0)then
            xx1=1.d0
         else if(xvec(ii).lt.0)then
            xx1=-1.d0
         else
            xx1=0.d0
         end if
         call rvldef(xx,-1.d0,1.d0,xx1)
         corfix(ii)=xx
         do  kk=1,2
         write(pp,'(a8,2i2)')para(1:8),jv(kk),jv(kk)
         do j=1,nparm
         jj=0
         if(param(j).eq.pp)then
            jj=j
            exit
         end if
         end do
         write(*,*)'running no. of variance no.',kk,'  ??  ',pp
         call optdef(ll,1,nparm,jj)
         ivarvc(kk,ii)=ll
         end do
         print *,'cov =',ii,'  var 1 =',ivarvc(1,ii),'  var 2=',
     *                                  ivarvc(2,ii)
         IPM(II)=2
         end do
      END IF

      RETURN

991   FORMAT(1X,A,2G16.6)
992   FORMAT(I3,4X,A,4X,'FIXED TO VALUE =',2G20.10)
      END subroutine dfipm3



