!======================================================================
      SUBROUTINE DFPOW3 (ioprun,nparm,kparm,iconv,xvec,fmin)
!======================================================================

      use params
      use units
      use likelihoods
      use parmap, only : ffvec,ipm
      use numbers
      use like_components
      use platform
      
!     arguments
      integer, intent(in)                     :: ioprun,nparm,kparm
      integer, intent(out)                    :: iconv
      real(8),dimension(nparm), intent(inout) :: xvec
      real(8), intent(inout)                  :: fmin

!     local variables
      real(8) :: flevel,escale,esca10,zzlim,ascale,xconv,fvalue
     *,          zzstep,zstep,zlimit,zaccur,zlevel,deriv2,zz,fval2,ff2
     *,          delmax,delta,fval0,dd,fpre,ftry,xx,xdel,ff1,fstart,ffm
      integer                             :: itime,i,idir,iinew,ii,jj,kk
      integer, dimension(:), allocatable   :: iwrk,iopvec
      real(8), dimension(3)                :: fvec,zvec
      real(8), dimension(:),allocatable :: stpvec,evec,xtry,dirvec,xvec0
      real(8), dimension(:,:), allocatable :: direct
      LOGICAL                              :: lopen
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate(iopvec(nparm),iwrk(nparm),stpvec(nparm),evec(nparm),
     *         xtry(nparm),xvec0(nparm),dirvec(nparm),
     *         direct(nparm,nparm),stat=ii)
      if(ii>0)stop 'dfpow3 : alloc'

C     -------------------------------------------------
C     D.I.Y. VERSION OF POWELL'S DERIVATIVE-FREE SEARCH
C     -------------------------------------------------

      CALL IPOW

      NROUND=0
      ESCA10=0.1D0*ESCALE

!     INITIAL DIRECTIONS ARE PARALLEL TO CO-ORDINATES
      direct=0.d0
      EVEC=ESCA10
      STPVEC=ESCALE
      DO  I=1,NPARM
      II=IOPVEC(I)
      IF(IPM(II).EQ.0)DIRECT(II,I)=DABS(EVEC(II))
      END DO
      ZZLIM=ESCA10
      ASCALE=0.05D0/ESCALE

!     FUNCTION VALUE FOR STARTING POINT
      fvalue=fmin
      if(fvalue.eq.big)then
 222     CALL DXLIK3(NPARM,XVEC,FVALUE,0)
         IF(dabs(FVALUE-BIG)<zz0)then
            call dferr4(ioprun,nparm,xvec)
            go to 222
         end if
      else
         WRITE(*,'(A)')' Starting point recovered from DF59#DAT '
         XLIKE=-0.5d0*fvalue
         WRITE(*,'(A,G20.10)')' Log L = ',xlike
      end if
      FSTART=FVALUE
      FFM=FVALUE

C-------------------------------------------------------------------------
C     START OF A ROUND OF ITERATION
C-------------------------------------------------------------------------

3000  NROUND=NROUND+1

C     SAVE STARTING POINT (P0) AND CORRESPONDING FUNCTION VALUE (F0)
      XVEC0=XVEC(:nparm)
      FVAL0=FVALUE
      if(fvalue<ffm)ffm=fvalue

C     VARIABLES TO DETERMINE DIRECTION WITH LARGEST CHANGE
      DELMAX=0.D0
      IINEW=0

C     ---------------------------------------------------
C     PHASE I : CARRY OUT LINEAR SEARCH IN ALL DIRECTIONS 
C     ---------------------------------------------------

      DO IDIR=1,NPARM

C     ... SKIP PARAMETERS HELD CONSTANT  
      ii=iopvec(idir)
      IF(IPM(ii).NE.0)cycle

C     ... SAVE FUNCTION VALUE  AT BEGINNING OF ITERATE
      FPRE=FVALUE

C     ... COPY DIRECTION
      DIRVEC=DIRECT(:,IDIR)

C     ... SET STEP SIZES ETC.
      ZSTEP=STPVEC(IDIR)
      ZACCUR=ASCALE*ZSTEP
      DERIV2=0.D0

C     ... CARRY OUT LINE SEARCH
      CALL DFLMIN(0)
      
C     IMPROVEMENT IN FUNCTION VALUE; KEEP TRACK OF MAX. CHANGE
      DELTA=FPRE-FVALUE
      IF(DELTA.GT.DELMAX)THEN
         DELMAX=DELTA
         IINEW=IDIR
      END IF

C     SCALE TO UNIT SECOND DERIVATIVES
      IF(DERIV2.GT.ZERO)THEN
         DD=1.D0/DSQRT(0.5D0*DERIV2)
         DIRECT(:,IDIR)=DIRECT(:,IDIR)*DD
         STPVEC(IDIR)=STPVEC(IDIR)/DD      
      END IF

      write(*,'(1x,a,i5,a,i4,a,g16.6,a,g20.8)')'IT=',NROUND,' DIR=',
     &                 IDIR,' DELTA=',-0.5D0*DELTA,
     &                 ' max.L',-0.5d0*ffm 
      END do ! idir

C     ---------------------------------------------------
C     PHASE II : EXAMINE NEW DIRECTION OF SEARCH
C     ---------------------------------------------------

C     EXTRAPOLATED POINT & FUNCTION VALUE
      PRINT *,'EXTRAPOLATED POINT'
      XTRY=2.D0*XVEC(:nparm)-XVEC0
      CALL DXLIK3(NPARM,XTRY,FVAL2,0)
      if(fval2<ffm)ffm=fval2

C     CHECK WHETHER NEW DIRECTION IS TO BE USED 
      FF1=(FVAL0-2.D0*FVALUE+FVAL2)*((FVAL0-FVALUE-DELMAX)**2)
      FF2=0.5D0*DELMAX*((FVAL0-FVAL2)**2)
      IF( (FVAL2.GE.FVAL0) .OR. (FF1.GE.FF2) )GO TO 3500

C     ---------------------------------------------------
C     PHASE III : MINIMIZE ALONG NEW DIRECTION
C     ---------------------------------------------------

C     NEW DIRECTION
      dirvec=0
      do i=1,nparm
      if(ipm(i).eq.0)dirvec(i)=xvec(i)-xvec0(i)
      end do


C     KEEP DIRECTIONS 1 TO IINEW-1; MOVE UP IINEW TO NPARM-1
      DO II=IINEW,NPARM-1
      IF(IPM(iopvec(II)).Eq.0)then
         JJ=II+1
3311     IF(IPM(iopvec(JJ)).NE.0)THEN
           JJ=JJ+1
           IF(JJ.LE.NPARM)GO TO 3311
            KK=II
            GO TO 3312
         END IF
         DIRECT(:,II)=DIRECT(:,JJ)
         STPVEC(II)=STPVEC(JJ)
      END IF
      end do

C     ... REPLACE LAST DIRECTION VECTOR BY NEW DIRECTION
      KK=NPARM
3312  XX=0.D0
      DO I=1,NPARM
      IF( (IPM(I).EQ.0).AND.(XX.LT. DABS(DIRVEC(I)/EVEC(I))))
     *                   XX=DABS(DIRVEC(I)/EVEC(I))
      END DO
      STPVEC(KK)=ESCALE/XX

C     MINIMIZE ALONG NEW DIRECTION
      zvec=(/ -1.d0,0.d0,1.d0 /)
      FVEC=(/ FVAL0,FVALUE,FVAL2 /)
      ZSTEP=STPVEC(KK)
      ZACCUR=ZSTEP*ASCALE
      ZZLIM=1.D0
      ZLIMIT=5.D0
      CALL DFLMIN(1)
      IF(DERIV2.GT.ZERO)THEN
         DD=1.D0/DSQRT(0.5D0*DERIV2 )
         DIRECT(:,KK)=DIRVEC*DD
         STPVEC(KK)=STPVEC(KK)/DD      
      END IF
    
C     ---------------------------
C     END OF A ROUND OF ITERATION
C     ---------------------------

C     CHECK FOR CONVERGENCE 
3500  CONTINUE
      XX=0.D0
      DO  I=1,NPARM
      IF( (IPM(I).EQ.0).AND.(XX.LT. DABS(DIRVEC(I)/EVEC(I)) ) )
     *                   XX=DABS(DIRVEC(I)/EVEC(I))
      END DO
      XDEL=0.D0
      DO I=1,NPARM
      if(ipm(i).ne.0) cycle
      XX=DABS( (XVEC(I)-XVEC0(I))/XVEC0(I) )
      IF(XX.GT.XDEL)XDEL=XX
      end do
      XDEL=XDEL*100.D0

!      if(fvalue>ffm.or.fval2>ffm)then
!        do i=min0(ncall,mxfunc),1,-1
!        if(ffm.eq.feval(1,i))then
!          xvec(:nparm)=feval(2:nparm+1,i)
!          fvalue=ffm
!          write(*,'(a,g20.10)')' Point replaced',fvalue
!          exit
!        end if
!        end do
!      end if

      xlike=-0.5d0*fvalue

      WRITE(IUN66,3699)NROUND,NCALL,xlike,FVALUE-FVAL0,XDEL
      WRITE(*,3699)NROUND,NCALL,xlike,FVALUE-FVAL0,XDEL

      INQUIRE(unit=28,opened=lopen)
      if(lopen) close(28)
      open(28,file='Iterates',status='unknown',form='formatted',
     *                                         position='append')
      WRITE(28,3699)NROUND,NCALL,xlike,FVALUE-FVAL0,XDEL
      CLOSE(28)
      IF( DABS(FVAL0-FVALUE).LT.FLEVEL)THEN
         ICONV=1
         WRITE(IUN66,*)'CONVERGENCE : CHANGE IN FUNCTION VALUE',FLEVEL
         WRITE(*,*)    'CONVERGENCE : CHANGE IN FUNCTION VALUE',FLEVEL
      ELSE IF(XDEL.LT.XCONV)THEN
         ICONV=1
         WRITE(IUN66,*)'CONVERGENCE : CHANGE IN PARAMETERS',XCONV
         WRITE(*,*)    'CONVERGENCE : CHANGE IN PARAMETERS',XCONV
      ELSE
         ICONV=0
         IF(NROUND.LT.MROUND)GO TO 3000
      END IF

C     ------------
C     HOUSEKEEPING
C     ------------

3503  FMIN=FVALUE

      deallocate(iopvec,iwrk,stpvec,evec,xtry,dirvec,xvec0,direct)

      RETURN
3699  FORMAT(' IT=',I4,3X,'NC=',I5,3X,'F=',G20.10,
     *                                    3X,'D=',G15.7,'  CH=',G10.3)

      contains

c     ================================
      subroutine dflmin(iotrip)
c     ================================

      integer, intent(in) :: iotrip

      real(8) :: z23,z31,z12,zz23,zz31,zz12,det,det2,d1,d2,d3,zbig
      integer :: ii,ind,ntrial,im
      integer, external :: ipsmin

c     initial step must be to the 'right'
      if(zstep.lt.0)zstep=-zstep
      zzstep=dmin1(zzlim,0.1d0*zstep)
      zzstep=dmax1(zzstep,20.d0*zaccur)

c     initial triplett has been established in calling routine
      if(iotrip.ne.0)go to 9
      zlimit=10.d0*zzstep

c     first trial point 
      zz=zzstep
2     xtry=xvec(:nparm)+zz*dirvec
      call dxlik3(nparm,xtry,ftry,0)
      if(dabs(ftry-big)<zz0)then
         zz=0.95d0*zz
         GO to 2
      end if
      if(ftry<ffm)ffm=ftry
      if(ftry-fvalue.gt.zero)then
c        ... trial point worse than initial; reverse search direction 
         zvec(2)=0.d0
         zvec(3)=zz
         zz=-zzstep
         fvec(2)=fvalue
         fvec(3)=ftry
         ind=1
         zbig=-1.d12
      else if(ftry-fvalue.lt.zero)then
c        ... trial point better than initial; double step size
         zvec(1)=0.d0
         zvec(2)=zz
         zz=zz+zzstep
         fvec(1)=fvalue
         fvec(2)=ftry
         ind=3
         zbig=1.d12
      else if(dabs(zz).le.zlimit)then
c        ... function values equal; double step size if in range allowed
         zz=zz+zz
         go to 2
      else
c        ... something has gone wrong; check routine to calculate log L !
         write(*,*)'dflmin : max. change in parameter value does not'
         write(*,*)'         alter function value(s) !!!'
         go to 10
      end if
      fvec(ind)=big
      zvec(ind)=zbig

      ntrial=0
      if(deriv2.gt.zero)then
c        ... direction has been searched in before : 
c            use 2nd derivative info for first search step
         ii=4-ind
         zz=0.5d0*( zvec(2)+zvec(ii)-(fvec(2)-fvec(ii))/
     *            (zvec(2)-zvec(ii)) )
c        ... check for convergence
         d2=dabs(zvec(2)-zz)
         IF( (dabs(zvec(ii)-zz).le.zaccur .or. d2.le.zaccur) .or. 
     *                      (d2.LE.zlevel*DABS(zz)) )go to 10
c        ... check that point is acceptable
         if(d2.gt.zlimit)then
            zz=zvec(2)+dsign(zlimit,zz-zvec(2))
            go to 4
         else
            ii=0
            go to 7
         end if
      end if

c     second trial point
4     ntrial=ntrial+1
      xtry=xvec(:nparm)+zz*dirvec
      call dxlik3(nparm,xtry,ftry,0)
      if(dabs(ftry-big)<zz0)then
         zz=0.95d0*zz
         GO to 4
      end if
      if(ftry<ffm)ffm=ftry
      call triord(fvec,zvec,ftry,zz,big)

c     quadratic inter/extra-polation      
9     z23=zvec(2)-zvec(3)
      z31=zvec(3)-zvec(1)
      z12=zvec(1)-zvec(2)
      zz23=zvec(2)*zvec(2)-zvec(3)*zvec(3)
      zz31=zvec(3)*zvec(3)-zvec(1)*zvec(1)
      zz12=zvec(1)*zvec(1)-zvec(2)*zvec(2)
      det2=z23*fvec(1)+z31*fvec(2)+z12*fvec(3)
      if(dabs(det2).lt.zero)then
         print *,'equations undetermined',det2
         go to 10
      end if
      det=z12*z23*z31
      deriv2=-2.d0*det2/det
      if(dabs(det).lt.zero .and. deriv2.lt.zero)deriv2=zero

c     check for minimum
      if(deriv2.lt.zero)then
c        ... maximum rather than minimum
         if(fvec(3).gt.fvec(1))then
            ii=1
         else
            ii=3
         end if
      else 
c        ...  minimum of parabola
         zz=0.5d0*(zz23*fvec(1)+zz31*fvec(2)+zz12*fvec(3))/det2
         d1=dabs(zvec(1)-zz)
         d3=dabs(zvec(3)-zz)
         if( dmin1(d1,d3).gt.zlimit )then
c           ... extrapolated point exceeds max. step size allowed
            if(zz.lt.zvec(1)-zlimit)then
               ii=1
            else if(zz.gt.zvec(3)+zlimit)then
               ii=3
            else
               print *,'d1,d3',d1,d3,'   zlimit',zlimit
               print *,zvec,fvec
               stop 'error : should not occur'
            end if
         else
c           ... point acceptable;
            ii=0
c           check for convergence
            if( (d1.lt.zaccur) .or. (d3.lt.zaccur) .or. 
     *                (dabs(zvec(2)-zz).lt.zaccur) )go to 10
            im=ipsmin(fvec,big,3)
            if( dabs(zz-zvec(im)).le. zlevel*dabs(zz) )go to 10 
         end if
      end if

c     repeat quadratic interpolation
7     if(ii.ne.0)zz=zvec(ii)+dsign(zlimit,zvec(ii)-zvec(2))
      if(ntrial.lt.12)go to 4

c     search completed; update parameter vector
10    im=ipsmin(fvec,big,3)
      fvalue=fvec(im)
      xvec(:nparm)=xvec(:nparm)+zvec(im)*dirvec

!      if(fvalue>ffm)then
!        write(*,'(a,2g20.10,i4)')' ffm',ffm,fvalue,im
!        write(*,'(a,3g20.10)')' fvec',fvec
!        do i=min0(ncall,mxfunc),1,-1
!        if(ffm.eq.feval(1,i))then
!          xvec(:nparm)=feval(2:nparm+1,i)
!          fvalue=ffm
!          write(*,'(a)')' Point replaced'
!          return
!        end if
!        end do
!      end if

      return
      END subroutine dflmin

!     ================
      subroutine ipow
!     ================

      integer :: i,ii,j,jj, mxiter=999

      ZLEVEL=0.03D0

      if(iosrch>0)then  ! use default values
         j=0
         do i=nparm,1,-1
         j=j+1
         iopvec(j)=i
         end do
         mround=nparm+2
         escale=0.1d0
         flevel=1.d-4
         xconv=2.d0
         return
      end if
          
      write(*,*)' '
      write(*,*)'order of search ?'
      write(*,*)'        1  ... Forward       '
      write(*,*)'        2  ... Backward      '
      write(*,*)'        3  ... User-specified '
      call optdef(ii,1,3,2)
      if(ii.eq.1)then 
         write(*,*)'Start at parameter no. ?'
         call optdef(jj,1,nparm,1)
         j=0
         do i=jj,nparm
         j=j+1
         iopvec(j)=i
         end do
         do i=1,jj-1
         j=j+1
         iopvec(j)=i
         end do
      else if(ii.eq.2)then 
         write(*,*)'Start at parameter no. ?'
         call optdef(jj,1,nparm,nparm)
         j=0
         do i=jj,1,-1
         j=j+1
         iopvec(j)=i
         end do
         do i=nparm,jj+1,-1
         j=j+1
         iopvec(j)=i
         end do
      else if(ii.eq.3)then
         iwrk=0
         do i=1,nparm
 91      write(*,*)'running no. (of parameter) for direction no.',i
         call option(jj,1,nparm)
         if(ipm(jj).ne.0)then
            write(*,*)'this parameter is not to be estimated !?! '
            go to 91
         else if(iwrk(jj).ne.0)then
            write(*,'(a,i6)')' No. has been specified before',iwrk(jj)
            go to 91
         end if
         iwrk(jj)=i
         iopvec(i)=jj
         end do
         i1=1
         do i=kparm+1,nparm
         do j=i1,nparm
         if(ipm(j).ne.0)go to 92
         end do
 92      iopvec(j)=i
         i1=j+1
         end do
      end if
      WRITE(*,'((20i4))')IOPVEC

      write(*,*)' '
      write(*,*)'max. number of iterates to be performed ?'
      call optdef(mround,1,mxiter,nparm+2)

      write(*,*)' '
      write(*,*)'accuracy/step size parameter (escale) ?'
      call rvldef(escale,0.d0,2.d0,0.1d0)

      write(*,*)' '
      write(*,*)'stop when log l changes by less than ?'
      call dfconv(flevel,4)
      write(*,*)' '
      write(*,*)'or stop when parameters change less than ?'
      write(*,fmt9(1))'1  ...  1 % of estimate from previous iterate'
      write(*,fmt9(1))'2  ...  2 %                                  '
      write(*,fmt9(1))'   ...           '
      write(*,fmt9(1))'n  ...  n %                                  '
      call rvldef(xconv,0.d0,10.d0,2.d0)

      write(*,*)'dfpowm  : flevel =',flevel
      write(*,*)'          xconv  =',xconv
      write(*,*)'          escale =',escale
      write(*,*)'          mround =',mround

      return
      end subroutine ipow

      END subroutine dfpow3



















