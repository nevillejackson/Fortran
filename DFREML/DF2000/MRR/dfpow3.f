!======================================================================
      SUBROUTINE DFPOW3 (ioprun,isilen,iconv,fmin)
!======================================================================

      use parameters
      use platform
      use units
      use likelihoods
      use parmap
      use numbers
      use like_components
      
!     arguments
      integer, intent(in)                     :: ioprun,isilen
      integer, intent(out)                    :: iconv
      real(8), intent(inout)                  :: fmin

!     local variables
      real(8) :: flevel,escale,esca10,zzlim,ascale,xconv,fvalue,fstart
     *,          zzstep,zstep,zlimit,zaccur,zlevel,deriv2,zz,fval2,ff2
     *,          delmax,delta,fval0,dd,fpre,ftry,xx,xdel,ff1,xlike,ffm
      integer :: itime,i,idir,iinew,ii,jj,kk
      logical :: lopen
      integer, dimension(:), allocatable   :: iwrk,iopvec
      real(8), dimension(3)                :: fvec,zvec
      real(8), dimension(:),allocatable    :: stpvec,evec,xtry,dirvec,
     *                                        xvec0
      real(8), dimension(:,:), allocatable :: direct
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if(iopeps.eq.1)then
         mparm=kparm-1 ! no. of parameters (on reparameterised scale)
      else
         mparm=kparm
      end if

      allocate(iopvec(mparm),iwrk(mparm),stpvec(mparm),evec(mparm),
     *         xtry(kparm),xvec0(kparm),dirvec(kparm),
     *         direct(kparm,mparm),stat=ii)
      if(ii>0)stop 'dfpow3 : alloc'

      INQUIRE(unit=iun28,opened=lopen)
      if(lopen) close(iun28)
      open(iun28,file='Iterates',status='unknown',form='formatted',
     *                                         position='append')
C     -------------------------------------------------
C     D.I.Y. VERSION OF POWELL'S DERIVATIVE-FREE SEARCH
C     -------------------------------------------------

      CALL IPOW

!     get sensible starting value for sig eps
      if(iopeps.eq.1)CALL DxLIK3(XVEC,FVALUE,xlike,0)

      NROUND=0
      NCALL=LCALL
      ESCA10=0.1D0*ESCALE

C     INITIAL DIRECTIONS ARE PARALLEL TO CO-ORDINATES 

c     set arrays ...         
      DIRECT=0.D0
      EVEC=ESCA10
      STPVEC=ESCALE

      DO  I=1,MPARM
      II=IOPVEC(I)
      IF(IPM(II).EQ.0)DIRECT(II,I)=DABS(EVEC(II))
      END DO
      ZZLIM=ESCA10
      ASCALE=0.05D0/ESCALE

C     FUNCTION VALUE FOR STARTING POINT
      fvalue=fmin
      if(fvalue.eq.big)then
 222     CALL DxLIK3(XVEC,FVALUE,xlike,0)
         if(kparm>44)write(*,'(i6,g20.10)')ncall,xlike
         IF(dabs(FVALUE-BIG)<zz0)then
            call dferr4(ioprun,kparm,xvec)
            go to 222
         end if
         write(iun28,'(a,i12)')' Time per "DXLIK3" ',isecs
      else
         WRITE(*,'(A)')' Starting point recovered from DF59#DAT '
         XLIKE=-0.5d0*fvalue -DETL*(KsFIT(1)+ksfit(2))
         WRITE(*,'(A,G20.10)')' Log L = ',xlike
      end if
      FSTART=FVALUE
      FFM=FVALUE

C-------------------------------------------------------------------------
C     START OF A ROUND OF ITERATION
C-------------------------------------------------------------------------

3000  NROUND=NROUND+1

C     SAVE STARTING POINT (P0) AND CORRESPONDING FUNCTION VALUE (F0)
      XVEC0=XVEC(:kparm)
      FVAL0=FVALUE
      if(fvalue<ffm)ffm=fvalue

C     VARIABLES TO DETERMINE DIRECTION WITH LARGEST CHANGE
      DELMAX=0.D0
      IINEW=0

C     ---------------------------------------------------
C     PHASE I : CARRY OUT LINEAR SEARCH IN ALL DIRECTIONS 
C     ---------------------------------------------------

      DO IDIR=1,MPARM

C     ... SKIP PARAMETERS HELD CONSTANT  
      jdir=iopvec(idir)
      IF(IPM(jdir).NE.0)cycle

C     ... SAVE FUNCTION VALUE  AT BEGINNING OF DIRECTION
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
     &                 ' max.L',-0.5d0*ffm -DETL*(KsFIT(1)+ksfit(2))
      end do                       ! idir

C     ---------------------------------------------------
C     PHASE II : EXAMINE NEW DIRECTION OF SEARCH
C     ---------------------------------------------------

C     EXTRAPOLATED POINT & FUNCTION VALUE
      write(*,'(a)')' EXTRAPOLATED POINT'
      XTRY=2.D0*XVEC(:kparm)-XVEC0
      CALL DxLIK3(XTRY,FVAL2,xlike,0)
      if(mparm<kparm)xvec(kparm)=xtry(kparm)
      
      if(kparm>44)write(*,'(i6,g20.10)')ncall,xlike
      if(fval2<ffm)ffm=fval2

C     CHECK WHETHER NEW DIRECTION IS TO BE USED 
      FF1=(FVAL0-2.D0*FVALUE+FVAL2)*((FVAL0-FVALUE-DELMAX)**2)
      FF2=0.5D0*DELMAX*((FVAL0-FVAL2)**2)
      IF( (FVAL2.GE.FVAL0) .OR. (FF1.GE.FF2) )GO TO 3500

C     ---------------------------------------------------
C     PHASE III : MINIMIZE ALONG NEW DIRECTION
C     ---------------------------------------------------

C     NEW DIRECTION
      dirvec=0.d0
      do i=1,kparm
      if(ipm(i).eq.0)dirvec(i)=xvec(i)-xvec0(i)
      end do

C     KEEP DIRECTIONS 1 TO IINEW-1; MOVE UP IINEW TO NPARM-1
      DO II=IINEW,MPARM-1
      IF(IPM(iopvec(II)).Eq.0)then
         JJ=II+1
3311     IF(IPM(iopvec(JJ)).NE.0)THEN
           JJ=JJ+1
           IF(JJ.LE.MPARM)GO TO 3311
            KK=II
            GO TO 3312
         END IF
         DIRECT(:,II)=DIRECT(:,JJ)
         STPVEC(II)=STPVEC(JJ)
      END IF
      end do

C     ... REPLACE LAST DIRECTION VECTOR BY NEW DIRECTION
      KK=MPARM
3312  XX=0.D0
      DO I=1,mPARM
      IF( (IPM(I).EQ.0).AND.(XX.LT. DABS(DIRVEC(I)/EVEC(I))))
     *                   XX=DABS(DIRVEC(I)/EVEC(I))
      END DO
      STPVEC(KK)=ESCALE/XX

C     MINIMIZE ALONG NEW DIRECTION
      ZVEC= (/ -1.D0, 0.D0, 1.D0 /)
      FVEC(1)=FVAL0
      FVEC(2)=FVALUE
      FVEC(3)=FVAL2
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
      DO  I=1,mPARM
      IF( (IPM(I).EQ.0).AND.(XX.LT. DABS(DIRVEC(I)/EVEC(I)) ) )
     *                   XX=DABS(DIRVEC(I)/EVEC(I))
      END DO
      XDEL=0.D0
      DO I=1,kPARM
      if(ipm(i).ne.0)cycle
      XX=DABS( (XVEC(I)-XVEC0(I))/XVEC0(I) )
      IF(XX.GT.XDEL)XDEL=XX
      end do
      XDEL=XDEL*100.D0

      if(fvalue>ffm.or.fval2>ffm)then
        do i=min0(ncall,mxfunc),1,-1
        if(ffm.eq.feval(1,i))then
          xvec(:kparm)=feval(2:kparm+1,i)
          fvalue=ffm
          write(*,'(a,g20.10)')' Point replaced',fvalue
          exit
        end if
        end do
      end if

      ximpr=-0.5d0*(fvalue-fval0)
      XLIKE=-0.5d0*fvalue -DETL*(KsFIT(1)+ksfit(2))
      WRITE(IUN66,3699)NROUND,NCALL,xlike,ximpr,XDEL
      WRITE(*,3699)NROUND,NCALL,xlike,ximpr,XDEL
      INQUIRE(unit=iun28,opened=lopen)
      if(lopen) close(iun28)
      open(iun28,file='Iterates',status='unknown',form='formatted',
     *                                         position='append')
      WRITE(iun28,3699)NROUND,NCALL,xlike,ximpr,XDEL
      IF( DABS(FVAL0-FVALUE).LT.FLEVEL)THEN
         ICONV=1
         WRITE(IUN66,'(2a,g16.6)')' CONVERGENCE : CHANGE IN ',
     &                                         'FUNCTION VALUE',FLEVEL
         WRITE(*,'(a,g16.6)')' CONVERGENCE : CHANGE IN FUNCTION VALUE'
     &                                                         ,FLEVEL
      ELSE IF(XDEL.LT.XCONV)THEN
         ICONV=1
         WRITE(IUN66,'(a,g16.6)')' CONVERGENCE : CHANGE IN PARAMETERS'
     &,                                                         XCONV
         WRITE(*,'(a,g16.6)')' CONVERGENCE : CHANGE IN PARAMETERS',
     &                                                          XCONV
      ELSE
         ICONV=0
         IF(NROUND.LT.MROUND)GO TO 3000
      END IF

C     ------------
C     HOUSEKEEPING
C     ------------

      FMIN=FVALUE
      deallocate(iopvec,iwrk,stpvec,evec,xtry,dirvec,xvec0,direct)

      RETURN
3699  FORMAT(' IT=',I4,3X,'NC=',I5,3X,'F=',G20.10,3X,'D=',G15.7,'  CH='
     *                                    ,G10.3)

      contains

c     ================================
      subroutine dflmin(iotrip)
c     ================================

      integer, intent(in) :: iotrip

      real(8)      :: z23,z31,z12,zz23,zz31,zz12,det,det2,d1,d2,d3,zbig
      integer           :: ii,ind,ntrial,im
      integer, external :: ipsmin

c     initial step must be to the 'right'
      if(zstep.lt.0)zstep=-zstep
      zzstep=dmin1(zzlim,0.1d0*zstep)
      zzstep=dmax1(zzstep,20.d0*zaccur)

c     initial triplett has been established in calling routine
      if(iotrip.ne.0)go to 9
      zlimit=10.d0*zzstep

c     first trial point 
      ntry=0
      zz=zzstep
2     xtry=xvec(:kparm)+zz*dirvec
      call dxlik3(xtry,ftry,xlike,0)
      if(mparm<kparm)xvec(kparm)=xtry(kparm)
      if(kparm>44)write(*,'(i6,2g20.10,i4)')ncall,xlike,
     *                                    xtry(jdir)*ffvec(jdir),jdir
      if(dabs(ftry-big)<zz0)then
         zz=0.95d0*zz
         ntry=ntry+1
         if(ntry>22)stop 'DFPOW3'
         GO to 2
      end if
      if(ftry<ffm)ffm=ftry
      if(ftry>fvalue)then
c        ... trial point worse than initial; reverse search direction 
         zvec(2)=0.d0
         zvec(3)=zz
         zz=-zzstep
         fvec(2)=fvalue
         fvec(3)=ftry
         ind=1
         zbig=-1.d12
      else if(ftry<fvalue)then
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
         write(*,'(a)')'dflmin : max. change in parameter value does'
         write(*,'(a)')'         not alter function value(s) !!!'
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
            ntry=0
            go to 4
         else
            ii=0
            go to 7
         end if
      end if

c     second trial point
      ntry=0
4     ntrial=ntrial+1
      xtry=xvec(:kparm)+zz*dirvec
      call dxlik3(xtry,ftry,xlike,0)
      if(mparm<kparm)xvec(kparm)=xtry(kparm)
      if(ftry<ffm)ffm=ftry
      if(kparm>44)write(*,'(i6,2g20.10,i4)')ncall,xlike,
     &                                  xtry(jdir)*ffvec(jdir),jdir
      if(dabs(ftry-big)<zz0)then
         zz=0.95d0*zz
         ntry=ntry+1
         if(ntry>22)stop 'DFPOW3'
         GO to 4
      end if

!     order triplett
      call trip_ord

c     quadratic inter/extra-polation      
9     z23=zvec(2)-zvec(3)
      z31=zvec(3)-zvec(1)
      z12=zvec(1)-zvec(2)
      zz23=zvec(2)*zvec(2)-zvec(3)*zvec(3)
      zz31=zvec(3)*zvec(3)-zvec(1)*zvec(1)
      zz12=zvec(1)*zvec(1)-zvec(2)*zvec(2)
      det2=z23*fvec(1)+z31*fvec(2)+z12*fvec(3)
      if(dabs(det2).lt.zero)then
         write(*,'(a,g16.6)')' DFLMIN : equations undetermined',det2
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
               write(*,'(a,2g13.6,a,g13.6)')' d1,d3',d1,d3,
     *                                            '  zlimit',zlimit
               write(*,'(a,3f10.5)')' zvec', zvec
               write(*,'(a,3f10.5)')' fvec', fvec
               stop 'Error : should not occur'
            end if
         else
c           ... point acceptable;
            ii=0
c           check for convergence
            if( (d1.lt.zaccur) .or. (d3.lt.zaccur) .or. 
     *                (dabs(zvec(2)-zz).lt.zaccur) )go to 10
            im=ipsmin(fvec,big,3)
c            im=minloc(fvec)
            if( dabs(zz-zvec(im)).le. zlevel*dabs(zz) )go to 10 
         end if
      end if

c     repeat quadratic interpolation
7     if(ii.ne.0)zz=zvec(ii)+dsign(zlimit,zvec(ii)-zvec(2))
      if(ntrial.lt.12)go to 4

c     search completed; update parameter vector
10    xx=big
      im=0
      do i=1,3
      if(fvec(i)<xx)then
         xx=fvec(i)
         im=i
      end if
      end do

      fvalue=fvec(im)
!      xvec(:kparm)=xvec(:kparm)+zvec(im)*dirvec
      xvec(:mparm)=xvec(:mparm)+zvec(im)*dirvec
      if(fvalue>ffm)then
         write(*,'(a,2g20.10,i4)')' ffm',ffm,fvalue,im
         do i=min0(ncall,mxfunc),1,-1
         if(ffm.eq.feval(1,i))then
            xvec(:kparm)=feval(2:kparm+1,i)
            fvalue=ffm
            write(*,'(a)')' Point replaced'
            return
          end if
          end do
      end if
      return
      END subroutine dflmin

!     ================
      subroutine ipow
!     ================

      integer :: i,ii,j,jj, mxiter=999

      ZLEVEL=0.03D0

      if(isilen.eq.1 .or. iosrch>0)then  ! use default values
         j=0
         do i=mparm,1,-1
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
         call optdef(jj,1,mparm,1)
         j=0
         do i=jj,mparm
         j=j+1
         iopvec(j)=i
         end do
         do i=1,jj-1
         j=j+1
         iopvec(j)=i
         end do
      else if(ii.eq.2)then 
         write(*,*)'Start at parameter no. ?'
         call optdef(jj,1,mparm,mparm)
         j=0
         do i=jj,1,-1
         j=j+1
         iopvec(j)=i
         end do
         do i=mparm,jj+1,-1
         j=j+1
         iopvec(j)=i
         end do
      else if(ii.eq.3)then
         iwrk=0
         do i=1,mparm
 91      write(*,*)'running no. (of parameter) for direction no.',i
         call option(jj,1,mparm)
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
         do i=kparm+1,mparm
         do j=i1,mparm
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

      write(*,'(a,g16.6)')' dfpowm  : flevel =',flevel
      write(*,'(a,g16.6)')'           xconv  =',xconv
      write(*,'(a,g16.6)')'           escale =',escale
      write(*,'(a,i16)')'           mround =',mround
      return
      end subroutine ipow

!     ===================
      subroutine trip_ord
!     ===================

      integer :: ind,i
      real(8) :: ff

!     check for bracket
      ff=dmax1(fvec(1),fvec(3))
      if(ff.ne.big .and. ftry.lt.fvec(1) .and. ftry.lt.fvec(2) .and. 
     *                     zz.gt.zvec(1) .and. zz.lt.zvec(2))then
         ind=2
         fvec(3)=fvec(2)
         zvec(3)=zvec(2)
      else if(ff.ne.big .and. ftry.lt.fvec(3) .and. ftry.lt.fvec(2)
     *                    .and. zz.gt.zvec(2) .and. zz.lt.zvec(3))then
         ind=2
         fvec(1)=fvec(2)
         zvec(1)=zvec(2)

c     discard worst point
      else if(fvec(1).gt.fvec(3))then
         if(zz.lt.zvec(2))then
            ind=1
         else if(zz.lt.zvec(3))then
            ind=2
         else
            ind=3
         end if
         do i=1,ind-1
         zvec(i)=zvec(i+1)
         fvec(i)=fvec(i+1)
         end do
      else if(fvec(3).gt.fvec(1))then
         if(zz.gt.zvec(2))then
            ind=3
         else if(zz.gt.zvec(1))then
            ind=2
         else
            ind=1
         end if
         do i=3,ind+1,-1
         zvec(i)=zvec(i-1)
         fvec(i)=fvec(i-1)
         end do
      else
         write(*,'(a,3f16.6)')' zvec',zvec
         write(*,'(a,3g16.6)')' fvec',fvec
         write(*,'(a,g16.6)')' zz',zz
         stop 'trip_ord'
      end if
      zvec(ind)=zz
      fvec(ind)=ftry
      write(*,'(a,3g16.6)')' z ',zvec

      return
      end subroutine trip_ord

      END subroutine dfpow3







