!======================================================================
      SUBROUTINE DFPOW1 (iopt,nparm,kparm,iconv,fmin,sige)
!======================================================================

      use likelihoods
      use names, only : param
      use powell
      use spasol
      use like
      use parameters
      use iterates
      use constants
      use units
      use numbers
      use platform

      implicit double precision (a-h,o-z)

      integer, intent(in)                 :: nparm,kparm,iopt
      integer, intent(out)                :: iconv
      real(8), intent(out)                :: fmin,sige
      real(8), dimension(:), allocatable  :: xtry,dirvec,xvec0,evec,
     &                                       stpvec
      integer, dimension(:), allocatable  :: iwrk,iopvec
      real(8), dimension(3)               :: fvec,zvec
      real(8), dimension(:,:), allocatable :: direct
      real(8) :: zstep, zzstep,zlimit,zzlim,zaccur,zlevel,evalue,deriv2,
     &           fvalue,fval0,ff1,ff2,time,tt,xsecs

      allocate(evec(nparm),stpvec(nparm),iopvec(nparm),xtry(nparm),
     &         xvec0(nparm),dirvec(nparm),direct(nparm,nparm),
     &         iwrk(nparm),stat=ii)

C     -------------------------------------------------
C     D.I.Y. VERSION OF POWELL'S DERIVATIVE-FREE SEARCH
C     -------------------------------------------------

      CALL ipow

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

C     FUNCTION VALUE FOR STARTING POINT
      CALL DFTIME(time,xsecs,isecs,0,'DFPOW1')
      evalue=xvec(nparm+1)
      CALL DFLIK1(NPARM,XVEC,FVALUE,EVALUE)
      IF(dabs(FVALUE-BIG)<10.d-8)stop 'dfpow : invalid starting point !'
      CALL DFTIME(time,xsecs,isecs,10,'DFPOW1')
      PRINT *,'CPU TIME (SEC.S) REQUIRED PER LOG L =',ISECS
      write(iun28,'(a,i12)')' Time per "DXLIK1" ',isecs

C-------------------------------------------------------------------------
C     START OF A ROUND OF ITERATION
C-------------------------------------------------------------------------

3000  NROUND=NROUND+1

C     SAVE STARTING POINT (P0) AND CORRESPONDING FUNCTION VALUE (F0)
      XVEC0=XVEC(:nparm)
      FVAL0=FVALUE

C     VARIABLES TO DETERMINE DIRECTION WITH LARGEST CHANGE
      DELMAX=0.D0
      IINEW=0

C     ---------------------------------------------------
C     PHASE I : CARRY OUT LINEAR SEARCH IN ALL DIRECTIONS 
C     ---------------------------------------------------

      DO IDIR=1,NPARM

C     ... SKIP PARAMETERS HELD CONSTANT  
      IF(IPM(IDIR).NE.0)cycle

C     ... SAVE FUNCTION VALUE  AT BEGINNING OF ITERATE
      FPRE=FVALUE

C     ... COPY DIRECTION
      DIRVEC(:nparm)=DIRECT(:,IDIR)

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
     &                 ' max.L',-0.5d0*fvalue

      end do ! idir

C     ---------------------------------------------------
C     PHASE II : EXAMINE NEW DIRECTION OF SEARCH
C     ---------------------------------------------------

C     EXTRAPOLATED POINT & FUNCTION VALUE
      PRINT *,'EXTRAPOLATED POINT'
      XTRY(:nparm)=2.D0*XVEC(:nparm)-XVEC0
      CALL DFLIK1(NPARM,XTRY,FVAL2,EVALUE)

C     CHECK WHETHER NEW DIRECTION IS TO BE USED 
      FF1=(FVAL0-2.D0*FVALUE+FVAL2)*((FVAL0-FVALUE-DELMAX)**2)
      FF2=0.5D0*DELMAX*((FVAL0-FVAL2)**2)
      IF( (FVAL2.GE.FVAL0) .OR. (FF1.GE.FF2) )GO TO 3500

C     ---------------------------------------------------
C     PHASE III : MINIMIZE ALONG NEW DIRECTION
C     ---------------------------------------------------

C     NEW DIRECTION
      dirvec=0.d0
      where(IPM.EQ.0)DIRVEC=XVEC(:nparm)-XVEC0

C     KEEP DIRECTIONS 1 TO IINEW-1; MOVE UP IINEW TO NPARM-1
      DO II=IINEW,NPARM-1
      IF(IPM(II).NE.0)cycle
      JJ=II+1
3311  IF(IPM(JJ).NE.0)THEN
         JJ=JJ+1
         IF(JJ.LE.NPARM)GO TO 3311
         KK=II
         GO TO 3312
      END IF
      DIRECT(:,II)=DIRECT(:,JJ)
      STPVEC(II)=STPVEC(JJ)
      end do

C     ... REPLACE LAST DIRECTION VECTOR BY NEW DIRECTION
      KK=NPARM
3312  XX=0.D0
      DO I=1,NPARM
      IF( (IPM(I).EQ.0) .AND. (XX.LT. DABS(DIRVEC(I)/EVEC(I)) ) )
     *                   XX=DABS(DIRVEC(I)/EVEC(I))
      end do
      STPVEC(KK)=ESCALE/XX

C     MINIMIZE ALONG NEW DIRECTION
      ZVEC(1)=-1.D0
      ZVEC(2)=0.D0
      ZVEC(3)=1.D0
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
         DIRECT(:,KK)=DIRVEC(:)*DD
         STPVEC(KK)=STPVEC(KK)/DD      
      END IF
    
C     ---------------------------
C     END OF A ROUND OF ITERATION
C     ---------------------------

C     CHECK FOR CONVERGENCE 
3500  CONTINUE
      XX=0.D0
      DO  I=1,NPARM
      IF( (IPM(I).EQ.0) .AND. (XX.LT. DABS(DIRVEC(I)/EVEC(I)) ) )
     *                   XX=DABS(DIRVEC(I)/EVEC(I))
      end do
      XDEL=0.D0
      DO I=1,NPARM
      XX=DABS( (XVEC(I)-XVEC0(I))/XVEC0(I) )
      IF(XX.GT.XDEL)XDEL=XX
      end do
      XDEL=XDEL*100.D0
      WRITE(IUN66,3699)NROUND,NCALL,-0.5D0*FVALUE,FVALUE-FVAL0,XDEL
      WRITE(*,3699)NROUND,NCALL,-0.5D0*FVALUE,FVALUE-FVAL0,XDEL
3699  FORMAT(' IT=',I4,3X,'NC=',I5,3X,'F=',F20.10,
     *                                3X,'D=',F15.10,'  CH=',F10.3)
      WRITE(iun28,3699)NROUND,NCALL,-0.5D0*FVALUE,FVALUE-FVAL0,XDEL
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
      DO II=MIN0(NCALL,MXFUNC),1,-1
      IF(dabs(FEVAL(NPARM+1,II)-FVALUE)<10.d-9)THEN
         SIGE=FEVAL(NPARM+2,ii)
         RETURN
      END IF
      end do
      print *,'dfpowl : sige not found among stored lik. values'
      print *,'ncall =',ncall,'    mxfunc =',mxfunc
      sige=0.d0
      return

      contains

C     =========================
      SUBROUTINE DFLMIN(IOTRIP)
c     =========================

c     initial step must be to the 'right'
      if(zstep.lt.0)zstep=-zstep
      zzstep=dmin1(zzlim,0.1d0*zstep)
      zzstep=dmax1(zzstep,20.d0*zaccur)

c     initial triplett has been established in calling routine
      if(iotrip.ne.0)go to 9
      zlimit=10.d0*zzstep

c     first trial point 
      zz=zzstep
 2    xtry(:nparm)=xvec(:nparm)+zz*dirvec(:nparm)
      call dflik1(nparm,xtry,ftry,evalue)
      if(dabs(ftry-big)<10.d-9)then
         zz=0.95d0*zz
         GO to 2
      end if
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
      call dflik1(nparm,xtry,ftry,evalue)
      if(dabs(ftry-big)<10.d-9)then
         zz=0.95d0*zz
         GO to 4
      end if
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
      return
      END subroutine dflmin

!     ================
      subroutine ipow
!     ================

      integer :: i,ii,j,jj, mxiter=999

      ZLEVEL=0.03D0

      if(iopt>0)then  ! use default values
         iopvec(:nparm)=(/ (i,i=nparm,1,-1) /)
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

      write(*,'(a,g16.6)')' dfpowm  : flevel =',flevel
      write(*,'(a,g16.6)')'           xconv  =',xconv
      write(*,'(a,g16.6)')'           escale =',escale
      write(*,'(a,i16)')'           mround =',mround
      return
      end subroutine ipow

      END subroutine dfpow1
