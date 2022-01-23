!===========================================================================
      subroutine phimat (kq,nfit,ipar,iq)
!===========================================================================

      use legendre
      use phimatrix
      use ages
      use numbers
      use units, only : zz0

      integer, intent(in)    :: kq,nfit,ipar,iq

      real(8)                :: xlow,xupp,aamin,aamax,aa,pp,age,cc,dd
      character (len=25)     :: OLD='old',FORMA='formatted',FSTAND
      integer                :: kun,i

!     set up "phi" matrix as in Kirkpatrick et al.'s paper (1990) 
      if(irropt(ipar,iq).eq.1)then
!        ... set up matrix of coefficients for legendre polynomials
         CALL DXLGND (nfit)

c        set up vector of standardised ages;  assume ages are in order
         xlow=-1.d0
         xupp=1.d0
         astar(:kq)=iiage(:kq,irrmet(ipar,iq))
         if(ialog.eq.-1)then
            write(*,*)'Transform ages to log scale ?'
            call yndef(ialog,0)
         end if
         if(ialog.eq.1)astar(:kq)=dlog(astar(:kq))
         aamin=astar(1)
         aamax=astar(kq)
         aa=(xupp-xlow)/(aamax-aamin)
         astar(:kq)=xlow+aa*(astar(:kq)-aamin)

c        evaluate legendre polynomials at given ages
         do ifit=1,nfit
         do i=1,kq
         aa=astar(i)
         pp=clgndr(1,ifit)
         if(dabs(aa)>zz0)then
            do j=2,ifit
            if(dabs(clgndr(j,ifit))>zz0)pp=pp+clgndr(j,ifit)*aa**(j-1)
            end do
         end if
         phi(i,ifit,ipar,iq)=pp
         end do
         end do

         return

!     read in user defined matrix "phi"

      else if(irropt(ipar,iq).eq.2)then
         write(*,*)' '
         write(*,*)' PHI Matrix No. =',ipar
         write(*,*)'Supply equivalent to matrix "phi" for ages in data'
         write(*,*)'Expect input from "DF21#DAT" (list-directed READ)'
         write(*,*)'   No. of ages (rows)            =',kq
         write(*,*)'   No. of coefficients (columns) =',nfit
         write(*,*)' '
         kUN=21
         FSTAND='DF21#DAT'
         CALL FCONCT(kUN,FSTAND,FORMA,OLD)
         do i=1,kq
         read(kun,*,end=99)phi(i,:nfit,ipar,iq)
         write(*,*)'read : age=',iiage(i,irrmet(ipar,iq)),'"phi" =',
     &                                         phi(i,:nfit,ipar,iq)
         end do
         return
 99      write(*,*)'subroutine "DXPHI" : Premature end of "DF21#DAT" !'
         write(*,*)'                     variable no. =',ipar
         write(*,*)' Remember you need to supply a "Phi" matrix for'
         write(*,*)' EACH random effect which fits a user-supplied '
         write(*,*)' function !!! (duplication may be necessary)   '
         stop 'Routine "DXPHI" '

!     set up matrix "phi" for L.R.Schaeffer's RR model
      else if (irropt(ipar,iq).eq.3 .and. nfit.eq.5)then
         write(*,*)' '
         write(*,*)'L.R.S.''s test day random regression model'
         phi(:,1,ipar,iq)=1.d0     ! scalar  coeff.
         do i=1,kq
         age=iiage(i,irrmet(ipar,iq))
         cc=age/305.d0
         phi(i,2,ipar,iq)=cc
         phi(i,3,ipar,iq)=cc*cc
         dd=dlog(1.d0/cc)
         phi(i,4,ipar,iq)=dd
         phi(i,5,ipar,iq)=dd*dd
         write(*,10)'age=',iiage(i,irrmet(ipar,iq)),'"phi" =',
     &                                             phi(i,:nfit,ipar,iq)
 10      format(1x,a,i5,2x,a,(5g14.6))
         end do
         return

!     ordinary polynomial regression
      else if (irropt(ipar,iq).eq.0 )then
         phi(:,1,ipar,iq)=1.d0     ! scalar  coeff.
         do i=1,kq
         age=iiage(i,irrmet(ipar,iq))
         if(age.eq.0)stop 'Routine "DXPHI" : zero age found ! '
         aa=age
         do j=2,nfit
         phi(i,j,ipar,iq)=aa
         aa=aa*age
         end do
         end do
         return
      end if

      end subroutine phimat



















