!===========================================================================
      SUBROUTINE      DxAPSE 
!===========================================================================

      use parameters
      use names
      use units
      use derivs
      use parmap
      use numbers
      use hess_wrk

!     local variables
      CHARACTER(len=20)                      :: aa1,aa2
      integer                                :: ii,npp,ij,i,j
      integer, dimension (:), allocatable    :: ivec
      real(8)                                :: xx,pp,cc,oo,det
      real(8), dimension (:), allocatable    :: sigp,varp
      real(8), dimension (:,:), allocatable  :: covp

      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'-----------------------------------'
      WRITE(IUN66,*)'APPROXIMATION OF SAMPLING VARIANCES'
      WRITE(IUN66,*)'-----------------------------------'
      WRITE(IUN66,*)' '

      call all_hessian(nparm)

      npp=NPARM*(NPARM+1)/2
      allocate(ivec(0:nparm),stat=ii)
      if(ii>0)stop 'dxase3 : alloc'

      HESS=-DEVL2(:npp)
      CALL DKMWHF(HESS,W1,W2,DET,ZERO,IW,KRANK,NPARM,0)

      WRITE(IUN66,900)'NUMBER OF (CO)VARIANCE COMPONENTS',NPARM
      WRITE(IUN66,900)'RANK OF APPROXIMATE INFORMATION MATRIX',KRANK
      WRITE(IUN66,901)'LOG DETERMINANT ......................',DET
      if(det<12.0)then
      WRITE(IUN66,901)'DETERMINANT ......................',dexp(DET)
      IF(KRANK.LT.NPARM.OR.DEXP(DET).LT.ZERO)
     * WRITE(IUN66,*)'APPROXIMATE INFORM. MATRIX ILL-CONDITIONED ??!'
      end if

      WRITE(IUN66,*)' '
      INEG=0
      WRITE(IUN66,*)'PARAMETER ESTIMATES WITH THEIR APPROX. S.E.'
      DO I=1,NPARM
      XX=HESS(IHMII(I,NPARM))
      IF(XX.GT.ZERO)THEN
         XX=DSQRT(XX)
      ELSE if(ipm(i).eq.0)then
         INEG=INEG+1
      END IF
      WRITE(*    ,905)I,PARVAR(I),XVEC(I),XX
      WRITE(IUN66,905)I,PARVAR(I),XVEC(I),XX
      end do
      WRITE(IUN66,*)' '

      IF(INEG.GT.0)THEN
         WRITE(IUN66,*)'APPROXIMATION OF SAMPLING ERRORS FAILED !'
         WRITE(IUN66,*)'NO. OF NEGATIVE DIAG.S OF I**(-1) =',INEG
         RETURN
      END IF

      if(imodel.eq.4.or.imodel.eq.8.or.irpmod.eq.2)go to 100
      if(iopmod.eq.5 .or. iopmod.eq.6)go to 100

c     work out approx. s.e. for phenotypic (co)variances

      allocate (sigp(nqq),varp(nqq),covp(nqq,6),stat=ii)
      if(ii>0)stop 'dxapse : alloc 2'

      write(iun66,*)'phenotypic variance components'
      ijq=0
      do iq=1,nq
      do jq=iq,nq
      ijq=ijq+1
      ivec=0
      do ll=1,7
      if(nparno(iq,jq,ll).gt.0)ivec(nparno(iq,jq,ll))=1
      end do
!     this code needs adjusting for models including sigam or sigr!

      xx=0.d0
      pp=0.d0
      do i=1,nparm
      pp=pp+ivec(i)*xvec(i)
      do j=1,nparm
      if(ivec(i).eq.1 .and.ivec(j).eq.1)xx=xx+hess(ihmssf(i,j,nparm))
      end do
      end do
      sigp(ijq)=pp
      varp(ijq)=xx
      WRITE(*    ,906)nparm+ijq,'SIG P   ',Iq,jq,pp,dsqrt(xx)
      WRITE(iun66,906)nparm+ijq,'SIG P   ',Iq,jq,pp,dsqrt(xx)

c     work out covariances between sig_i (i=a,m,c,e) and sig_p
      do ll=1,7
      ii=nparno(iq,jq,ll)
      if(ii.gt.0)then
         cc=0.d0
         do j=1,nparm
         if(ivec(j).eq.1)cc=cc+hess(ihmssf(ii,j,nparm))
         end do
      end if
      covp(ijq,ll)=cc
      end do

      end do
      end do

      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'APPROXIMATE SAMPLING ERRORS OF GENETIC PARAMETERS'
      ll=1
      aa1='Heritability        '
      aa2='Genetic correlation '
      call dxbse3 

      if(ioprn2.eq.1)then
         ll=2
         aa1='Maternal Heritab.   '
         aa2='Mat. Gen. correl.   '
         call dxbse3 
      end if

      if(ioprn1.eq.1)then
         ll=4
         aa1='"C-squared"         '
         aa2='Perm. env. correl.  '
         call dxbse3
      end if

      if(ioprn3.eq.1)then
         ll=4
         aa1='"q-squared"         '
         aa2='Add.RE correl.  '
         call dxbse3
      end if

      ll=7
      aa1='                    '
      aa2='Residual correlation'
      call dxbse3 

 100  WRITE(IUN66,*)' '
      WRITE(IUN66,*)'APPROXIMATE SAMPLING CORRELATIONS',
     *' (AMONG COV. COMPONENTS)'
      oo=1.d0
      IJ=0
      DO  I=1,NPARM
      IJ=IJ+1
      XX=HESS(IJ)
      DO  J=I+1,NPARM
      IJ=IJ+1
      if(xx.ge.zero.and.hess(ihmii(j,nparm)).ge.zero)then
         hess(ij)=HESS(IJ)/DSQRT(XX*HESS(IHMII(J,NPARM)))
      else
         hess(ij)=0.d0
      end if
      end do
      end do
      do i=1,nparm
      if(ipm(i).eq.0)write(iun66,908)parvar(i),
     *                           (hess(ihmssf(i,j,nparm)),j=1,i-1),oo
      end do
      WRITE(IUN66,*)' '

      IF(JPARAM.EQ.2)THEN
        DO IJ=1,KPARM*(KPARM+1)/2
C     .... check about factor 1/2 here !!!! av. info vs sum of obs+exp
         HESS(IJ)=-0.5d0*DEVL2(IJ)
         END DO
         CALL DKMWHF(HESS,W1,W2,DET,ZERO,IW,KRANK,kPARM,0)
         WRITE(IUN66,900)'NUMBER OF PARAMETERS',KPARM
         WRITE(IUN66,900)'RANK OF APPROXIMATE INFORMATION MATRIX',KRANK
         WRITE(IUN66,901)'LOG DETERMINANT ......................',DET
         WRITE(IUN66,901)'DETERMINANT ......................',dexp(DET)
         IF(KRANK.LT.NPARM.OR.DEXP(DET).LT.ZERO)
     *   WRITE(IUN66,*)'APPROXIMATE INFORM. MATRIX ILL-CONDITIONED ??!'
         WRITE(IUN66,*)' '

         WRITE(IUN66,*)'PARAMETER ESTIMATES WITH THEIR APPROX. S.E.'
         INEG=0
         DO I=1,kPARM
         XX=HESS(IHMII(I,kPARM))
         IF(XX.GT.ZERO)THEN
            XX=DSQRT(XX)
            WRITE(*    ,905)I,PARAM(I),save(I),XX
         ELSE
            INEG=INEG+1
         END IF
         WRITE(IUN66,905)I,PARAM(I),SAVE(I),XX
         end do
         WRITE(IUN66,*)' '
         IF(INEG.GT.0)THEN
            WRITE(IUN66,*)'APPROXIMATION OF SAMPLING ERRORS FAILED !'
            WRITE(IUN66,*)'NO. OF NEGATIVE DIAG.S OF I**(-1) =',INEG
            RETURN
         END IF

         WRITE(IUN66,*)' '
         WRITE(IUN66,*)'APPROXIMATE SAMPLING CORRELATIONS'
         oo=1.d0
         IJ=0
         DO  I=1,kPARM
         IJ=IJ+1
         XX=HESS(IJ)
         DO  J=I+1,kPARM
         IJ=IJ+1
         hess(ij)=HESS(IJ)/DSQRT(XX*HESS(IHMII(J,kPARM)))
         end do
         end do
         do i=1,kparm
         write(iun66,908)param(i),(hess(ihmssf(i,j,kparm)),j=1,i-1),oo
         end do
         WRITE(IUN66,*)' '
      end if

      call deall_hessian(kparm)
      RETURN
900   FORMAT(1X,A,T50,'=',I16)
901   FORMAT(1X,A,T50,'=',G18.10)
905   FORMAT(I4,2X,A,T25,':',2G18.8)
906   FORMAT(i4,2x,a,2I2,T25,':',2G18.8)
908   FORMAT(1X,A,(t14,10f6.2))

      contains

c     =================
      subroutine dxbse3
c     =================

!     calculate & write out approximate s.e. for heritabilities + correlations

      integer :: iq,ii,ii1,jq,ij,jj
      real(8) :: x,y,z,vxx,vxy,vxz,vyy,vyz,vzz,se,r,det,var
      real(8) :: vartio
      EXTERNAL vartio

      do iq=1,nq
      ii=nparno(iq,iq,ll)
      if(ii.gt.0)then
         ii1=nparno(iq,iq,1) 
         if(ll.ne.5)then
            x=xvec(ii)
            y=sigp(ii1)
!            vxx=hess(ihmii(ii,nparm))
!            vxy=covp(ihmii(iq,nq),ll)
!            vyy=varp(ii1)                ! phenotypic variance
            se=0.d0
!            var=vartio(x,y,vxx,vxy,vyy)
!            if(var.gt.0)se=sqrt(var)
            WRITE(*,907)ii,aa1,Iq,x/y,se
            WRITE(iun66,907)ii,aa1,Iq,x/y,se
         end if

         do jq=iq+1,nq
         ij=nparno(iq,jq,ll)
         jj=nparno(jq,jq,ll)
         if(ij.gt.0 .and.jj.gt.0)then
            x=xvec(ij)                 ! covariance
            y=xvec(ii)                 ! variance 1
            z=xvec(jj)                 ! variance 2
!            vxx=hess(ihmii(ij,nparm))
!            vyy=hess(ihmii(ii,nparm))
!            vzz=hess(ihmii(jj,nparm))
!            vxy=hess(ihmssf(ij,ii,nparm))
!            vxz=hess(ihmssf(ij,jj,nparm))
!            vyz=hess(ihmssf(ii,jj,nparm))
c            se=corrse(x,y,z,vxx,vyy,vzz,vxy,vxz,vyz)
            r=x/dsqrt(y*z)              ! correlation
            WRITE(*,908)ij,aa2,Iq,jq,r,se
            WRITE(iun66,908)ij,aa2,Iq,jq,r,se
         end if
         end do
      end if
      end do

 907  FORMAT(i4,2x,a,I2,2x,T35,':',2F10.4)
 908  FORMAT(i4,2x,a,2I2,T35,':',2F10.4)
      return
      end subroutine dxbse3

      END subroutine dxapse















