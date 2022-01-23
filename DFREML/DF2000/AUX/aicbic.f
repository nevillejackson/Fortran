      PROGRAM AICBIC

!     program to calculate AIC and BIC from log L - for multiple files

!     Input : Files DF17#DAT (re-named) - must start with df or DF !
!     Output : AIC.out

      integer, parameter     :: mf=20 ! max no. of "DF17#DAT" files
      character(len=70)      :: aaa
      character(len=40)      :: cwd
      character(len=26)      :: fn, fnv(mf)
      character(len=1), dimension(mf) :: aa=' ', bb=' ',cc=' ',hh=' '
      real,dimension (mf)    :: xlik, aic, bic, cic, hqc
      integer, dimension(mf) :: nnp
      real(8)                :: df
      integer                :: ii,getcwd
      external getcwd

!     READ DF17#DAT FILES & PICK OUT LIKELIHOOD VALUES
      nf=0
 50   print *,'input file ?'
      read(*,'(a)',end=99)fn
      print *,fn
      if( fn(1:2).ne.'df' .and. fn(1:2).ne.'DF')go to 99
      nf=nf+1
      fnv(nf)=fn
      open(1,file=fn)
      np=0
 150  read(1,'(a)',end=199)aaa
      read(aaa,*,err=299)xx
      np=np+1
      go to 150
 299  xlik(nf)=xx
      nnp(nf)=np-1
      print *,nf,np,xlik(nf)
      close(1)
 199  go to 50

 99   read(fn, *)nrec,nrfix
      xnn=nrec
      print *,nrec,nrfix


!     WRITE OUT SUMMARY
      open(2,file='AIC.out')
      ii=getcwd(cwd)
      write(2,'(2a)')'Directory : ',cwd

!     calculate AIC, BIC, etc.
!     ... use formulae given by wolfinger(1993)
      write(2,'(26x,a,7x,a,4(9x,a))')'  np','log L','AIC','BIC','HQC',
     &                                              'CIC'
      do i=1,nf
      df=xnn-nrfix
      print *,df,log(df)
!     akaike's information criterion
      aic(i)=-2.d0*xlik(i)+2.d0*nnp(i)
!     schwarz' baysian information criterion
      bic(i)=-2.d0*xlik(i)+nnp(i)*log(df)
!     consistent AIC
      cic(i)=-2.d0*xlik(i)+nnp(i)*(log(df)+1)
!     hannan & quinn citerion
      hqc(i)=-2.d0*xlik(i)+2.d0*nnp(i)*log(log(xnn-nrfix))
      print *,i,xlik(i),aic(i),cic(i)
      end do
      ia=1
      ib=1
      ic=1
      ih=1
      do i=2,nf
      if(aic(i)<aic(ia))ia=i
      if(bic(i)<bic(ib))ib=i
      if(cic(i)<cic(ic))ic=i
      if(hqc(i)<hqc(ih))ih=i
      end do
      aa(ia)='*'
      bb(ib)='*'
      cc(ic)='*'
      hh(ih)='*'
      do i=1,nf
      write(2,'(a,i4,5(f11.2,a))')fnv(i),nnp(i),xlik(i),' ',aic(i),aa(i)
     &,                           bic(i),bb(i),hqc(i),hh(i),cic(i),cc(i)
      end do

      write(2,'(a,i8)')'sample size given        =',nrec
      write(2,'(a,i8)')'... DF for fixed effects =',nrfix
      df=xnn-nrfix
      write(2,*)'BIC factor                      =',log(df)
      write(2,*)'CAIC factor                     =',log(df+1)
      write(2,*)'HQC factor                      =',2.d0*log(log(df))

      END PROGRAM AICBIC



















