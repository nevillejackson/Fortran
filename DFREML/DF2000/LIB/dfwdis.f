C=======================================================================
      subroutine dfwdis(nn,nl,npl,nlow,nup,nall,iun66)
C=======================================================================
                                                                        
C     Routine to write out distribution over subclasses linewise        

c     parameters
c     nn    : vector with no.s per subclass
c     nl    : no. of counts to be printed per line
c     nlow  : lowest filled subclass
c     nup   : highest filled subclass
c     nall  : total number, calculated within the routine if zero
c             on entry
c     iun66 : unit number for output file

c----------------------------------------------------------------------

      parameter(maxpl=25)
      DIMENSION nn(0:nl),ILINE(maxpl),JLINE(maxpl),XLINE(maxpl)              
                                                                        
      if(npl.gt.maxpl)then
         write(*,*)'routine "dfwdis" : max. no per line exceeded'
         write(*,*)'  ... current maximum allowed =',maxpl
         write(*,*)'  ... no. requested           =',npl
         stop
      end if

c     determine range with non-zero counts
      do 1 i=0,nl
      if(nn(i).gt.0)then
         nlow=i
         go to 2
      end if     
1     continue
      return
2     do 3 i=nl,0,-1
      if(nn(i).gt.0)then
         nup=i
         go to 4
      end if     
3     continue
                
c     total no.
4     if(nall.eq.0)then
         do 5 i=nlow,nup
5        nall=nall+nn(i)
      end if
      zz=100./nall
      ii=0

      DO 6 I=nlow,nup
      if(nn(i).gt.0)then
         ii=ii+1
         iline(ii)=i
         jline(ii)=nn(i)
         xline(ii)=zz*nn(i)
      end if
      if(ii.eq.npl)then
c        line complete, write out
         WRITE(IUN66,10)'size',(iline(l),l=1,npl)
         WRITE(IUN66,10)'no.s',(jline(l),l=1,npl)
         WRITE(IUN66,11)' %  ',(xline(l),l=1,npl)
         write(iun66,*)' '
         ii=0
      end if
6     continue
      if(ii.gt.0)then
         WRITE(IUN66,10)'size',(iline(l),l=1,ii)
         WRITE(IUN66,10)'no.s',(jline(l),l=1,ii)
         WRITE(IUN66,11)' %  ',(xline(l),l=1,ii)
         write(iun66,*)' '
      end if
      return

10    format(1x,a,25i7)
11    format(1x,a,25f7.1)
      END                                                               

