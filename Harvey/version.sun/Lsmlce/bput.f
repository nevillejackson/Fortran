      subroutine bput(mo,name,a,nr,nc,ms,mull)
c-----write a matrix from a double precision array to a 'Blocked format' file.
c-----  32 x 16_byte elements per block
c-----  binary write
      double precision a(*)
      double precision temp,tol
      double precision abuf(32)
      integer ibuf(32),jbuf(32)
      character*80 name
      common /stdio/ lin,lout,lerr
      common /limit/ maxn,maxnam,maxiw
      logical mull,first
      data lenbuf /32/,tol/1.0d-12/
c----- defined here specially for lsmlce - normally do in main or blockdata
      data maxn /200/,maxnam /40/,maxiw /20/
c-----
      mull=.false.
      first=.true.
      temp=0.0
c-----
      ibuf(1)=nr
      jbuf(1)=nc
      if(ms.ge.0) then
        abuf(1)=ms
      else
c       special harvey symmetric case
        abuf(1) = 1
      endif
c-----
    1 kount=1
      do 30 i=1,nr
      if(ms .eq	. o) then
        jmin=1
        jmax=nc
      else if(ms.eq.1) then
        jmin=1
        jmax=i
      else if(ms.eq.2) then
        jmin=i
        jmax=i
      else if(ms.lt.0) then
c       special case upper triangle rowwise for harvey
        jmin = i
        jmax = -ms
      else
        call jobend
      endif
      do 30 j=jmin,jmax
      temp=a(iloc(i,j,nr,nc,ms))
      if(dabs(temp).ge.tol) then 
        kount=kount+1
        ibuf(kount)=i
        jbuf(kount)=j
        abuf(kount)=temp
        if(first.and.kount.eq.lenbuf) then
          first=.false.
          go to 20
        else if (.not.first.and.kount.eq.lenbuf) then
          go to 20
        else 
          go to 30
        endif
c----- write full buffer
   20   write(mo,err=98,iostat=ios)ibuf,jbuf,abuf
   10   format(32a)
        kount=0
        go to 30
c-----do nothing if element zero
      endif
   30 continue  
c-----write part full buffer   - pad by extend last element
      if(kount.gt.o) then
        j=kount+1
        do 31 i=j,lenbuf
        abuf(i)=temp
        jbuf(i)=nc
   31   ibuf(i)=nr
        write(mo,err=98,iostat=ios) ibuf,jbuf,abuf
      endif
      write(lmess,16) nr,nc,ms,name
   16 format(' matrix written: nrows & cols = ',2i4,' ms = ',i1,
     +     ' file = ',a)
      return
c----
   98 mull=.true.
      write(lmess,97) ios,name
   97 format(' error writing matrix: iostat = ',i4,
     +    ' file = ',a)
      return
      end

