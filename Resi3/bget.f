      subroutine bget(mi,name,a,nr,nc,ms,mull)
c-----read a matrix from a ' Blocked format' file to a double precision array
c-----binary read
c-----32 x 16_byte elements per block
      double precision a(*)
      double precision abuf(32)
      integer ibuf(32),jbuf(32)
      character*40 name
      common /stdio/lin,lout,lmess
      common /limit/ maxn,maxnam,maxiw
      logical mull,first
      data lenbuf/32/
c-----
      mull=.false.
      first=.true.
c-----
    1 read(mi,end=99,err=98,iostat=ios) ibuf,jbuf,abuf
      if(first) then
        nr=ibuf(1)
        nc=jbuf(1)
        ms=abuf(1)
        do 20 k=2,lenbuf
        i=ibuf(k)
        j=jbuf(k)
   20   a(iloc(i,j,nr,nc,ms))=abuf(k)
        first=.false.
      else
        do 21 k=1,lenbuf
        i=ibuf(k)
        j=jbuf(k)
   21   a(iloc(i,j,nr,nc,ms))=abuf(k)
      endif
      go to 1
c-----
   99 write(lmess,16)nr,nc,ms,name
   16 format(' matrix read: nrows & cols = ',2i4,' ms = ',i3
     +  ,' file = ',a)
      return
c-----
   98 mull=.true.
      write(lmess,97)ios,name
   97 format(' error reading matrix:  iostat = ',i4,
     +     ' file = ',a)
      return
      end
