      subroutine rcard (in,k,l,ic,kd,iflag)                               
c-----returns iflag=0  ok
c-----        iflag=-1 endfile
c-----        iflag=+1 error
c-----  kd is card no within record
c-----   k is beginning subscript for ic(:)
c-----   l is ending subscript for ic(:)
      character ic*(*)
      include 'com7'
      include 'com8'
      if(in-5) 5,6,5
c-----read card_image record
    6 read(in,1000,end=1,err=9,iostat=ios) ic(1:lrec)
 1000 format(a)
      go to 2
c-----read a charm record
    5 read(in,1001,end=1,err=9,iostat=ios) lrec,kseq,ic(1:lrec)
 1001 format(i3,i6,1x,a)
    2 iflag=0
      return
c-----err branch
    9 iflag=1
      write(6,1003) in,ios
 1003 format(' error on reading unit ',i3,' ios = ',i4)
      return
c-----end branch
    1 iflag=-1
      write(6,1005) in,ios
 1005 format(' end of file on unit',i3,' ios = ',i4)
      return
c-----
      entry fcard(in,k,l,ic,kd,iflag)
      if(in-5) 3,4,3
    4 lrec=80
      iflag=0
      return
    3 lrec=0
      iflag=0
      return
c-----
      entry wcard(in,k,l,ic,kd,iflag)
      if(in-5) 7,8,7
    8 write(6,1002) ic(1:lrec)
 1002 format(' ',a)
      return
    7 write(6,1004)lrec,kseq,ic(1:lrec)
 1004 format(' ',i3,i6,1x,a)
      return
      end
