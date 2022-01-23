      subroutine cntrd (cnam,lenc,icfb,icfl,os,tnam,lent,itb,itl,
     +itd,pnam,lenp,ipb,ipl,ipd,kcc,ksd,kld,list,listf,sform,irl2, 
     +csf)
c-----reads directives for count 
      dimension cnam(10),lenc(10),icfb(10),icfl(10),os(10),
     +tnam(50),lent(50),itb(50),itl(50),itd(50),pnam(10), 
     +lenp(10),ipb(10),ipl(10),ipd(10)
      character cnam*10,os*1,tnam*10,pnam*10,string*80,par*10
      character del*1,sform*80
      logical nonc,list,listf,csf 
      common /stdio/ lin,lout,lmess
      common /tio/ lti,lto,lto2
      common /limits/ lc,ll,lp,lr,nv
      data par/1*' '/
      knt = 0
      k = 0
      j = 0
      i = 0
      list = .false.
      listf = .false.
      csf = .false. 
    1 read (lin,10,end=99,err=98,iostat=ios) string
   10 format (a)
      knt = knt+1
c-----echo directive to screen 
      write (lmess,20) string
   20 format (' ',a)
      nonc = .false.
      ipos = 4
      del = ','
      if (string(1:3).eq.'*c,') then
c-----control field change directive 
      i = i+1
      if (i.gt.nv) goto 96
      call nextct (ipos,del,string,cnam(i),lenc(i),nonc)
      if (lenc(i).gt.lp) call exceed (3,lenc(i))
      nonc = .true.
      call nextct (ipos,del,string,par,len,nonc)
      call chint (len,par,icfb(i))
      call nextct (ipos,del,string,par,len,nonc)
      call chint (len,par,icfl(i))
      if (icfl(i).gt.lp) call exceed (3,icfl(i))
      jpos = ipos+1
      os(i) = string(ipos:ipos)
      if (os(i).ne.'a'.and.os(i).ne.'d') call jobend
      else if (string(1:3).eq.'*s,') then
c-----summing directive
      j = j+1
      if (j.gt.50) goto 96
      call nextct (ipos,del,string,tnam(j),lent(j),nonc)
      if (lent(j).gt.lp) call exceed (3,lent(j))
      nonc = .true.
      call nextct (ipos,del,string,par,len,nonc)
      call chint (len,par,itb(j))
      call nextct (ipos,del,string,par,len,nonc)
      call chint (len,par,itl(j))
      if (itl(j).gt.lp) call exceed (3,itl(j))
      del = ' '
      call nextct (ipos,del,string,par,len,nonc)
      call chint (len,par,itd(j))
      if (itd(j).gt.lp) call exceed (3,itd(j))
      else if (string(1:3).eq.'*l,') then
c-----listing directive for specified fields only
      if (list) call jobend
      listf = .true.
      k = k+1
      call nextct (ipos,del,string,pnam(k),lenp(k),nonc)
      if (lenp(k).gt.lp) call exceed (3,lenp(k))
      nonc = .false.
      call nextct (ipos,del,string,par,len,nonc)
      call chint (len,par,ipb(k))
      call nextct (ipos,del,string,par,len,nonc)
      call chint (len,par,ipl(k))
      del = ' '
      call nextct (ipos,del,string,par,len,nonc)
      call chint (len,par,ipd(k))
      else if (string(1:3).eq.'*l ') then
c-----list directive for whole file to be listed 
      if (listf) call jobend
      list = .true.
      else if (string(1:4).eq.'*sf,') then
c-----count summary file format
      csf = .true.
      ipos = 5
      nonc = .true.
      call nextct (ipos,del,string,par,len,nonc) 
      if (len.gt.3) call jobend
      call chint (len,par,irl2)
      del = ' ' 
      nonc = .false.
      call nextct (ipos,del,string,sform,len,nonc)
      else
c-----unrecognised count directive 
      write (lmess,30)
   30 format (' *** fatal *** unrecognised count directive')
      call jobend
      end if
      goto 1
   96 write (lmess,40) nv
   40 format(' ***fatal***limit for *c and *l =',i3,' others 50')
      call jobend
      return
   98 call errr (lin,knt,ios)
      return
   99 kcc = i
      ksd = j
      kld = k
      return
      end
