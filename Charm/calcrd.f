      subroutine calcrd (com,const,ans,ipar,npar,nam,ipa,ipb,ipc,
     +ipd,na,nb,nc,nd,usrsub)
c-----reads calc directives 
      dimension com(10),const(10),ans(10),ipar(10,6),npar(10),
     +ipa(100,3),ipb(100,3),ipc(100,2),ipd(100,2) 
      double precision const
      character string*80,par*10,com*1,ans*1,del*1
      logical nonc,last,usrsub
      common /stdio/ lin,lout,lmess
      common /limcal/ ifl,iam,isub
      data i,k,l,m,n,len/6*0/ 
      data par/1*' '/
      nam = 0
      knt = 0
      usrsub = .false.
    1 read (lin,10,end=99,err=98,iostat=ios) string
   10 format (a)
      write (lmess,20) string
   20 format (' ',a)
      nonc = .false.
      knt = knt+1
      del = ','
      ipos = 4
      if (string(1:4).eq.'*cm,'.or.string(1:4).eq.'*ca,') then
c-----constant add/multiply directive 
      j = 0
      ipos = 5
      i = i+1
      if (i.gt.iam) call jobend
      com(i) = string(3:3)
      call nextdp (ipos,del,string,const(i))
      nonc = .true.
    2 call nextc (ipos,del,string,par,len,last,nonc,j)
      j = j+1
      if (j.gt.6) call jobend
      if (nonc) then
      call chint (len,par,ipar(i,j))
      else
      ans(i) = par(1:len)
      nonc = .true.
      end if
      if (.not.last) goto 2
      npar(i) = j
      else if (string(1:3).eq.'*a,') then
      k = k+1
      if (k.gt.isub) call jobend
      call nextrf (ipos,del,string,par,len,nonc)
      nonc = .true.
      kk = 0
    3 kk = kk+1
      call nextc (ipos,del,string,par,len,last,nonc,kk)
      call chint (len,par,ipa(k,kk))
      if (.not.last) goto 3
      else if (string(1:3).eq.'*b,') then
      l = l+1
      if (l.gt.isub) call jobend
      call nextrf (ipos,del,string,par,len,nonc)
      nonc = .true.
      ll = 0
    4 ll = ll+1
      call nextc (ipos,del,string,par,len,last,nonc,ll)
      call chint (len,par,ipb(l,ll))
      if (.not.last) goto 4
      else if (string(1:3).eq.'*c,') then
      m = m+1
      if (m.gt.isub) call jobend
      call nextrf (ipos,del,string,par,len,nonc)
      nonc = .true.
      mm = 0
    5 mm = mm+1
      call nextc (ipos,del,string,par,len,last,nonc,mm)
      call chint (len,par,ipc(m,mm))
      if (.not.last) goto 5
      else if (string(1:3).eq.'*d,') then
      n = n+1
      if (n.gt.isub) call jobend
      call nextrf (ipos,del,string,par,len,nonc)
      nonc = .true.
      nn = 0
    6 nn = nn+1
      call nextc (ipos,del,string,par,len,last,nonc,nn)
      call chint (len,par,ipd(n,nn))
      if (.not.last) goto 6
      else
      write (lmess,30)
   30 format (' *** fatal *** unrecognised calc directive')
      call jobend
      end if
      goto 1
   98 call errr (lin,knt,ios)
      return
   99 if (knt.eq.0) then
      write (lmess,40)
   40 format (' *** fatal *** no calc directives')
      call jobend 
      end if
      nam = i 
      na = k
      nb = l
      nc = m
      nd = n
      in = na+nb+nc+nd
      if (in.ge.1) usrsub=.true.
      return
      end
