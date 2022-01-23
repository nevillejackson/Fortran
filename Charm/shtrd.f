      subroutine shtrd (nc,ic,ihb,idb,prefix,ipw,ipl,com,para,
     +trig,limp)
c------Reads sheet directives, mandatory ones then optional ones.
c------Maximum directive length set at 130 characters
      dimension manc(5),ipar(5,10),ic(10),com(100),para(100,3)
      character string*130,del*1,para*130,com*10,manc*10,par*130,
     +bpar*130
      logical trig,prefix,nonc,last,limp
      common /stdio/ lin,lout,lmess
      common /slim/ limcom,limpar,limpfx,limpge,limpgm
      common /ctab/ ibegd,iendd,ilen,ibegp,ilenp,iendp,ibeg2
      common /cint/ knt,icom(100,10),ncom(100),lenp(100,3)
      common /cx/ manc
      save
      data manc/5*' '/
      data par,bpar/2*' '/
      prefix = .false.
      trig = .false.
      ii = 0
      do 7 l = 1,130
    7 par(l:l) = ' '
      bpar = par
c------mandatory commands = 4 : if page size not default = 5.
      do 3 i = 1,5
      read (lin,10,end=99,err=98,iostat=ios) string
   10 format (a)
      write (lout,20) string
   20 format (' ',a)
      nonc = .false.
      last = .false.
      ipos = 1
      del = ','
      j = 0
      call nextsh (ipos,del,string,manc(i),len,nonc,last)
    1 j = j+1
      if (j.gt.limpar) then
      write (lmess,70) limpar
   70 format (' limit of ',i3,' parameters/directive')
      call jobend
      end if
      nonc = .true.
      call nextsh (ipos,del,string,par,len,nonc,last)
      if (nonc) then
      call chint (len,par,ipar(i,j))
      else if (i.eq.4.and.j.eq.3) then
      if (par(1:1).ne.'r') goto 92
      prefix = .true.
      del = ','
      else if (i.eq.2.and.j.eq.3) then
      if (par(1:1).ne.'m') goto 92
      limp = .false.
      end if
      if (.not.last) goto 1
c------interpret mandatory commands 'manc'
      if (i.eq.1) then
      if (manc(i).ne.'*col') goto 90
c------no.cols. = nc ; col.tabs = ic(x) ; check 'nc' = no.col
c------tabs entered.
      nc = ipar(i,1)
      if (nc.ne.(j-1))then
      write (lmess,71) nc
   71 format (' *col requires no. col. tabs = nc .. given here as ',i3)
      call jobend
      end if
      k = 0
      do 2 l = 2,j
      k = k+1
    2 ic(k) = ipar(i,l)
      else if (i.eq.2) then
      if (manc(i).ne.'*row') goto 90
c------no. of blank lines after heading = ihb
c------ "   "   "     "   between data lines = idb
      ihb = ipar(i,1)
      idb = ipar(i,2)+1
      else if (i.eq.3) then
      if (manc(i).ne.'*data') goto 90
c------beginning of field = ibegd ; end of field =iendd
      ibegd = ipar(i,1)
      iendd = ipar(i,1)+ipar(i,2)-1
      ilen = ipar(i,2)
      else if (i.eq.4) then
      if (manc(i).ne.'*pfx') goto 90
c------start of prefix = ibegp ;end = iendp ;next char. = ibeg2
      ibegp = ipar(i,1)
      ilenp = ipar(i,2)
      if (ilenp.gt.limpfx) goto 94
      iendp = ibegp+ilenp-1
      ibeg2 = iendp+1
      else if (manc(i).eq.'*set') then
c------not default page. width = ipw ; length = ipl
      ipw = ipar(i,1)
      ipl = ipar(i,2)
      if (ipw.gt.130.or.ipl.gt.62) goto 94
      else
c------default settings (80,60)
      ipw = 80
      ipl = 60
      ii = 1
      goto 5
      end if
    3 continue
c------any further commands are optional
    4 read (lin,10,end=99,err=98,iostat=ios) string
      write (lout,20) string
      ii = ii+1
      if (ii.gt.limcom) goto 94
      com(ii) = '          '
    5 nonc = .false.
      last = .false.
      par = bpar
      ipos = 1
      j = 0
      del = ','
      call nextsh (ipos,del,string,com(ii),len,nonc,last)
      if (com(ii)(1:1).ne.'*') call jobend
      if (com(ii)(2:2).eq.'t') trig = .true.
      if (com(ii).eq.'*equiv') del = '='
      if (com(ii).eq.'*set') goto 90
    6 j = j+1
      if (j.gt.limpar) call jobend
      nonc = .true.
      call nextsh (ipos,del,string,par,len,nonc,last)
      if (nonc) then
      call chint (len,par,icom(ii,j))
      else
      lenp(ii,j) = len
      para(ii,j) = par
      if (j.gt.3) call jobend
      end if
      if (.not.last) goto 6
      ncom(ii) = j
      goto 4
   98 call errr (lin,ii,ios)
   99 knt = ii
      if (knt.eq.0) then
      ipw = 80
      ipl = 60 
      end if
      return
   90 write (lmess,91)
   91 format (' First 4 directives must be:- *col/*row/*data/*pfx
     +optional 5th directive:- *set')
      call jobend
      return
   92 write (lmess,93)
   93 format (' *row with m enclosed in single quotes (page limit 100)
     + *pfx with r ,enclosed in single quotes, (each record has prefix)
     +')
      call jobend
      return
   94 write (lmess,95)
   95 format (' Limit exceeded .. check .. see "manl" writeup')
      call jobend
      return
      end
