      subroutine recrd (idest,idesta,iorb,iorba,iore,iorea,icl,
     +icla,emf,emfa,lemf,lemfa,ibeg,ifl,ifend,nfield,rel,val,nval,
     +radt,rcdt,sdt,nad,ncd,nsd,nb,endrd)
      dimension idest(50),idesta(50),iorb(50),iorba(50),iore(50)
     +,iorea(50),icl(50),icla(50),emf(50),emfa(50),lemf(50),lemfa
     +(50),ibeg(50,5),ifl(50,5),ifend(50,5),rel(50,2),val(50,10)
     +,radt(50),rcdt(50),sdt(50),nfield(50),nval(50)
      character string*80,del2*1,dest*3,orb*3,fl*3,emf*10,
     +emfa*10,beg*4,rel*2,val*10,radt*1,rcdt*1,sdt*1
      common /limits/ lc,ll,lp,lr,nv
      common /stdio/ lin,lout,lmess
      common /rclim/ limr
      logical nonc,last,check,change,endrd
      endrd = .false.
      change = .false.
      ncd = 0
      nsd = 0
      i = 0
      nb = nb+1
c-----for block 2 on ... first *code directive string retained from
c-----the previous call to this subroutine.
      if (nb.gt.1) goto 7
    1 read (lin,10,end=99,err=98,iostat=ios)string
   10 format (a)
    7 check = .false.
      nonc = .true.
      ipos = 4
      del2 = ' '
      j = 1
      i = i+1
      rel(i,2) = '  '
      if(i.gt.limr)goto 97
c-----*all directives follow format of "refrd" from program "reform"
      if(string(1:5).eq.'*all,')then
      if(nb.ne.1)goto 97
      if (ncd.ne.0) goto 97
      ipos = 6
      nad = i
      call nextrf (ipos,',',string,dest,len,nonc)
      call chint (len,dest,idesta(i))
      if (string(ipos:ipos).eq.'''') then
      radt(i) = 'e'
      nonc = .false.
      ipos = ipos+1
      call nextrf (ipos,'''',string,emfa(i),lemfa(i),nonc)
      if (lemfa(i).gt.lp) then
      call exceed (3,lemfa(i))
      end if
                                    else
      radt(i) = 'c'
      call nextrf (ipos,',',string,orb,len,nonc)
      call chint (len,orb,iorba(i))
      call nextrf (ipos,' ',string,flc,len,nonc)
      call chint (len,flc,icla(i))
      iorea(i) = iorba(i)+icla(i)-1
                                    end if
c-----*code directives also follow "refrd" format.
      else if (string(1:6).eq.'*code,') then
      if (change) then
c-----code and select directives for block completed, current string
c-----is start of new block ... retain for next call of recrd.
      goto 96
      else if (ncd.eq.0) then
      i = 1
                    end if
      ipos = 7
      ncd = i
      call nextrf (ipos,',',string,dest,len,nonc)
      call chint (len,dest,idest(i))
      if (string(ipos:ipos).eq.'''') then
      rcdt(i) = 'e'
      nonc = .false.
      ipos = ipos+1
      call nextrf (ipos,'''',string,emf(i),lemf(i),nonc)
      if (lemf(i).gt.lp) then
      call exceed (3,lemf(i))
                         end if
                                    else
      rcdt(i) = 'c'
      call nextrf (ipos,',',string,orb,len,nonc)
      call chint (len,orb,iorb(i))
      call nextrf (ipos,' ',string,flc,len,nonc)
      call chint (len,flc,icl(i))
      iore(i) = iorb(i)+icl(i)-1
                                    end if
c-----select ... *i / *e directives follow format of "selrd".
      else if (string(1:3).eq.'*i,'.or.string(1:3).eq.'*e,') then
      last = .false.
      ipos = 4
      if(.not.change)then
      change = .true.
      i = 1
                     end if
      nsd = i
      sdt(i) = string(2:2)
c-----check for combined fields
      if (string(4:5).eq.'f(') then
      ipos = 6
      check = .true.
      del2 = ')'
      end if
      ifmax = 4
      nonc = .true.
    2 call nextrs (ipos,',',string,beg,len,check,del2,nonc
     +,ifmax,last)
      call chint (len,beg,ibeg(i,j))
      call nextrs (ipos,',',string,fl,len,check,del2,nonc
     +,ifmax,last)
      call chint (len,fl,ifl(i,j))
      ifend(i,j) = ibeg(i,j)+ifl(i,j)-1
c-----continue if more fields
      if (.not.check) goto 3
      j = j+1
      if (j.gt.5) goto 97
      goto 2
c-----read equivalence parameter "rel"
    3 nonc = .false.
      k = 1
      ifmax = 2
      call nextrs (ipos,',',string,rel(i,k),len,check,del2,nonc
     +,ifmax,last)
c-----determine field length of values to be equated
      ifmax = 0
      do 100 n = 1,j
      ifmax = ifmax+ifl(i,n)
  100 continue
      nfield(i) = j
      if (ifmax.gt.lp) then
      call exceed (3,ifmax)
      end if
c-----value parameter(s) ... set to blank for allowable length ... lp
    4 do 44 n = 1,lp
   44 val(i,k)(n:n) = ' '
      call nextrs (ipos,',',string,val(i,k),len,check,del2,nonc
     +,ifmax,last)
      if (len.ne.ifmax) goto 97
      if (last) goto 5
      k = k+1
c-----check for second equivalence, ie. range > <
      if (k.eq.2) then
      l = ipos+1
      if (string(ipos:l).eq.'lt') then
      rel(i,k) = string(ipos:l)
      ipos = ipos+3
      end if
      end if
      if(k.gt.nv) then
      call exceed (5,k)
      end if
      goto 4
c-----no more values directives
    5 nval(i) = k
c-----unrecognised directive ... abort program.
      else
      goto 97
      end if
c-----directive interpreted as valid ... echo to output
      write (lmess,20) string
   20 format (' ',a)
      goto 1
   96 return
   97 write (lmess,20) string
      write (lmess,40)
   40 format (' ***fatal*** invalid recode directive')
      call jobend
      return
   98 call errr(lin,i,ios)
      call jobend
   99 if(ncd.gt.0.and.(.not.change))then
      write (lmess,50)
   50 format(' ***fatal*** no select directives for "code" block')
      call jobend
      else
      endrd = .true.
      end if
      return
      end
