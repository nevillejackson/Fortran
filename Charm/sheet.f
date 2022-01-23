      program sheet 
c-----ftn5 program written june '85 by anne swinton.
c-----latest update 22/7/85.
c-----UNIX version 10/9/85   latest update 13/10/86
c-----subsht.f  latest update 9/1/87
c-----comment format 120 added 22/5/89
c-----Pre-sorted input unit10, one continuous data field includ-
c-----ing any blanks or special characters required on output.
c-----Single quotes .. ' .. NOT .. $ used in UNIX to enclose character
c-----strings such as headings and optional 'm' (*row) and 'r' (*pfx)
c-----Each page of output labelled with incremental and total page
c-----number .. page count re-zeroed by a *tph.
c-----Output written to unit11, count refers to data items only
c-----headings,blank lines etc. not included.
c-----The first 4 directives are mandatory commands and must be
c-----in sequence, the 5th directive need only be included if a
c-----page setting other than default is required.
c-----
c-----limits      10 ... *fph,*fch               (limcom /slim/)
c-----            75 ... *equiv                      "     "
c-----             1 ... *tph,*tch,*tbl,*tpc,*tcc    "     "
c-----            20 ... prefix length  (limpfx /slim/)
c-----            10 ... parameters/directive  (limpar /slim/)
c-----           130 ... page width (char)  default setting = 80
c-----            62 ... page length (lines)   "       "    = 60
c-----            20 ... pages/block unless 'm' set in *row directive
c-----                                                (limpge/slim/)
c-----           100 ... pages/block .. if 'm' set in *row (limpgm/slim/)
c-----           130 ... directive length
c-----
      dimension page(100,62),com(100),para(100,3),ic(10)
     +,icom(100,10),lenp(100,3),ncom(100)
      character page*130,lit*1000,litf*130,litb*130,para*130,
     +head*130,chead*80,com*10,cpf*20
      character bline*130
      logical new,prefix,trig,rdend,same,dat,repeat,write,limp
      logical newpage,newhead,newcol,newch,npage,ncol,nline
      common /ca/ page
      common /cb/ lit
      common /cc/ litf
      common /cd/ litb
      common /ce/ para
      common /cf/ head
      common /cg/ chead
      common /ch/ com
      common /ci/ cpf
      common /ck/ bline
      common /cint/ knt,icom,ncom,lenp
      common /ctab/ ibegd,iendd,ilen,ibegp,ilenp,iendp,ibeg2
      common /slim/ limcom,limpar,limpfx,limpge,limpgm
      common /stdio/ lin,lout,lmess
      common /tio/ lti,lto
c     data page/6200*' '/
      data litf,litb/2*' '/
      data para,com,cpf/300*' ',100*' ',1*' '/
      data head,chead/2*' '/
      data knt,icom,ncom,lenp/1,1000*1,100*1,300*1/
      data ibegd,iendd,ilen,ibegp,ilenp,iendp,ibeg2/7*1/
      data ltp,ltp2,ltc,ltc2,ltc3,itch/6*0/
      data limcom,limpar,limpfx,limpge,limpgm/100,10,20,20,100/
      data lti,lto/10,11/
      do 368 i=1,100
      do 368 j = 1,62
  368 page(i,j) = ' '
      repeat = .false.
      dat = .false.
      newpage = .false.
      newhead = .false.
      newcol = .false.
      newch = .false.
      npage = .false.
      ncol = .false.
      nline = .false.
      limp = .true.
      new = .true.
      write (lout,100)
  100 format ((/),' sheet run')
      call argopn (lti,1,'old')
      call argopn (lto,2,'new')
c-----read all directives for sheet
      call shtrd (nc,ic,ihb,idb,prefix,ipw,ipl,com,para,
     +trig,limp)
c-----set line of maximum allowable length to blank
      do 110 i = 1,130
  110 bline(i:i) = ' '
c-----check data length fits col width, and cols/width fit
      if (nc.gt.1) then
      if ((ic(2)-ic(1)).lt.ilen) then
      write (lmess,120)
  120 format (' *** insufficient room for data given column spacing')
      call jobend
      end if
      end if
c-----set up page with fixed headings and spaces for triggered
c-----headings to be added later. remainder of page set to blank
      call pgform (bline,page,com,para,ic,nc,lk,ltp,ltc,i,ipl)
c-----add 'ihb' blank lines following final heading.
      ilk = lk+ihb
c-----check prefix position in data field and in "litf"
      if (ibegp.eq.ibegd) then
         same = .true.
         ipb = 1
         ipe = ilenp
      else
         same = .false.
         ipb = (ibegp-ibegd)+1
         ipe = (ipb+ilenp)-1
      end if
c-----set litb of length 'ilen' to blank, equate to litf.
      do 130 j = 1,ilen
  130 litb(j:j) = ' '
c-----set record count to zero
      irk = 0
      icnt = 0
      goto 12
c-----new page count following *tph
    3 new = .true.
      i = 1
      goto 5
c-----increment page count following *tpc or full page etc.
    4 ip = i
      i = i+1
      if (i.gt.limpge.and.limp) goto 30
      if (i.gt.limpgm) goto 30
c-----write fixed headings to page, overwrite for *tph
c-----blank out then add heading for *tch (in do loop 13)
    5 continue
      do 6 j = 1,ilk
    6 page(i,j) = page(ip,j)
      if (ltp) 8,8,7
    7 page(i,ltp) = head
    8 if (ltc) 10,10,9
    9 page(i,ltc) = bline
c-----set rest of page to blank
   10 continue
      do 11 j = ilk,62
   11 page(i,j) = bline
   12 continue
      ltp2 = 0
      ltc3 = 0
      ltc2 = 0
      lkc = 0
   26 itch = 0
      newch = .false.
c-----increment column count following full col./*tch/*tcc etc.
      do 13 k = 1,nc
         ibc = ic(k)
         iec = ibc+ilen-1
         lk = ilk
         if ((new).and.(.not.dat)) goto 16
c-----check for further cols beginning part-way down page (lines)
         if (ltc2) 28,28,27
   27    page(i,ltc2)(ibc:iec) = chead
         lk = ltc2+1+ihb
         goto 15
c-----check for further cols beginning part-way down page (lines)
   28    if (ltc3) 29,29,21
   21    lk = ltc3+ihb
         goto 15
   29    if (ltc) 15,15,14
   14    page(i,ltc)(ibc:iec) = chead
   15    if (dat) then
            litf(ipb:ipe) = cpf
            goto 23
         end if
c-----read input file record by record, doesn't call eofr at eoi
   16    call readch (irl,iseq,lti,lit,rdend,irk)
         if (rdend) goto 24
         litf = litb
c-----determine content of data field 'litf'
         if (new.or.prefix.or.repeat) then
            cpf = lit(ibegp:iendp)
            litf = lit(ibegd:iendd)
         else
            call detlit (lit,litf,cpf,same)
         end if
         dat = .true.
c-----check for trigger fields
         if (trig) then
            call tset (com,para,ipl,lk,ilk,bline,head,chead,
     +new,newpage,newhead,newcol,newch,lit,npage,ncol,nline,
     +nbl,lenc,k,itch,ltp2,lkc)
         else
            goto 23
         end if
         if (new) then
            if (ltp) 18,18,17
   17       page(i,ltp) = head
   18       if (ltc) 23,23,19
   19       page(i,ltc)(ibc:iec) = chead
         else if (newpage) then
            goto 24
         else if (newhead) then
            page(i,ltp2) = head
            lk = ltp2+1
            call thead (page,com,para,ic,nc,lk,ltc2,i,ltc3)
            goto 26
         else if (npage) then
            goto 4
         else if (newcol.or.ncol) then
            goto 13
         else if (nline) then
            if (lk.eq.ilk) goto 23
            lk = lk-idb+nbl+1
            if (lk.gt.ipl) goto 13
         end if
c-----add data 'litf' to sheet
   23    page(i,lk)(ibc:iec) = litf(:ilen)
         write = .false.
         dat = .false.
         new = .false.
         lk = lk+idb
         repeat = .false.
         if (lk.le.ipl) goto 16
         repeat = .true.
   13 continue
      if (newch) then
         ltc2 = itch
         goto 26
      end if
      goto 4
c-----check page count
   24 if (write) goto 31
      if (k.eq.1.and.lk.eq.ilk) goto 30
      ip = i
   30 call nopage (ip,page,ipw,lto,irk,icnt,rdend,limp)
      write = .true.
      if (.not.rdend) goto 3
   31 call eofr (lti,irk)
      call eofw (lto,irk)
      stop
      end
