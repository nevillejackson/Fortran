      subroutine ctrig (n,com,para,newcol,newch,new,par,
     +pch,kk,bline,chead,lenc,k,itch,lk,lkc,lit)
c-----called by `tset` to determine content and position (? lines) of   
c-----chead when (1) new or (2) *tch field content has changed.
      dimension com(100),icom(100,10),para(100,3),lenp(100,3)
      dimension ncom(100)
      character com*10,para*130,par*80,pch*80,bline*130,chead*80
      character lit*1000
      logical new,newcol,newch
      common /stdio/ lin,lout,lmess
      common /cint/ knt,icom,ncom,lenp
      if (newcol) then
      write (lmess,10)
   10 format (' ***fatal*** only one "*tch" allowed/run')
      call jobend
      end if
      pch = par
      newcol = .true.
      if (lk.gt.lkc) lkc = lk
      if (ncom(n).ge.4.and.(.not.new)) then
c------lines parameter, check if new col necessary
c------make allowance for any extra blanks required (icom(n,5))
      if (kk.ge.(icom(n,4)+icom(n,5))) then
      if (k.eq.1) then
      itch = lk+icom(n,3)+icom(n,5)
      newch = .true.
      else if (newch) then
      il = lk+icom(n,3)+icom(n,5)
      if (il.gt.itch) itch = il
      end if
      else
      newch = .false.
      end if
      end if
    1 chead = bline
      nn = n+1
      if (nn.gt.knt) goto 3
c------check for *equiv(s) following immediately after *tch
      do 2 i = nn,knt
      if (com(i).ne.'*equiv') goto 3
      if (ncom(i).eq.1) then
      chead(1:lenp(i,1)) = para(i,1)
      return
      else if (para(i,1).eq.pch) then
      if (ncom(i).eq.3) then
      it = icom(i,2)+icom(i,3)-1
      chead(1:icom(i,3)) = lit(icom(i,2):it)
      return
      else
      chead(1:lenp(i,2)) = para(i,2)
      return
      end if
      end if
    2 continue
c------no *equiv(s) or none matching, default heading is content
c------of trigger field
    3 chead(1:icom(n,2)) = pch
      return
      end
