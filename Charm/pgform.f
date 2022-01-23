      subroutine pgform (bline,page,com,para,ic,nc,lk,ltp,ltc,i,ipl)
c------sets whole page to blank then writes in fixed page and
c------column headings. leaves lines for triggered headings to
c------be inserted in correct position (ltp,ltc respectively)
      dimension page(100,62),com(100),ic(10),para(100,3)
      character bline*130,page*130,com*10,para*130
      common /cint/ knt,icom(100,10),ncom(100),lenp(100,3)
      common /slim/ limc,lpar,limx,limo,limm
      save
      ltp = 0
      ltc = 0
      i = 1
      do 1 j = 1,ipl
    1 page(i,j) = bline
c------first line of page blank, second reserved for incremental
c------and total count.
      lk = 3
      kph = 0
      if (knt.eq.0) return
      do 2 k = 1,knt
      if (com(k).eq.'*fph') call hdpage (k,icom,para,lenp,lk,
     +page,i,kph)
    2 continue
      do 3 k = 1,knt
      if (com(k).eq.'*tph') then
      ltp = lk+icom(k,3)
      if (ltp.ge.lk) then
      lk = ltp+1
c------when nbl directive is -ve ltp<lk so lk not incremented
      end if
      goto 4
      end if
    3 continue
    4 kch = 0
      do 5 k = 1,knt
    5 if (com(k).eq.'*fch') call hdcol (k,icom,para,lenp,ic,nc,
     +lk,page,i,kch)
      do 6 k = 1,knt
      if (com(k).eq.'*tch') then
      ltc = lk+icom(k,3)
      if (ltc.ge.lk) then
      lk = ltc+1
c------when nbl -ve ltc<lk so lk not incremented
      end if
      return
      end if
    6 continue
      return
      end
