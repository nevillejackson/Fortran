      subroutine thead (page,com,para,ic,nc,lk,ltc2,i,ltc3)
c------sets up fch and line for tch following tph with lines
      dimension page(100,62),com(100),para(100,3),ic(10)
      character page*130,com*10,para*130
      common /cint/ knt,icom(100,10),ncom(100),lenp(100,3)
      common /slim/ limc,lpar,limx,limo,limm
      do 1 k = 1,knt
    1 if (com(k).eq.'*fch') call hdcol (k,icom,para,lenp,ic,nc,
     +lk,page,i,kch)
      do 2 k = 1,knt
      if (com(k).eq.'*tch') then
      ltc2 = lk+icom(k,3)
      if (ltc2.gt.lk) then
      lk = ltc2+1
      end if
      end if
    2 continue
c------sets line for col(s) to start on when only *tcc
      do 3 k = 1,knt
      if (com(k).eq.'*tcc'.and.ltc2.eq.0) then
      ltc3 = lk
      return
      end if
    3 continue
      return
      end
