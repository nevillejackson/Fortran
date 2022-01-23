      subroutine hdcol (j,icom,para,lenp,ic,nc,lk,page,i,kch)
c------form  ...  *fch,nbl,$heading$
c------leaves "nbl" blank lines, repeats heading "nc" times
c------beginning at each column tab "ic" across the page
      dimension icom(100,10),para(100,3),lenp(100,3),page(100,62)
      dimension ic(10)
      character para*130,page*130
      common /stdio/ lin,lout,lmess
      common /slim/ limc,lpar,limx,limo,limm
      lk = lk+icom(j,1)
      n = lenp(j,2)
      do 1 l = 1,nc
      m = ic(l)
      mm = m+n-1
    1 page(i,lk)(m:mm) = para(j,2)(:n)
c------check no. of *fch directives
      kch = kch+1
      if (kch.gt.lpar) then
      write (lmess,10) lpar
   10 format (' ***fatal*** Maximum of',i3,' *fch directives allowed')
      call jobend
      end if
c------increment line count
      lk = lk+1
      return
      end
