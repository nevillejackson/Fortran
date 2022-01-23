      subroutine hdpage (j,icom,para,lenp,lk,page,i,kph)
c------form  ...  *fph,nbl,col,$heading$
c------leaves "nbl" blank lines begins heading in "col"
      dimension icom(100,10),para(100,3),lenp(100,3),page(100,62)
      character para*130,page*130
      common /stdio/ lin,lout,lmess
      common /slim/ limc,lpar,limx,limo,limm
      lk = lk+icom(j,1)
      l = icom(j,2)
      m = lenp(j,3)
      n = l+m-1
      page(i,lk)(l:n) = para(j,3)(:m)
c------check no. of *fph directives
      kph = kph+1
      if (kph.gt.lpar) then 
      write (lmess,10) lpar
   10 format(' ***fatal*** Maximum of',i3,' *fph directives allowed')
      call jobend
      end if
c------increment line count
      lk = lk+1
      return
      end
