      subroutine binchr(ix,iy,n,lpos) 
c     locates ix in list iy leaving position value in lpos
      character*10 ix,iy(100) 
      lpos = 0
      if(ix.eq.'          ') return 
      if(ix.eq.iy(n)) then
      lpos=n
      return
      else
    9 nzt=0 
      nix=0 
      niy=n 
    8 nz=(nix+niy)/2
      if(nzt.eq.nz) then
      ix = '          ' 
      lpos = 0
      return
      endif
      if(nz.eq.0) then
      ix = '          ' 
      lpos = 0
      return
      endif 
      nzt=nz
      if(ix.eq.iy(nz)) then 
      lpos=nz 
      return
      else
      if(ix.lt.iy(nz)) then
      niy=nz
      go to 8 
      else
      nix=nz
      go to 8 
      endif
      endif
      endif
      end 
