      subroutine chdp (len,par,id,tray)
c-----gives 'Dw.d' format for internal file 
c-----converts type character to double precision using this format
      character par*10,fmtcr*7
      double precision tray
      common /cz/ fmtcr
      common /stdio/ lin,lout,lmess
      data fmtcr/1*' '/
      write (fmtcr,10) len,id
   10 format ('(D',i2,'.',i1,')')
      read (par,fmtcr,err=98,iostat=ios)tray
      return
   98 write (lmess,20) fmtcr,par,ios
   20 format(' format ',a7,' for c-dp conv of ',a10,' iostat',i6)
      call jobend
      return
      end
