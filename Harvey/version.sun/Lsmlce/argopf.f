      subroutine argopf (lun,fname,argno,stat,formt)
c------get a filename argument from UNIX command line and open 
c------its logical unit (lun)
      common /stdio/ lin,lout,lmess
      integer argno,lun
      character stat*4,fname*(*),formt*11
c-----read command line argument no argno
      call getarg (argno,fname)
c-----open file fname - if stat='new' and exists will overwrite
      call opn(lun,fname,stat,formt)
c------normal exit
      write (lmess,90) lun,stat,fname
   90 format (' opened to unit ',i3,'  status = ',a,'  file = ',a)
      rewind lun
      return
c------error exit
   80 write (lmess,95) lun,ios,fname
   95 format (' could not open unit ',i4,' iostat = ',i4,'file = ',a)
      call jobend
      return
      end
