      subroutine argopn (lun,fname,argno,stat)
c------get a filename argument from UNIX command line and open 
c------its logical unit (lun)
      common /stdio/ lin,lout,lmess
      integer argno,lun
      character stat*4,fname*(*)
      call getarg (argno,fname)
      open (lun,file=fname,status=stat,err=80,iostat=ios)
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
