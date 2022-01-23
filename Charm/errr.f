      subroutine errr(lun,k,ios)                                                
      common /stdio/ lin,lout,lmess                                             
      write (lmess,10) lun,k,ios                                                
   10 format ('oerror reading unit ',i3,' -- record count = ',i6,               
     +        ' iostat = ',i4)                                                  
      call jobend                                                               
      end                                                                       
