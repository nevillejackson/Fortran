      subroutine recwr (irl,iseq,rec,lto)                                    
c------writes record length, sequence number, record to output (lto)         
      character rec*1000
      write (lto,10) irl,iseq,rec(:irl)
   10 format (i3,i6,1x,a)
      return                                                                    
      end                                                                       
