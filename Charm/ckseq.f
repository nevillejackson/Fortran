      subroutine ckseq (order,com,coml,ik,i,it)
c-----sequence check of primary / secondary file
      character com*100,coml*100
      logical order
c-----check ascending order .. current field/previous field 
      if ((order).and.com.lt.coml) then
         write (lmess,10) ik,it
   10    format ('*** fatal *** record',i6,' on unit',i3,
     +' out of sequence') 
         call jobend
c-----check descending order
      else if ((.not.order).and.com.gt.coml) then
         write (lmess,10) ik,it
         call jobend
      else if (com.eq.coml.and.i.eq.1) then
c-----duplicate on primary .. abort
         write (lmess,20) ik
   20    format (' ***fatal*** duplicate on primary, record no.',i5) 
         call jobend
      end if
      return
      end
