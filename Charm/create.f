      program create 
c------f77 program written 01/85 by anne swinton                               
c------latest update 30/8/85                                                    
c------NOS version 31/7/85 UNIX version 4/9/85.
c------converts a card image file (tape10) to a "charm" file (tape11)           
c------tapes rewound at beginning and end of program.                           
c------irl = record length = rl
      character rl*3
      character *1000 rec                                                       
      common /stdio/ lin,lout,lmess                                             
      common /zrec/ rec                                                         
      common /limits/ lc,ll,lp,lr,nv                                            
      data rec/1*' '/
c-----
      write (lmess,20)                                                          
   20 format ((/),' create run')                                                
c-----get filename arguments and open
      call argopn(10,1,'old')
      call argopn(11,2,'new')
c------read record length directive from input (a3)                            
      call getarg(3,rl)
      write (lmess,40) rl                                                       
   40 format (' ',a3)                                                           
c------convert "rl" to integer "irl", check irl within limits                   
      read(rl,'(i3)')irl                                                        
      if (irl.gt.lr) call exceed (4,irl)                                        
      write (lmess,70) irl                                                   
   70 format(' record length read = ',i3,' characters')                
      write (lmess,80) irl                                                   
   80 format (' record length written = ',i3,' characters')        
c------k equals record count                                                    
      k = 0                                                                     
c------read records from tape10                                                 
    1 read(10,'(a)',end=99,err=98,iostat=ios)rec(1:irl)                         
      k = k+1                                                                   
c------write rl and seq.no. to beginning of record, write record to output file.             
      call recwr (irl,k,rec,11)                                           
      goto 1                                                                    
   98 call errr(10,k,ios)                                                       
   99 call eofr (10,k)
      call eofw (11,k)
      stop                                                                      
      end                                                                       
