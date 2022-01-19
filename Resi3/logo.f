      subroutine logo
      common /stdio/ lc,lp,lerr
      common /limits/ mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
      character*80 a(26)
      data a(1)/'                                                       
     +   #'/
      data a(2)/'                                                       
     +  # #'/
      data a(3)/'                                                       
     + #   #'/
      data a(4)/'                                                       
     +#     #'/
      data a(5)/'                                                      #
     +##   ###'/
      data a(6)/'                                                      #
     +       #'/
      data a(7)/'                                                     # 
     +        #'/
      data a(8)/'                                                    ###
     +       ###'/
      data a(9)/'                                                    #  
     +         #'/
      dataa(10)/'                                                   #   
     +          #'/
      dataa(11)/'          ##################                       #    
     +          #'/
      dataa(12)/'         ####################                     ###  
     +         ###'/
      dataa(13)/'        ####     ####     ####                     #   
     +          #'/
      dataa(14)/'       ###    ## #### ##    ###                    #   
     +          #'/
      dataa(15)/'      ###     ## #### ##     ###                  #    
     +           #'/
      dataa(16)/'      ###        ####        ###                  #    
     +           #'/
      dataa(17)/'      ###   ##   ####   ##   ###                 #     
     +            #'/
      dataa(18)/'      ###   ###  ####  ###   ###                 #     
     +            #'/
      dataa(19)/'      ###    ### #### ###    ###                #######
     +##   #########'/
      dataa(20)/'       ###   ### #### ###   ###                        
     + #   #'/
      dataa(21)/'        ######## #### ########                         
     + #   #'/
      dataa(22)/'         ######  ####  ######                          
     + #   #'/
      dataa(23)/'          ####   ####   ####                           
     + #   #'/
      dataa(24)/'                 ####                                  
     + #   #'/
      dataa(25)/'                 ####                                  
     + #   #'/
      dataa(26)/'                  ##                                   
     + #####'/
      write(lp,201)
  201 format(//30x,' RESI VERSION 3.1'/
     + /' CSIRO PROGRAM FOR MULTI_TRAIT COMBINED '
     +  'SELECTION INDEX WITH RESTRICTION'/)
      write(lp,202)
  202 format(/'     N. JACKSON',29X,
     +'C.A.DEAN & P.P.COTTERILL'/
     +'     CSIRO',34X,'CSIRO',/
     +'     DIVISION OF ANIMAL PRODUCTION',10X,
     +'DIVISION OF FOREST RESEARCH'/
     +'     P.O. BOX 239 BLACKTOWN',17X,'CUNNINGHAM LABORATORY'/
     +'     N.S.W.,2148',28X,'306 CARMODY ROAD'/
     +'     AUSTRALIA',30X,'ST.LUCIA,QLD.,4067'/
     +44X,'AUSTRALIA'//)
      write(lp,203)a(1)
  203 format(a80)
      write(lp,203)a(1)
      write(lp,203)a(2)
      write(lp,203)a(2)
      write(lp,203)a(3)
      write(lp,203)a(3)
      write(lp,203)a(4)
      write(lp,203)a(4)
      write(lp,203)a(5)
      write(lp,203)a(4)
      write(lp,203)a(4)
      write(lp,203)a(6)
      write(lp,203)a(6)
      write(lp,203)a(7)
      write(lp,203)a(7)
      write(lp,203)a(8)
      write(lp,203)a(7)
      write(lp,203)a(7)
      write(lp,203)a(9)
      write(lp,203)a(9)
      do 204 i=10,26
  204 write(lp,203)a(i)
      return
      end
