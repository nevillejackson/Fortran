c                                                                       
c     ..................................................................
c                                                                       
c        function iloc                                                 
c                                                                       
c        purpose                                                        
c           compute a vector subscript for an element in a matrix of    
c           specified storage mode                                      
c                                                                       
c        usage                                                          
c           iloc (i,j,n,m,ms)                                           
c                                                                       
c        description of parameters                                      
c           i   - row number of element                                 
c           j   - column number  of element                             
c           n   - number of rows in matrix                              
c           m   - number of columns in matrix                           
c           ms  - one digit number for storage mode of matrix           
c                  0 - general                                          
c                  1 - symmetric                                        
c                  2 - diagonal                                         
c                                                                       
c        remarks                                                        
c           none                                                        
c                                                                       
c        subroutines and function subprograms required                  
c           none                                                        
c                                                                       
c        method                                                         
c           ms=0   subscript is computed for a matrix with n*m elements 
c                  in storage (general matrix)                          
c           ms=1   subscript is computed for a matrix with n*(n+1)/2 in 
c                  storage (upper triangle of symmetric matrix). if     
c                  element is in lower triangular portion, subscript is 
c                  corresponding element in upper triangle.             
c           ms=2   subscript is computed for a matrix with n elements   
c                  in storage (diagonal elements of diagonal matrix).   
c                  if element is not on diagonal (and therefore not in  
c                  storage), ir is set to zero.                         
c                                                                       
c     ..................................................................
c                                                                       
      function iloc(i,j,n,m,ms)                                         
c                                                                       
      ix=i                                                              
      jx=j                                                              
      if(ms-1) 10,20,30                                                 
   10 irx=n*(jx-1)+ix                                                   
      go to 36                                                          
   20 if(ix-jx) 22,24,24                                                
   22 irx=ix+(jx*jx-jx)/2                                               
      go to 36                                                          
   24 irx=jx+(ix*ix-ix)/2                                               
      go to 36                                                          
   30 irx=0                                                             
      if(ix-jx) 36,32,36                                                
   32 irx=ix                                                            
   36 iloc=irx                                                          
      return                                                            
      end                                                               
