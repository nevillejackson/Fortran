c                                                                               loc      2
c     ..................................................................        loc      3
c                                                                               loc      4
c        function iloc                                                 
c                                                                               loc      6
c        purpose                                                                loc      7
c           compute a vector subscript for an element in a matrix of            loc      8
c           specified storage mode                                              loc      9
c                                                                               loc     10
c        usage                                                                  loc     11
c           iloc (i,j,n,m,ms)                                           
c                                                                               loc     13
c        description of parameters                                              loc     14
c           i   - row number of element                                         loc     15
c           j   - column number  of element                                     loc     16
c           n   - number of rows in matrix                                      loc     18
c           m   - number of columns in matrix                                   loc     19
c           ms  - one digit number for storage mode of matrix                   loc     20
c                  0 - general                                                  loc     21
c                  1 - symmetric                                                loc     22
c                  2 - diagonal                                                 loc     23
c                                                                               loc     24
c        remarks                                                                loc     25
c           none                                                                loc     26
c                                                                               loc     27
c        subroutines and function subprograms required                          loc     28
c           none                                                                loc     29
c                                                                               loc     30
c        method                                                                 loc     31
c           ms=0   subscript is computed for a matrix with n*m elements         loc     32
c                  in storage (general matrix)                                  loc     33
c           ms=1   subscript is computed for a matrix with n*(n+1)/2 in         loc     34
c                  storage (upper triangle of symmetric matrix). if             loc     35
c                  element is in lower triangular portion, subscript is         loc     36
c                  corresponding element in upper triangle.                     loc     37
c           ms=2   subscript is computed for a matrix with n elements           loc     38
c                  in storage (diagonal elements of diagonal matrix).           loc     39
c                  if element is not on diagonal (and therefore not in          loc     40
c                  storage), ir is set to zero.                                 loc     41
c           ms=-ms subscript is computed for a matrix with n*(n+1)/2
c                  elements in storage (upper triangle of symmetric
c                  matrix) in rowwise order for rows of length ms.
c                                                                               loc     42
c     ..................................................................        loc     43
c                                                                               loc     44
      function iloc(i,j,n,m,ms)                                         
c                                                                               loc     46
      ix=i                                                                      loc     47
      jx=j                                                                      loc     48
      if (ms.eq.0) then
c       general
   10   iloc=n*(jx-1)+ix                                                           loc     50
      else if (ms.eq.1) then
c       symmetrric ssp order - upper triangle colwise
   20   if(ix-jx) 22,24,24                                                        loc     52
   22   iloc=ix+(jx*jx-jx)/2                                                       loc     53
        go to 36
   24   iloc=jx+(ix*ix-ix)/2                                                       loc     55
   36   continue
      else if (ms.eq.3) then
c       diagonal
        iloc=0                                                                     loc     57
        if(ix.eq.jx) then
          iloc=ix                                                                    loc     59
        endif
      else if(ms.lt.o) then
c       symmetric harvey order - upper triangle rowwise
        k  = -ms*(ix-1)-ix*(ix-3)/2
        k1 = -ms*(jx-1)-jx*(jx-3)/2
        if (jx.ge.ix) then
          iloc = k+jx-ix
        else
          iloc = k1+ix-jx
        endif
      endif
      return                                                                    loc     61
      end                                                                       loc     62
