.TH SHEET l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
SHEET - provides a means of producing directive-controlled, structured,
multi-paged output of sorted records or sub-records, with the capability
of inserting selected headings throughout the output. Specifically, to
provide a facility for generating structured list(s) from a `CHARM` file
for recording data in the field.
.SH SYNOPSIS
.nf
sheet infile outfile
.PP
*col,ncols,icol1,icol2,icol3,......icoln     mandatory (1st)  
*row,nbl,nsp[,'m']                           mandatory (2nd)
*data,beg,len                                mandatory (3rd)
*pfx,beg,len[,'r']                           mandatory (4th)
.PP
*set,ncols,nlines                            optional  (5th)
.PP
*fph,nbl,col,'....heading....'               limit = 10
*fch,nbl,'....heading....'                   limit = 10
*tbl,beg,len,nbl                             limit = 1
*tch,beg,len,nbl [,lines [,nebl]]            limit = 1
*tph,beg,len,nbl,col [,lines]                limit = 1
*equiv,val=beg,len                         }
*equiv,val='.....'                combined } limit = 75
*equiv,'.........'                         }
*tcc,beg,len                                 limit = 1
*tpc,beg,len                                 limit = 1
.SH DESCRIPTION
SHEET requires 2 arguments :
.TP 10
infile
Input file, pre-sorted ie. the records must be in the order in which
the user requires them to be listed by SHEET which increments row then
column then page.
.TP 10
outfile
Formatted output file in the form of lists structured according to the
job directives with each output page labelled with incremental and total counts.
.PP
The first four directives are mandatory and must be in order, the *set 
directive is used only if a setting other than the default (80,60) is
required, when used *set must follow directly after *pfx.
Any *equiv modifiers must follow directly after the heading with which they
are associated.
.PP
The data field to be listed is one continuous field and must 
contain all characters (including blanks) required in the 
output. Re-formatting of records for SHEET cannot be done    
within the program, use REFORM or RECODE beforehand.
.PP
*pfx must not exceed 20 characters in length.
*tch and *tph fields must not exceed 80 characters in length.
.SH DIRECTIVES
.nf
.TP 8
*col
Column specifications
ncols ... no. of data blocks across the page
icol1,icol2, ... column tab for beginning of each  
          data block
.TP 8
*row
line spacing
nbl ... no. of blank lines (after last heading)    
        before first data item printed
nsp ... no. of blank lines between successive data 
        rows
\'m\' ... optional, used to extend pages/block limit  
        from 20 (default) to 100
.TP 8
*data
Data field specifications
beg ... beginning position in record of field to be 
        printed
len ... length of field to be printed
.TP 8
*pfx
Prefix field
beg ... beginning position in record of the prefix 
        field
len ... length of the prefix field in characters
\'r\' ... optional .. if present then duplicate pre-  
        fixes are printed, if absent (default) then 
        duplicate prefixes are not printed until  
        column and/or page changes or heading or  
        prefix itself changes
.TP 6
*set
Settings .. page size
ncols ... page width, default setting = 80, max. = 130
nlines .. page length, default setting = 60, max. = 62
.TP 8
*fph
Fixed Page Heading
nbl ... no. of blank lines before heading
col ... character position on page to begin heading 
\'..\' .. heading to be repeated on every page
      \- must be enclosed in single quotes ('...')
      \- do not continue past column 130, use another 
        *fph directive if necessary (with nbl = -1)
.TP 8
*fch
Fixed Column Heading
nbl ... no. of blank lines before heading
\'..\' .. heading to be repeated for every column  
        across the page with each heading beginning
        at the character position set by *COL
      \- must be enclosed in single quotes ('...')
      \- must fit column width specified in *col 
        directive
      \- use a second *fch (nbl = -1) if >130 chars
.TP 8
*tbl
Triggered Blank Line(s)
beg ... defines the beginning of the trigger field
len ...    "     "  length     "  "     "      " 
      \- if the content of the defined field changes 
        then one or more blank lines will be written  
        before the current data item
nbl ... no. of blank lines to be written
.TP 8
*tch
Triggered Column Heading
beg ... defines the beginning of the trigger field
len ...    "     "  length     "  "     "      "
nbl ... no. of blank lines to be written
lines . optional parameter for forcing new columns 
        under existing columns ie. without a page  
        change. When a column change is triggered
        and NCOLS (see *COL directive) would not be
        exceeded by taking a new column, then a new
        column is taken. If, however, NCOLS would be
        exceeded, a new page is taken unless "lines"
        directs otherwise. If "lines" does not  
        exceed the no. of remaining blank lines on 
        the page a new column is taken on the same
        page, directly below the previous columns,
        beginning at the first column tab. Should
        "lines" exceed the no. of available lines a
        new page will be taken.
nebl .. no. of extra blank lines after last data 
        item before new column heading lower down
        page is printed. If "nebl" is not used
        the new heading will be "nbl" lines below
        the last data item.      
NOTE(1) The onus is on the user to ensure that 
        "lines" is sufficient to include the rele- 
        vent headings, spaces etc. otherwise over-
        flow will occur. Check data and layout!!
.PP
        NOTE(2) Heading will be the data field specified by  
                (beg,len) ... limit 10 characters, unless
                *tch is followed directly by one or more
                *equiv(s) with alternative heading(s).
.TP 8
*tph
Triggered Page Heading
beg ... }
len ... } As for *tch
nbl ... }
col ... beginning position across new page for heading
lines . optional parameter. If absent a new page is
        automatically taken when content of the trigger
        field changes. If specified, a new page heading
        under the existing data on the same page
        results if the parameter is satisified, other-
        wise a new page is taken. The onus is on the
        user to ensure that 'lines' provides enough
        space for ensuing data, if not over-writing 
        can occur. Refer to *tch with lines as the
        principal is the same.
.PP
        NOTE(1) The default heading is the trigger field
                content unless one or more *equiv modifiers
                follow directly after the *tph directive,
                there are three forms of *equiv available.
.TP 8  
*equiv
Equivalence (heading modifier)
val ........... if "val" contains blanks or special
                characters it must be enclosed in 
                single quotes eg. 'val'=beg,len
val=beg,len ... if the content of the trigger field
                is "val", then the content of the
                field defined by (beg,len) becomes
                the heading.
val='     ' ... if the content of the trigger field 
                is "val", then the character string
                enclosed in single quotes becomes
                the heading.
\'     \' ....... the character string enclosed by
                single quotes becomes the heading
                regardless of the content of the
                trigger field.
.TP 8
*tcc
Triggered Column Change
beg ... defines the beginning of the trigger field
len ...    "     "  length     "  "     "      "
        If the content of the defined field changes
        a new column is taken before the current   
        data item is printed. Column heading unchanged.
.TP 8
*tpc
Triggered Page Change
beg ... defines the beginning of the trigger field
len ...    "     "  length     "  "     "      "
        If the content of the defined field changes
        a new page is taken before the current data
        item is printed. Page heading unchanged.
.SH DIAGNOSTICS
errors written to standard error
