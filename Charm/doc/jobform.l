.TH JOBDECKS l CHARM x "CSIRO CHARM USERS GUIDE"
.SH JOBFORM
Script for running a shell job on UNIX in the Bourne Shell.
.PP
nohup time sh -e jobnam 1>outnam 2>&1 &
.nf

nohup ... Optional parameter, prevents a job from aborting if  
          user logs off before a job has completed running.
time  ... Optional parameter, provides information on real,
          user and system time taken by a job.
sh    ... Invokes shell
-e    ... Forces job to abort on error, if not included a job 
          will attempt to proceed despite a fatal error.
jobnam .. Name of jobdeck to be run
1>    ... Re-directs output to specified file, default is to 
          print to the screen.
outnam .. File to receive job output and error messages etc.
2>&1  ... Re-directs error messages to the specified output 
          file ... merges standard output & errors on file
&     ... Runs the job in the background. On submitting the
          job it's process id (3-5 digit number) will be shown  
          on the screen followed by the usual prompt and the     
          user will be able to proceed with other work whilst  
          the job runs. If a job is not run in the background  
          nothing else can be done at the terminal until it 
          finishes.

.PP

.SH .PROFILE
To use the CHARM programs a user's `.profile` must contain a path
to the sub-directory containing these programs (CHARMFTN5).
For an amended path to be available to shell jobs run in background
it is necessary to include an `export PATH` statement also. 
It is also advisable to include /usr/local in PATH so that utilities
contained therein are available to the user.
.PP
Thus the user's .profile will look something like the following :-
.nf
          stty erase
          PATH=/dev1g/grp2/CHARMFTN5$PATH:/usr/local
          export PATH
          TERM=tab132w
          export TERM

TERM refers to terminal type, 'export TERM' is necessary for
the vi editor.
For the keyboard's backspace key to be recognised by UNIX set
stty erase to control H or backspace.



.SH REGISTER
On UNIX we have a Register to keep track of all the Sheep Database groups
and files which are entered in different sub-registers depending on either
the type of file or the group number. Stockfiles and Sheepdatafiles are in
separate sub-registers with prefix `S1` and `A1` respectively (also `A2`)
the groups are presently in 4 separate sub-registers with prefix `G1` to 
`G4`.
.nf
          G1 ... groups    1 - 550
          G2 ... groups  551 - 900
          G3 ... groups  901 - 1100
          G4 ... groups 1101 - 1200
          G5 ... groups 1200 - 1300  (proprosed)
.PP
.fi
The Register records the necessary pathnames for files along with cycle
numbers and file status (whether on disk or tar tape backup or both).
Most recent files and those in general usage are retained on disk as well as
having tape backup. In accessing files with the directory programs it is
usually not necessary to specify cycle number as the default is to take the
most recent (highest) cycle of a file.
.fi
.SH PROGRAMS
.TP 8
grab
AVAILABLE to all users .. accesses a file on the Register. The highest cycle
of a file is accessed unless a particular cycle is specified in the "grab".
If a file is unavailable the program aborts with an informative message.
.TP 8
bag
RESTRICTED to /dev1g/grp2 users ..
enters a file on the Register in the appropriate sub-register as indicated
by the file prefix. If cycle number is specified it's availability is checked
before entering in the Register, otherwise the next cycle number is allocated
for an existing file or cycle=1 for a new file. When a new sub-register is
being created it must first have it's name and prefix entered in "newsubreg"
before a file can be entered in it with `bag`. 
.TP 8
scrub
RESTRICTED to /dev1g/grp2 users only ..
checks the "stat" of a file's entry on the sub-register ...
.nf
(1) stat = D0   File not on tar tape or NOS backup, 
                file entry deleted.
(2) stat = DT   File on tar tape backup and disk, 
                deletes the disk copy, updates stat 
                to 0T.
(3) stat = DTN  File on tar tape & NOS backup and 
                on the disk, deletes disk copy,
                updates stat ~ 0TN
(4) stat = DB   File awaiting tape backup, disk copy 
    stat = DBN  deleted, stat updated to 0B / 0BN  
                (backup copy in BUTxx sub-directory).
(5) stat = 0T   File on tar tape backup only ... 
    stat = 0TN  File on tar tape & NOS backup ....
                abort the job
Scrub is used in conjunction with UNIX `rm` command  
which removes the filename returned by scrub 
(includes the path)
.TP 8
butt
RESTRICTED to /dev1g/grp2 users only ..
Selects files for tape backup .
.TP 8
tfetch
RESTRICTED to /dev1g/grp2 users only ..
Retrieves files from tape backup .
.TP 8
tdel
Deletes files awaiting tape backup and/or those 
already on tape backup.
.TP 8
nosud
RESTRICTED to /dev1g/grp2 users only ..
Checks 'stat' of latest (highest) cycle of each 
file in a specified area, if no NOS backup then
stat(3:3) is updated from ' ' to 'N' and the
(a) filename is printed to xxNOS
(b) filename & path is printed to xxCOPY for
    files already on the disk
(c) filename is printed to xxTAR for files on
    tar tape backup only
.SH JOB NOTES
.TP 8
(1)
Unix saves all the files it makes unless directed otherwise by a remove
statement at the end of a job. To facilitate the handling of these 
`intermediate` or `scratch` files CHARM jobs are run with all `scratch` files
output from the CHARM programs on the sub-directory /tmp. For unique identification
of a user's `scratch` files the job process-id (pid) is incorporated in the
name of each of these files, this allows easy removal of the unwanted files at
the end of a job. The shell variable `$$` returns the `pid` of a shell job.
Thus if all jobdecks begin with the statement :-
.nf
                    t=/tmp/ab$$
where ab indicates any two letters other than ca,ch,cr,               
cb as these are already in use by anne,group,robyn and 
barb.

`Scratch` filenames within the jobdeck are written :-
                    $t.x
thus NOS : select(a,b,c)
and UNIX : select $t.a $t.b $t.c <<eoi
.SH
(2)
At the end of a job the statement :-
.nf
                    rm $t*

Will remove all scratch files made by the job, unless
it aborts in which case this statement will never be  
reached and the user will have to remove the files from
/tmp himself.

                rm /tmp/abxxxxx*      (xxxxx = pid)
.fi
.SH
(3)
If some intermediate files are to be retained then make them files
on your own sub-directory eg.
.nf
         select $t.a $t.b nmbwt <<eoi
This will save the file `nmbwt` on your sub-directory not /tmp
.fi
.SH
(4)
CHARM opens input as `old` files and writes output to `new` files. If
you try to write to an existing file, ie. if in the previous example
a file `nmbwt` already existed on your sub-directory the select would
have aborted with the message `could not open file nmbwt` because it
was trying to open type old as type new.
.SH
(5)
All CHARM programs that have associated directives need a `here document`
to indicate where the program directives end.   
.nf
                  select $t.m $t.n $t.o <<eoi
                  *i,1,2,lt,85
                  *i,20,1,eq,L
                  *e,52,2,eq,20
                  eoi
.fi
Thus the 3 lines between <<eoi and eoi are taken as the directives for
select. The `eoi` is arbitrary, the termination being dependent on the
occurence of a line containing ONLY the string following the <<. For
ourselves we opted to consistently use `eoi` as our terminator.
.SH
(6)
CHARM directive fields that would on NOS have been enclosed in dollar
signs ($.....$) are enclosed in single quotes (') on UNIX.
.nf
     (a) Headings in program SHEET
         *FPH,0,25,'Checklist of Culls'
     (b) Fields being emitted in REFORM or RECODE
         *all,10,'cull'
         *code,20,'11'
         *e,60,'TE2'
.fi
.SH
(7)
When using the Register programs they are enclosed in graves ... backward
sloping single quotes (`) ... this tells the shell to execute what is in
the graves and use it's output. 
.nf
         select `grab G4.1143. ` $t.a $t.b <<eoi
The call to `grab` will return the highest cycle of group
1143 from which the select to $t.a and $t.b will be made,
according to the directives supplied by the user.
NOTE: The delimeter for the Register programs is a blank
space, so even if cycle number is specified in `grab` a
space must still be left before the closing grave.
.fi
         select `grab G4.1143.5 ` $t.a $t.b <<eoi
.SH
(8)
When using the UNIX sort facility nothing will appear on the job output
unless an informative `echo` statement is inserted in the jobdeck 
with, preferably before, each sort.
.nf
         echo '
         sort on tag(1,7) from a to b '
         sort +2.0 -2.7 -o $t.b $t.a
NOTE: a sort incorporating `bag` will print the `bag` output
first unless the informative sort echo is placed before the
actual call to `sort` in the jobdeck.
         echo '
         sort tag(1,7) cn(77,1) from x to G3.1105. '
         sort +2.0 -2.7 +2.76 -2.77 -o `bag G3.1105. ` $t.x
.SH
(9)
Sorting CHARM files ... UNIX `sort` considers records to consist of
1 or more fields delimited by blank(s), col.1 of each new field is the first
non-blank character following a delimeter of blank(s) ... hence a CHARM
record has :-                     
.nf
     field 1 ... record length
     field 2 ... sequence number
     field 3 ... the actual record
The whole of the actual record can be treated as one field
even though it may have blanks in it, but the beginning of
the record will be the first non-blank character following
the field 2 delimeters, therefore a record with any blanks
at the beginning will sort on different columns to one    
without blanks. Correct this by recoding records so that
any blank on col.1 have a `-` in col.1 instead.
To sort field 3 two fields must be skipped (+2.) to specify
columns to be sorted the beginning col. less 1  and the last
column are given. Thus:-
     sort +2.0 -2.7 -o outfile infile
sorts a CHARM record on cols. 1-7 from infile to outfile.
.SH
(10)
Program calc incorporates a user written subroutine so it
exists as a `.o` file which must be loaded to run calc in
a job. The following is an example of a jobdeck using calc.
.nf

f77 -v -f -lskym -o calc /dev1g/grp2/CHARMFTN5/calc.o bxa.f
cat bxa.f
calc $t.a $t.b <<eoi
*a,gfw,41,3,2
*a,yield,44,3,1
*b,cww,47,3,2
eoi
.SH
(11)
Programs which on NOS specified order and sequence now specify order 
(ascending/descending) only. eg. match,count,dupchk
.SH
(12)
When there are several blocks of directives in a `recode` and all records
have been recoded before the blocks of directives are finished recode
finishes up .. ie. unused recode blocks are not printed to output.
.SH
(13)
To print lists made by `sheet` on the DE printer after changing to special
paper
.nf
     fpr <listname |lpr
If there are several lists `cat` them together then print
them out.
     cat list1 list2 list3 >lists
     fpr <lists |lpr
.SH
(14)
To print manuals from usr/man/manl/charm :-
.nf
     man charm/filename
There is a delay while the manual is compiled then it prints
to the screen. Printout can be re-directed to the line 
printer or to a filename in the user's charge.
     man charm/jobform | lpr
