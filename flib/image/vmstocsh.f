c       VMSTOCSH takes a VMS-style command file from standard input and
c       converts it to text suitable for piping to a C shell, on standard
c       output.  The command file may contain: lines to run programs, which
c       must be preceded by either a $ or a %; comment lines, preceded by
c       either $! or #; and entries to the programs that are run, which
c       follow the line starting the program, just as in a VMS command file.
c       See the manual page for more details.
c       
c       $Id$
c       Log at end
c       
      logical reading
      integer*4 iffirst,lenin,lencom,indarrow
      character*10240 linein,linecom,logfile
      character*10 herestring/'HERESTRING'/
c       
      logfile=' '
      lenlog=1
      indarrow = 2
      write(6,101)'nohup'
      if(iargc().ne.0)then
        call getarg(1,logfile)
        lenlog=len_trim(logfile)
        write(6,101)'if (-e "'//logfile(1:lenlog)//'") \\mv -f "'
     &      //logfile(1:lenlog)//'" "'//logfile(1:lenlog)//'~"'
        logfile='  > "'//logfile(1:lenlog)//'"'
        lenlog=len_trim(logfile)
      endif

      write(6,101)'if ($?IMOD_DIR) then'
      write(6,101)'    setenv PATH "$IMOD_DIR/bin:$PATH"'
      write(6,101)'endif'
      write(6,101)'if ($?IMOD_QTLIBDIR && $?LD_LIBRARY_PATH) then'
      write(6,101)
     &    '    setenv LD_LIBRARY_PATH "${IMOD_QTLIBDIR}:$LD_LIBRARY_PATH"'
      write(6,101)'endif'
      write(6,101)'setenv PIP_PRINT_ENTRIES 1'
      write(6,101)'echo2 Shell PID: $$'

      iffirst=-1
      lencom = 2
      reading=.true.
      do while (reading)
        reading=.false.
        linein='$ '
        read(5,101,end=10)linein
101     format(a)
        reading=.true.
10      lenin=len_trim(linein)
c         
c         For Cygwin/windows, if the line is not properly stripped of
c         Return, replace it now
c         
        if (lenin.gt.0) then
          if (linein(lenin:lenin).eq.char(13)) then
            linein(lenin:lenin) = ' '
            lenin = lenin - 1
          endif
        endif
        if(linein(1:1).ne.'#' .and. linein(1:2).ne.'$!')then
          if(iffirst.eq.0.and.linecom(lencom:lencom).eq.'\\')then
            if (lencom.gt.1 .and. linecom(max(1,lencom-1):max(1,lencom-1))
     &          .eq.'\\') then
c               
c               if last line needs to be continued in the output
c               dump the last line, replace with current line
c               
              write(6,101)linecom(2:lencom-1)
              linecom = ' '//linein
            else
c		
c               otherwise, a continuation line of a command line: add it on
c               
              linecom=linecom(1:lencom-1)//' '//linein(1:lenin)
            endif
            lencom=len_trim(linecom)
c             
          elseif(linein(1:1).eq.'$'.or.linein(1:1).eq.'%') then
c             
c             a new command line: if the last line was not an entry line
c             to a previous command, it was a command itself and needs to
c             be passed through now; if it was an entry line, put out the
c             herestring to terminate entries
c             
            if(iffirst.ge.0)then
              if(iffirst.eq.0)then
                write(6,101)linecom(2:lencom)//logfile(1:lenlog)
                logfile(indarrow:indarrow)='>'
              else
                write(6,101)herestring
              endif
            endif
            linecom=linein
            lencom=lenin
            iffirst=0
          else
c             
c             not a command line: if it is the first entry line, dump the
c             command line with the << herestring on the end
c             in any case, pass the line through
c             
            if(iffirst.ge.0)then
              if(iffirst.eq.0)then
                write(6,101)linecom(2:lencom)
     &              //' << '//herestring//logfile(1:lenlog)
                logfile(indarrow:indarrow)='>'
                iffirst=1
              endif
c               
c               Remove escape of leading $ so variables can be passed in
c               
              if (linein(1:2) .eq. '\\$') then
                write(6,101)linein(2:lenin)
              else
                write(6,101)linein(1:lenin)
              endif
            endif
          endif
        endif
      enddo
      write(6,101)'echo SUCCESSFULLY COMPLETED'//logfile(1:lenlog)
      call exit(0)
      end

c       
c       $Log$
c       Revision 3.13  2010/04/26 22:02:06  mast
c       Made log file length 10240 and allowed spaces in it
c
c       Revision 3.11.2.1  2010/04/09 22:13:25  mast
c       Increase log file size to 320
c
c       Revision 3.11  2009/12/04 20:28:23  mast
c       Set variable for printing entries, add success statement at end
c
c       Revision 3.10  2008/05/08 02:49:54  mast
c       Increased line length to 10240, nested if statement to avoid invalid
c       index to character variable
c
c       Revision 3.9  2007/12/10 15:57:02  mast
c       Had to add braces around IMOD_QTLIBDIR
c
c       Revision 3.8  2007/11/20 20:31:04  mast
c       Added IMOD_QTLIBDIR to LD_LIBRARY_PATH if both defined
c
c       Revision 3.7  2005/11/30 06:51:45  mast
c       Needed to initialize lencom to valid value for if test to work
c	
c       Revision 3.6  2005/11/19 16:59:55  mast
c       Fixed path-setting to preserve spaces
c	
c       Revision 3.5  2005/07/24 16:47:25  mast
c       Added ability to escape $ at start of non-command line
c	
c       Revision 3.4  2004/06/29 03:30:48  mast
c       Fixed path-setting command, added nohup at front
c	
c       Revision 3.3  2003/11/18 04:13:33  mast
c       add ability to output a true continuation line ending in \
c	
