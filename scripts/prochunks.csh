#!/bin/csh -f
# A script to process command files on multiple machines
#
#  $Id$
#
#  Log at end of file
#

nohup

# Basic sleep time, and number of machines for each extra second of sleep
#
@ sleepsec = 2
@ numPerExtraSec = 100
@ maxLocalByNum = 32

set pn = processchunks
set nice = 18
@ dropcrit = 5
@ maxchunkerr = 5
@ timeout = 30
set queuename = queue

if ($#argv < 2) then
cat <<EOF
Usage: $pn [options] machine_list root_name
Will process multiple command files on multiple processors or machines
  machine_list is a list of available machines, separated by commas; enter 
    ALL (or all) for a list defined in environment variable IMOD_ALL_MACHINES
  root_name is the base name of the command files, omitting -nnn.com
  Options:
    -r     Resume, retaining existing finished files (the default is to remove
             all log files and redo everything)
    -s     Run a single command file named root_name or root_name.com
    -O #   Set maximum number of threads per process (default 1 unless -s)
    -g     Go process, without asking for confirmation after probing machines
    -n #   Set the "nice" value to the given number (default $nice, range 0-19)
    -d #   Drop a machine after given number of failures in a row (default $dropcrit)
    -e #   Quit after the given # of processing errors for a chunk (default $maxchunkerr)
    -c name  Check file "name" for commands P, Q, and D
    -w path  Full path to working directory on remote machines
    -q #   Run on cluster queue with given maximum # of jobs at once
    -Q name  Machine name to use for the queue (default $queuename)
    -P     Output process ID
EOF
exit 0
endif

if ($?IMOD_DIR) then
    setenv PATH "$IMOD_DIR/bin:$PATH"
endif

# Set the ssh options based on ssh version
#
set sshopts = "-o PreferredAuthentications=publickey -o StrictHostKeyChecking=no"
set sshvers = `ssh -V | & sed -n -e '/[[:cntrl:]]/s///g' -e '/.*_\([0-9]\)\.\([0-9]\).*/s//\1\2/p'`
if ("$sshvers" != "") then
    @ version = $sshvers
    if ($version >= 39) set sshopts = "-o ConnectTimeout=5 $sshopts"
endif

set retain = 0
set justgo = 0
set skipprobe = 0
set checkfile = ""
@ queue = 0
set singleFile = 0
set sshext = ssh
set pidext = pid

while ($#argv > 2)
  switch ($argv[1])
    case -r:
      set retain = 1
      shift
      breaksw

    case -g:
      set justgo = 1
      shift
      breaksw

    case -s:
      set singleFile = 1
      shift
      breaksw

    case -O:
      @ numThreads = $argv[2]
      shift ; shift
      breaksw

    case -n:
      set nice = $argv[2]
      shift ; shift
      breaksw

    case -d:
      @ dropcrit = $argv[2]
      shift ; shift
      breaksw

    case -e:
      @ maxchunkerr = $argv[2]
      shift ; shift
      breaksw

    case -P:
      echo2 Shell PID: $$
      set skipprobe = 1
      shift
      breaksw

    case -c:
      set checkfile = `\echo "$argv[2]" | sed '/\\/s//\//g'`
      shift ; shift
      breaksw

    case -w:
      set curdir = "$argv[2]"
      shift ; shift
      breaksw

    case -q:
      @ queue = $argv[2]
      shift ; shift
      breaksw

    case -Q:
      set queuename = $argv[2]
      shift ; shift
      breaksw

    default:
        echo "ERROR: $pn - Unrecognized option $argv[1]"
        exit 1
        breaksw
  endsw
end

if ($retain && $singleFile) then
    echo "ERROR: $pn - You cannot use the retain option with a single command file"
    exit 1
endif

if (! $?numThreads) then
    @ numThreads = 1
    if ($singleFile) @ numThreads = 0
endif

if ($queue) then

    # For a queue, make a machine list that is all the same name
    #
    set queuecom = "$argv[1]"
    @ i = 0
    set machines = ()
    while ($i < $queue)
        @ i++
        set machines = ($machines $queuename)
    end
    set skipprobe = 1
    set justgo = 1
    set sshext = job
    set pidext = qid

else if ("$argv[1]" == "ALL" || "$argv[1]" == "all") then
    if ($?IMOD_ALL_MACHINES) then
        set machines = `echo $IMOD_ALL_MACHINES | sed '/,/s// /g'`
        echo "The All-machine list is:"
        echo "$IMOD_ALL_MACHINES"
    else
        echo "ERROR: $pn - Environment variable IMOD_ALL_MACHINES is not defined"
        exit 1
    endif
else
    set machines = `echo $argv[1] | sed '/,/s// /g'`
endif

if ($#machines < 1) then
    echo "ERROR: $pn - No machines specified"
    exit 1
endif

# Translate a single number into a list of localhost entries
#
if ($#machines == 1 && ! $queue) then
    set numonly = `echo $machines | sed '/[^0-9]/s///g'`
    if ("$machines" == "$numonly") then
        @ numlocal = $machines
        if ($numlocal > $maxLocalByNum) then
            echo "ERROR: $pn - You cannot run more than $maxLocalByNum chunks on localhost by entering a number"
            exit 1
        endif
        if (! $numlocal) @ numlocal = 1
        set machines = ()
        @ i = 0
        while ($i < $numlocal)
            @ i++
            set machines = (localhost $machines)
        end
    endif
endif

set rootname = $argv[2]
set endroot = $rootname-gobbledygook
if ($singleFile) then

    # Make the list for a single file
    set rootname = $rootname:r
    set comlist = ($rootname.com)
    if (! -e "$rootname.com") then
        echo "ERROR: $pn - The single command file $rootname.com does not exist"
        exit 1
    endif
else

    # Build up lists in order -nnn, -nnnn, -nnnnn*, which should work both for
    # lists that are all 5 digits or lists that are 3, 4, 5 digits
    # Put -start.com on front and -finish.com on end
    #
    set endroot = $rootname-finish
    set comlist = ()
    if (-e $rootname-start.com) set comlist = ($rootname-start.com)

    set hundlist = `\find . -maxdepth 1 -name "$rootname-[0-9][0-9][0-9][^0-9]*com" -print | sed '/\.\//s///' | sort`
    if ($#hundlist > 0) set comlist = ($comlist $hundlist)

    set thoulist = `\find . -maxdepth 1 -name "$rootname-[0-9][0-9][0-9][0-9][^0-9]*com" -print | sed '/\.\//s///' | sort`
    if ($#thoulist > 0) set comlist = ($comlist $thoulist)

    set tenthoulist = `\find . -maxdepth 1 -name "$rootname-[0-9][0-9][0-9][0-9][0-9]*com" -print | sed '/\.\//s///' | sort`
    if ($#tenthoulist > 0) set comlist = ($comlist $tenthoulist)

    if (-e $endroot.com) set comlist = ($comlist $endroot.com)

    if ($#hundlist < 1 && $#thoulist < 1 && $#tenthoulist < 1) then
        echo "ERROR: $pn - There are no command files matching $rootname-nnn.com"
        exit 1
    endif
endif


set thishost = `hostname`
set hostroot = $thishost:r:r:r

# Get current directory if not entered, escape the spaces
#
if ($?curdir) then
    set curdir = `echo "$curdir" | sed -e '/ /s//\\ /g'`
else
    set curdir = `pwd | sed -e "/^\/localscratch/s//\/scratch\/$hostroot/" -e "/ /s//\\ /g"`
endif

# probe machines and get all the verifications unless etomo is running it
#
if ($skipprobe == 0 || $justgo == 0) then
    if ("$checkfile" != "" && -e "$checkfile") \rm -f $checkfile
    echo "Probing machine connections and loads..."
    @ num = 0
    set newlist = ()
    while ($num < $#machines)
        @ num++ 
        set machname = $machines[$num]
        set checkit = 1
        if ($num > 1) then
            @ last = $num - 1
            if ( "$machname" == "$machines[$last]" ) set checkit = 0
        endif
        if ($checkit == 1) then
            if ("$machname" == $hostroot || $machname == localhost) then
                echo $machname
                w | head -n 1
                set laststat = 0
            else
                ssh -x $sshopts $machname "hostname ; w | head -n 1"
                set laststat = $status

                # The SGI returns 141 for some strange reason so...
                #
                if ($laststat != 0 && $laststat != 141) then
                    echo "Dropping $machname from list because it does not respond"
                    echo ""
                endif
            endif
        endif
        if ($laststat == 0 || $laststat == 141) set newlist = ($newlist $machname)
    end 

    set machines = ($newlist)
endif

# Set sleep time now that machines are known
#
@ sleepsec = $sleepsec + $#machines / $numPerExtraSec

if ($justgo == 0) then
    echo -n "Enter Y to proceed with the current set of machines: "
    set answer = $<
    if ("$answer" != "Y" && "$answer" != "y") exit
endif

restart:

# set up flag list and list of assignments and set up which chunk to copy the
# log from, the first non-sync if any, otherwise just the first one
#
set flags = ()
set numchunkerr = ()
@ num = 0
@ copylog = -1
while ($num < $#comlist)
    @ num++
    set sync = 0
    set comname = $comlist[$num]
    if ($singleFile || $comname =~ *-start.com || $comname =~ *-finish.com || \
        $comname =~ *-sync.com ) set sync = -1
    set flags = ($flags $sync)
    set numchunkerr = ($numchunkerr 0)
    if ($sync == 0 && $copylog < 0) @ copylog = $num
end
if ($copylog < 0) @ copylog = 1

set assigned = ()
set startcount = ()
set failed = ()
set namelist = ()
set nameind = ()
set chunkerred = ()
@ num = 0
while ($num < $#machines)
    @ num++
    set assigned = ($assigned 0)
    set startcount = ($startcount 0)


    # Build a list of unique names and an index to the name and failure list
    #
    @ name = 0
    set index = 0
    while ($name < $#namelist)
        @ name++
        if ($namelist[$name] == $machines[$num]) then
            set index = $name
            break;
        endif
    end

    if ($index == 0) then
        set namelist = ($namelist $machines[$num])
        set index = $#namelist
        set failed = ($failed 0)
        set chunkerred = ($chunkerred 0)
    endif
    set nameind = ($nameind $index)
end

# Set this to prevent errors from trying to substitute *'s in error messages
# Quoting the assignments did not always work
set noglob

# Prescan logs for done ones to find first undone one, or back up unfinished
#
@ numdone = 0
@ firstundone = -1
@ num = 0
while ($num < $#comlist)
    @ num++
    set logname = $comlist[$num]:r.log
    if (-e $logname) then
        set lastline = `tail -n 1 $logname | sed '/[[:cntrl:]]/s///g'`
        if ("$lastline" == "CHUNK DONE") then
        
            # If it was done and we are resuming, set flag it is done, count
            if ($retain) then
                set flags[$num] = 2
                @ numdone++
            endif
        else if ($retain == 0) then

            # If it was not done and we are restarting, back up the old log
            #
            \mv $logname $logname~
        endif
    endif

    # If resuming and this is the first undone one, keep track of that
    #
    if ($retain && $firstundone == -1 && $flags[$num] != 2) \
        @ firstundone = $num
end 
if ($firstundone == -1) @ firstundone = 1

# remove logs if not restarting
#
if ($retain == 0) then
    \find . -maxdepth 1 -type f -name  "$rootname-[0-9][0-9][0-9]*.log" -exec rm -f "{}" \;
    \find . -maxdepth 1 -type f -name  "$rootname-start.log" -exec rm -f "{}" \;
    
    \find . -maxdepth 1 -type f -name  "$endroot.log" -exec rm -f "{}" \;
    if ($singleFile) \find . -maxdepth 1 -type f -name  "$rootname.log" -exec rm -f "{}" \;
endif

# Clean up .ssh and .pid files to avoid confusion and .csh on general principle
#
\find . -type f -name  "$rootname-[0-9][0-9][0-9]*.$sshext" -exec rm -f "{}" \;
\find . -type f -name  "$rootname-[0-9][0-9][0-9]*.$pidext" -exec rm -f "{}" \;
\find . -type f -name  "$rootname-[0-9][0-9][0-9]*.csh" -exec rm -f "{}" \;

onintr report
if (! $singleFile || $skipprobe) echo $numdone OF $#comlist DONE SO FAR

# Loop until all appear to be done
#
@ lastdone = 0
@ count = 0
@ nextsync = $#comlist + 2
set anydone = 0
@ holdcrit = ($#machines + 1) / 2
set pausing = 0
@ checkline = 0
set syncing = 0

continue:

while ($numdone < $#comlist)
    @ count++

    # Check the check file now so it can be stopped before running anything
    #
    if ("$checkfile" != "" && -r "$checkfile") then
        set checkwc = `wc -l $checkfile`
        @ checklen = $checkwc[1]
        if ($checklen > $checkline) then
            @ checkline++
            set comline = `tail -n +$checkline $checkfile | head -n 1 | sed '/[[:cntrl:]]/s///g'`
            if ($#comline > 0) then
                set ans = $comline[1]
                if ("$ans" == "P" || "$ans" == "Q" || \
                    ("$ans" == "D" && $#comline > 1)) then
                    if ("$ans" == "D") then
                        shift comline
                        set drops = `echo $comline | sed '/,/s// /g'`
                    endif
                    goto command
                else
                    echo "BAD COMMAND IGNORED: $comline"
                endif
            endif
        endif
    endif

    # Count failures and assignments
    #
    @ proc = 0
    @ failtot = 0
    @ assigntot =0
    @ minfail = $dropcrit
    @ chunkerrtot = 0
    while ($proc < $#machines)
        @ proc++
        if ($assigned[$proc] != 0) @ assigntot++
        @ failcount = $failed[$nameind[$proc]]
        if ($failcount != 0) @ failtot++
        if ($failcount < $minfail) @ minfail = $failcount
        if ($chunkerred[$nameind[$proc]] != 0) @ chunkerrtot++
    end

    # Stop if all have now been dropped out or all have failed and none done
    #
    if ($minfail >= $dropcrit) then
        echo "ERROR: ALL MACHINES HAVE BEEN DROPPED DUE TO FAILURES"
        exit 1
    endif

    if ($failtot == $#machines && $assigntot == 0 && $numdone == 0) then
        echo "ERROR: NO CHUNKS HAVE WORKED AND EVERY MACHINE HAS FAILED"
        exit 1
    endif

    if ($pausing != 0 && $assigntot == 0) then
        echo "All previously running chunks are done - exiting as requested"
        echo "Rerun with -r to resume and retain existing results"
        exit 2
    endif

    # Loop on machines, if they have an assignment check if it is done
    #
    @ proc = 0
    set nochunks = 0
    set didlslogs = 0
    while ($proc < $#machines)
        @ proc++
        @ ind =$assigned[$proc]
        set dropout = 0
        set name = $nameind[$proc]
        if ($ind != 0) then
            set logname = $comlist[$ind]:r.log
            set cshname = $comlist[$ind]:r.csh
            set sshname = $comlist[$ind]:r.$sshext
            set pidname = $comlist[$ind]:r.$pidext
            set errormess = ""
            set dropmess = ""
            set checkpid = ""
            if ((-e $logname) && (! -e $cshname)) then
                
                # If the log is present and the .csh is gone, it has exited
                #
                set lastline = `tail -n 1 $logname | sed '/[[:cntrl:]]/s///g'`
                if ("$lastline" == "CHUNK DONE") then

                    # If it is DONE, then set flag to done and deassign
                    # Exonerate the machine from chunk errors if this chunk 
                    # gave a previous chunk error
                    #
                    set flags[$ind] = 2
                    set assigned[$proc] = 0
                    set failed[$name] = 0
                    set syncing = 0
                    if ($numchunkerr[$ind] != 0) set chunkerred[$name] = 0
                    @ numdone++
                    echo "$comlist[$ind] finished on $machines[$proc]"
                    grep 'WARNING:' $logname
                    if ($singleFile && ! $skipprobe) exit 0
                    if ($singleFile) break
                    
                    # If this is the first one done, issue drop messages now
                    # on ones that chunk errored and exceeded failure count
                    #
                    if ($anydone == 0) then
                      @ tmpind = 0
                      while ($tmpind < $#namelist)
                        @ tmpind++
                        @ failcount = $failed[$tmpind]
                        if ($failcount >= $dropcrit && $chunkerred[$tmpind]) \
                           echo "Dropping $namelist[$tmpind]"
                      end
                    endif
                    set anydone = 1

                    # copy the log for the first non-sync chunk
                    #
                    if ($ind == $copylog) then
                        if (-e $rootname.log) \mv $rootname.log $rootname.log~
                        cat >! $rootname.log << EOF
THIS FILE IS JUST THE LOG FOR ONE CHUNK AND WAS COPIED BY PROCESSCHUNKS FROM
$logname

EOF
                        cat $logname >> $rootname.log
                    endif
                else

                    # otherwise set flag to redo it
                    #
                    set dropout = 1
                    if (! -z $logname) then
                        set errormess = `awk '/ERROR:/, /EOF/' $logname`
                        if ("$errormess" == "") then
                            set errormess = `sed '/^ *$/d' $logname | tail -n 1`
                            set errormess = "CHUNK ERROR: (last line) - $errormess"
                        else
                            set errormess = "CHUNK $errormess"
                        endif
                        @ numerr = $numchunkerr[$ind]
                        @ numerr++
                        set numchunkerr[$ind] = $numerr
                        set chunkerred[$name] = 1

                        # Give up if the chunk errored too many times: and
                        # for a sync chunk that is twice or once if one machine
                        #
                        if ($numerr >= $maxchunkerr || ($syncing && \
                            ($#namelist == 1 || $numerr >= 2))) then
                            echo \
        "$comlist[$ind] has given processing error $numerr times - giving up"
                            echo "$errormess"
                            set ans = "E"
                            set assigned[$proc] = 0
                            goto command
                        endif
                    else if (! $queue && -e $pidname) then
                        
                        # If log is zero length, check for something in .pid
                        #
                        set checkpid = `grep -v PID: $pidname`
                    endif
                endif
            else if ($queue && ! -e $pidname) then

                # For a queue, the qid file should be there
                #
                set dropout = 1
                set dropmess = "it failed to be submitted to queue"

            else if (! $queue) then

                # Either there is no log file or the .csh is still present:
                # check the ssh file and accumulate timeout
                #
                if (-e $sshname && ! -z $sshname) then

                    # If the ssh file is non empty check for errors there
                    #
                    set checkcd = `grep 'cd: ' $sshname`
                    set checkdown = `grep 'ssh: connect to host' $sshname`
                    if ("$checkcd" != "" || "$checkdown" != "") then
                        set dropout = 1
                        set failed[$name] = $dropcrit
                        if ("$checkcd" != "") \
                            set dropmess = "it cannot cd to $curdir ($checkcd)"
                        if ("$checkdown" != "") \
                            set dropmess = "cannot connect ($checkdown)"
                    endif
                endif
                if (! -e $logname) then

                    # If log file seems not to exist, do a ls on all logs once
                    if (! $didlslogs) then
                        unset noglob
                        ls *.log >& /dev/null
                        set noglob
                        set didlslogs = 1
                    endif
                    if (! -e $logname) then

                        # if log file doesn't exist, check the pid
                        # and give up after timeout
                        #
                        if (-e $pidname) set checkpid = `grep -v PID: $pidname`
                        @ tmpcount = $startcount[$proc]
                        if ($count > $tmpcount + $timeout) then
                            echo "Timeout occurred on lack of log file:"
                            set dropout = 1
                        endif
                    endif
                endif
            endif

            # Now if pid file has anything but a PID in the two cases of
            # nonexistent or zero-length log file, drop machine
            #
            if ("$checkpid" != "") then
                set dropout = 1
                set failed[$name] = $dropcrit
                set dropmess = "it cannot run IMOD commands ($checkpid)"
            endif

            # if failed, remove the assignment, mark chunk as to be done, 
            # skip this machine on this round
            #
            if ($dropout == 1) then
                echo "$comlist[$ind] failed on $machines[$proc] - need to restart"
                if ("$errormess" != "") echo "$errormess"
                set flags[$ind] = 0
                if ($syncing) set flags[$ind] = -1
                if ($syncing) set syncing = 1
                set assigned[$proc] = 0
                set nochunks = 0
                @ failcount = $failed[$name]
                @ failcount++
                set failed[$name] = $failcount
                if ($failcount >= $dropcrit) then
                    if ("$dropmess" == "" && $chunkerred[$name] == 0) \
                      set dropmess = "it failed (with time out) $failcount times in a row"
                    if ("$dropmess" == "") set dropmess = \
                      "it failed (with chunk error) $failcount times in a row"
                    if ($anydone == 0 && $chunkerred[$name] != 0) then
                      echo "Holding off on using $machines[$proc] - $dropmess"
                    else
                      echo "Dropping $machines[$proc] - $dropmess"
                    endif
                endif
            endif

            # Clean up .ssh and .pid if no longer assigned
            #
            if ($assigned[$proc] == 0) \rm -f $pidname $sshname
        endif

        # Drop a machine if it has failed more than given number of times
        # Institute hold on any failed machine if no chunks are done and 
        # machine failure count is above criterion
        #
        @ failcount = $failed[$name]
        if ($failcount >= $dropcrit || $pausing != 0 || ($failcount && \
            $anydone == 0 && $failtot >= $holdcrit)) set dropout = 1

        # If the current machine is unassigned, find next com to do and run it
        # Move current log out of way so non-existence of log can be sign of
        # nothing having started.  Skip if no chunks are available
        #
        if ($assigned[$proc] == 0 && $dropout == 0 && $nochunks == 0 && \
                $syncing != 2) then
            @ num = $firstundone
            set foundchunks = 0
            @ undone = -1
            while ($num <= $#comlist && $assigned[$proc] == 0)
                @ runflag = $flags[$num]

                # But if the next com is a sync, record number and break loop
                #
                if ($runflag == -1 && ! $syncing) then
                    @ nextsync = $num
                    if ($foundchunks == 0) set nochunks = 1
                    break
                endif
                if ($undone == -1 && $runflag != 2) @ undone = $num

                # If any chunks found set that flag
                #
                if ($runflag <= 0) set foundchunks = 1

                # Skip a chunk if it has errored, if this machine has given chunk 
                # error, and not all machines have done so
                #
                set chunkok = 1
                if ($runflag <= 0 && $numchunkerr[$num] > 0 && \
                    $chunkerred[$name] == 1 && $chunkerrtot < $#machines) then
                    set chunkok = 0
                    if ($syncing) break
                endif

                if ($runflag <= 0 && $chunkok == 1) then
                    set assigned[$proc] = $num
                    set flags[$num] = 1
                    set startcount[$proc] = $count
                    set comname = $comlist[$num]
                    set logname = $comname:r.log
                    set cshname = $comname:r.csh
                    set pidname = $comname:r.$pidext
                    set sshname = $comname:r.$sshext
                    set machname = $machines[$proc]
                    if (-e $logname) \mv $logname $logname~
                    if (-e $pidname) \rm -f $pidname
                    echo "Running $comname on $machname ..."
                    if ($queue) then
                        \echo -n >! $cshname
                    else
                        \echo "nice +$nice" >! $cshname
                    endif
                    if ($numThreads > 0) then
                        \echo "setenv OMP_NUM_THREADS $numThreads" >> $cshname
                    endif

                    # convert and add CHUNK DONE to all files
                    #
                    vmstocsh $logname < $comname >> $cshname
                    echo "echo CHUNK DONE >> $logname" >> $cshname

                    # If running a sync, set the syncing flag to 2
                    #
                    if ($syncing) set syncing = 2
                    if ($queue) then
                        $queuecom -w "$curdir" -a R $comname:r
                    else if ("$machname" == $hostroot || "$machname" == localhost) then
                        csh -ef < $cshname >& $pidname ; \rm -f $cshname &
                    else
                        ssh -x $sshopts $machname bash --login -c \'"cd $curdir && (csh -ef < $cshname >& $pidname ; \rm -f $cshname)"\' >&! $sshname &
                    endif
                endif
                @ num++
            end 

            # If no chunks were found in that loop set the nochunks flag
            #
            if ($foundchunks == 0) set nochunks = 1
            if ($undone > $firstundone) @ firstundone = $undone
        endif
    end

    if ($numdone > $lastdone) echo $numdone OF $#comlist DONE SO FAR
    @ lastdone = $numdone

    # If we have finished up to the sync file, then allow the loop to run it
    if ($numdone >= $nextsync - 1) then
        if ($comlist[$nextsync] == $endroot.com) \
            echo "ALL DONE - going to run $endroot to reassemble"

        # Set syncing flag to 1 to get it started
        set syncing = 1
        @ firstundone = $nextsync
        @ nextsync = $#comlist + 2
        set nochunks = 0
    endif

    if (! ($singleFile && $numdone > 0)) sleep $sleepsec
end

# Etomo is looking for "to reassemble"
#
if (! -e "$endroot.com") echo "ALL DONE - nothing to reassemble"

# Etomo is looking for this line too
#
echo "Finished reassembling"
if ("$checkfile" != "" && -e "$checkfile") \rm -f $checkfile
exit

report:
@ undone = $#comlist - $numdone
echo  " $undone chunks are still undone"
set ans = 0
while ("$ans" != "R" && "$ans" != "Q" && "$ans" != "C" && "$ans" != "P" && "$ans" != "D")
    echo "Enter R to kill and restart with the current list of machines,"
    echo " Q to kill all jobs and quit, P to finish running jobs then exit,"
    echo " D machine_list to kill jobs and drop given machines,"
    echo " or C to continue waiting: "
    set ans = "$<"
    set ans = ($ans)
    if ("$ans" == "r") set ans = R
    if ("$ans" == "q") set ans = Q
    if ("$ans" == "p") set ans = P
    if ("$ans" == "c") set ans = C
    if ($#ans > 1) then
        if ($ans[1] == "d" || $ans[1] == "D") then
            shift ans
            set drops = `echo $ans | sed '/,/s// /g'`
            set ans = D
        endif
    endif
end

command:
set pausing = 0
if ("$ans" == "P") set pausing = 1
if ("$ans" == "C" || ("$ans" == "P" && ! $queue)) goto continue

# now try to kill jobs
#
unset noglob
@ proc = 0
set machkills = ()
set comkills = ()
set pidkills = ()
while ($proc < $#machines)
    @ proc++
    @ ind =$assigned[$proc]
    set name = $nameind[$proc]
    set machname = $machines[$proc]
    set kill = 1
    if ("$ans" == "D") then
        set kill = 0
        foreach i ($drops)
            if ("$machname" == $i) then
                set kill = 1
                set failed[$name] = $dropcrit
                set assigned[$proc] = 0
                echo "Dropping $machname as requested"
            endif
        end
    endif
    if ($ind != 0 && $kill != 0) then
        set flags[$ind] = 0
        set pidname = $comlist[$ind]:r.$pidext
        set cshname = $comlist[$ind]:r.csh

        # If the PID file does not exist, do a ls
        # This did the trick on our RHEL5 network - local and remote syncs
        # did not
        if ((! $queue) && (! -e $pidname)) then
            ls *.pid >& /dev/null
        endif

        if ($queue && "$ans" != "D") then
            set action = $ans
            if ("$ans" != "P") set action = "K"
            $queuecom -w "$curdir" -a $action $comlist[$ind]:r
            if (! $status) set assigned[$proc] = 0
        else if (-e $pidname) then
            echo "Killing $comlist[$ind] on $machname"

            # If the pid file is not really there yet, do a sync and wait for
            # it to take effect
            #
            if (-z $pidname) then
                if ("$machname" == $hostroot || "$machname" == localhost) then
                    sync
                else
                    ssh -x $sshopts $machname sync
                endif
                @ waited = 0
                while (-z $pidname && $waited < 15)
                    @ waited++
                    sleep 1
                end
            endif
            set pid = `cat $pidname | sed '/[^0-9]/s///g'`
            if ("$machname" == $hostroot || "$machname" == localhost) then
                imodkillgroup $pid
                \rm -f $pidname
            else

                # Kill a remote job in background and have this command clean
                # up the PID file
                #
                ssh -x $sshopts $machname bash --login -c \'"imodkillgroup $pid ; \rm -f $curdir/$pidname"\' &
            endif
            set machkills = ($machkills $machname)
            set comkills = ($comkills $comlist[$ind])
            set pidkills = ($pidkills $pidname)
            \rm -f $cshname
        endif
    endif
end

# Loop until all pid's disappear or it times out
#
@ waited = 0
@ undone = 1
while ($waited < 15 && $undone > 0)
    @ waited++
    sleep 1
    @ ind = 0
    @ undone = 0
    while ($ind < $#pidkills)
        @ ind++
        if (-e $pidkills[$ind]) @ undone++
    end
end

# Loop again to clean up and report if anybody didn't clean up
#
@ ind = 0
while ($ind < $#pidkills)
    @ ind++
    if (-e $pidkills[$ind]) then
        \rm -f $pidkills[$ind]
        echo "Failed to kill $comkills[$ind] on $machkills[$ind] (no problem if machine is dead)"
    endif
end

if ("$ans" == "E") then
    if (! $syncing) echo "ERROR: A CHUNK HAS FAILED $maxchunkerr TIMES"
    if ($syncing) echo "ERROR: A START, FINISH, OR SYNC CHUNK HAS FAILED"
    exit 4
endif

if ("$ans" == "D" || "$ans" == "P") then
    echo "Resuming processing"
    goto continue
endif

set retain = 1
if ("$ans" == "R") goto restart
echo ""
echo "When you rerun with a different set of machines, be sure to use"
echo "the -r flag to retain the existing results"
exit 2


#
#  $Log$
#  Revision 3.55  2010/08/31 18:36:10  mast
#  Fixed two problems with hanging due to nochunks being set erroneously
#
#  Revision 3.54  2010/08/19 01:48:24  mast
#  Fixed test for no numbered com files
#
#  Revision 3.53  2010/06/14 20:42:37  mast
#  Copy log from first non-sync chunk if any; report WARNINGS
#
#  Revision 3.52  2010/05/25 03:23:04  mast
#  Increased specificity of cleanup to include 3 digits
#
#  Revision 3.51  2010/01/08 20:27:22  mast
#  Allowed a single number to be used for localhost entries
#
#  Revision 3.50  2009/12/07 20:54:09  mast
#  Make processchunks responsible for CHUNK DONE
#
#  Revision 3.49  2009/11/11 20:45:08  mast
#  Added ability to run a single command file
#
#  Revision 3.48  2009/11/11 05:56:04  mast
#  Made start, sync, and finish all run asynchronously on first machine;
#  they are now killable, and resume will treat start and finish properly.
#  Backed up logs of undone chunks on fresh start.
#
#  Revision 3.47  2009/08/04 22:41:08  mast
#  Move the noglob up so it works when scanning existing logs
#
#  Revision 3.46  2009/05/26 18:37:12  mast
#  Ignore error lines when getting version string
#
#  Revision 3.45  2009/03/04 01:24:36  mast
#  Changes to handle up to 99999 files with constant or increasing digits
#  Set noglob during the main loop to prevent problems from * in errors
#
#  Revision 3.44  2009/01/23 18:00:37  mast
#  Made it say it is holding a machine if no chucks done yet, and drop it when
#  one is done.
#
#  Revision 3.43  2008/12/08 21:12:29  mast
#  Fixed hold to hold only on a failed machine
#
#  Revision 3.42  2008/07/16 04:37:05  mast
#  Added silly ls *.log if log file not present
#
#  Revision 3.41  2008/07/06 21:36:31  mast
#  Use ls *.pid to bring them into existence if they are missing
#
#  Revision 3.40  2008/02/22 04:58:39  mast
#  Made it convert backslash in cmds file, quoted current dir also
#
#  Revision 3.39  2008/02/20 01:29:04  mast
#  Fixed bug in last change and made it keep track of first undone chunk to
#  minimize the looping it has to do to find a chunk to assign
#
#  Revision 3.38  2008/02/18 23:21:11  mast
#  Grep on a space too to avoid finding cd: in ssh key complaints
#
#  Revision 3.37  2008/02/18 18:53:03  mast
#  Made it not check for every machine when no more chunks are available
#
#  Revision 3.36  2008/01/22 03:49:46  mast
#  Quoted drop message to step problems from weird messages
#
#  Revision 3.35  2008/01/11 05:38:51  mast
#  Stripped control chars from ssh version sed output
#
#  Revision 3.34  2007/12/10 22:59:49  mast
#  Need to quote errormess when echoing it
#
#  Revision 3.33  2007/10/04 00:34:45  mast
#  Made it able to run without a finish file
#
#  Revision 3.32  2007/09/14 21:54:18  mast
#  Added cluster support
#
#  Revision 3.31  2007/08/16 05:01:49  mast
#  Changes to handle up to 10000 command files, nnn then nnnn.com
#
#  Revision 3.30  2007/07/17 22:05:39  mast
#  Make it copy the first log to rootname.log
#
#  Revision 3.29  2007/05/25 18:56:03  mast
#  Added ssh options the same as are used in etomo, to prevent password
#  requests and problems with host keys
#
#  Revision 3.28  2007/05/03 23:49:42  mast
#  Fixed bug when there is failure to run a sync file and improved error
#  extraction from log; fixed bug when log file is empty and PID file exist
#
#  Revision 3.27  2007/03/05 22:34:10  mast
#  Added finished output for sync
#
#  Revision 3.26  2006/10/09 23:46:32  mast
#  Moved checking of check file to top of run loop
#
#  Revision 3.25  2006/09/19 20:29:10  mast
#  Fixed problem with losing timeouts and checked for connect errors too
#
#  Revision 3.24  2006/09/18 20:30:58  mast
#  Made it capture ssh output and analyze PID output too, so it can detect
#  faster if something goes wrong and report failure to cd or run IMOD
#
#  Revision 3.23  2006/02/16 06:47:09  mast
#  Stripped control chars from output of sed/header etc for Windows
#
#  Revision 3.22  2006/01/07 01:17:28  mast
#  Standardized errors and extracted com file errors on start, finish, sync
#
#  Revision 3.21  2005/11/19 04:31:26  mast
#  Quote path setting to preserve spaces
#
#  Revision 3.20  2005/11/16 05:52:32  mast
#  *** empty log message ***
#
#  Revision 3.19  2005/10/06 16:47:25  mast
#  Removed extra character
#
#  Revision 3.18  2005/10/06 16:44:04  mast
#  Provided option to enter path; kept track of chunk errors and machines that
#  gave chunk errors so that a bad chunk will not have to be run until it
#  each machine to drop; fixed order of tests for .csh gone versus CHUNK DONE
#  present; fixed test for all machines failed and no chunks worked
#
#  Revision 3.17  2005/09/08 00:14:50  mast
#  Made it take commands from a file since the interrupts don't work
#  from inside etomo
#
#  Revision 3.16  2005/08/30 22:03:13  mast
#  Kill remote jobs with background ssh calls that can time out
#
#  Revision 3.15  2005/08/27 22:27:16  mast
#  Added ability to enter machines to drop after interrupt
#
#  Revision 3.14  2005/08/26 23:11:23  mast
#  Fixed dumb bugs
#
#  Revision 3.13  2005/08/26 20:44:44  mast
#  Added CHUNK ERROR reports when there is a nonempty log
#
#  Revision 3.12  2005/08/24 16:10:50  mast
#  Implemented killing via calling a process group kill script, added a
#  sync message in case pid file was not written by nfs yet, switched to
#  bash --login and allowed exit code 145 from the w to get it to work on
#  our SGI
#
#  Revision 3.11  2005/07/19 00:19:01  mast
#  Switched to running bash -l -c and added PID output
#
#  Revision 3.10  2005/07/16 00:26:25  mast
#  Added failure counting, machine dropping, a pause option to let jobs finish,
#  exiting if all machines have failed and no chunks have finished, and
#  exiting if all machines have dropped out.
#
#  Revision 3.9  2005/06/30 18:13:03  mast
#  Added finished on machine message, changed failed message, reduce sleep
#
#  Revision 3.8  2005/06/28 23:33:20  mast
#  Made it work with spaces in the directory path
#
#  Revision 3.7  2005/05/12 23:11:08  mast
#  Fixed for windows, made .csh file before starting process
#
#  Revision 3.6  2005/05/12 21:24:09  mast
#  Added ability to run with sync files
#
#  Revision 3.5  2004/08/27 05:46:48  mast
#  Switched to using head -n and tail -n
#
#  Revision 3.4  2004/07/23 00:09:00  mast
#  Set default nice to 18 per user request
#
#  Revision 3.3  2004/07/21 17:00:27  mast
#  Output the all file list when it is used.
#
#  Revision 3.2  2004/06/29 03:41:48  mast
#  Added nohup
#
#  Revision 3.1  2004/06/29 02:47:09  mast
#  Initial addition to package
