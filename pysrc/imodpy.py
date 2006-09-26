#!/usr/bin/python
# imodpy.py module
#
# Authors: Tor Mohling and David Mastronarde
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#
 
"""A collection of useful functions for use in IMOD scripts

This module provides the following functions:
  runcmd(cmd[, input][,outfile]) - spawn a command with optional input, return
                                   its output or print to stdout or a file
  getmrcsize(file)     - run the 'header' command on <file>.
                         returns a triple of x,y,z size integers
  getmrc(file)         - run the 'header' command on <file>.
                         returns a 'tuple' of x,y,z,mode,px,py,pz
"""

# other modules needed by imodpy
import sys, os, exceptions


# Use the subprocess module if available except on cygwin where broken
# pipes occurred occasionally; require it on win32
useSubprocess = True
if sys.platform.find('cygwin') < 0 and (sys.version_info[0] > 2 or \
       (sys.version_info[0] == 2  and sys.version_info[1] > 3)):
    from subprocess import *
else:
    useSubprocess = False
    if sys.platform.find('win32') >= 0:
        print "ERROR: imodpy - Windows Python must be at least version 2.4"
        sys.exit(1)

# define a new exception class so our caller can more easily catch errors
# raised here
class ImodpyError(exceptions.Exception):
    def __init__(self, args=None):
        self.args = args
#    def __str__(self):
#        return repr(self.args)

# output_lines = imodpy.runcmd(cmd, input_lines, output_file)
#  output_lines, input_lines are ARRAYs; output_lines is None if output_file
# not None
#  cmd is a STRING
#
def runcmd(cmd, input=None, outfile=None):
    """runcmd(cmd[, input][, outfile])
         - spawn a command with optional input, either send its output to
         outfile or return its output in an array of strings.
         cmd is a string; input is an array; outfile is a file object or
         the string 'stdout' to send output to standard out"""

    # Set up flags for whether to collect output or send to stderr
    collect = 1
    toStdout = 0
    output = None
    if outfile:
        collect = 0
        if isinstance(outfile, str) and outfile == 'stdout':
            toStdout = 1

    try:
        if useSubprocess:

            # The subprocess interface: input must be all one string
            if input:
                if isinstance(input, list):
                    joined = ''
                    for l in input:
                        joined += l + '\n'
                    input = joined
            else:
                input = str(None)

            # Run it three different ways depending on where output goes
            if collect:
                p = Popen(cmd, shell=True, stdout=PIPE, stdin=PIPE)
                kout, kerr = p.communicate(input)
                if kout:
                    output = kout.splitlines(1)

            elif toStdout:
                print 'running Popen(' + cmd
                p = Popen(cmd, shell=True, stdin=PIPE)
                p.communicate(input)
            else:
                p = Popen(cmd, shell=True, stdout=outfile, stdin=PIPE)
                p.communicate(input)
            ec = p.returncode
            
        else:

            # The old way, use popen or popen2 depending on where the output
            # is going
            if toStdout:
                kin = os.popen(cmd, 'w')
                if input:
                    for l in input:
                        print >> kin, l
                ec = kin.close()
            else:
                (kin, kout) = os.popen2(cmd)
                if input:
                    for l in input:
                        print >> kin, l
                kin.close()
                output = kout.readlines()
                kout.close()
                kpid, ec = os.wait()
                if not collect and output:
                    outfile.writelines(output)
                
    except:
        err = "command " + cmd + ": " + str(sys.exc_info()[0]) + "\n"
        raise ImodpyError, [err]

    if ec:
        # look thru the output for 'ERROR' line(s) and put them before this
        errstr = cmd + ": exited with status %d %d\n" % ((ec >> 8), (ec & 255))
        err = []
        if collect and output:
            err = [l for l in output
                   if l.find('ERROR:') >= 0]
        err.append(errstr)
        raise ImodpyError, err

    if collect:
        return output
    return None

def getmrc(file):
    """getmrc(file)      - run the 'header' command on <file>
    
    Returns a tuple with seven elements (x,y,z,mode,px,py,pz)
    (x,y,z are size in int, mode is int, px,py,pz are pixelsize in float."""

    input = ["InputFile " + file]
    hdrout = runcmd("header -si -mo -pi -StandardInput", input)

    if len(hdrout) < 3:
        err = "header " + file + ": too few lines of output"
        raise ImodpyError, [err]

    nxyz = hdrout[0].split()
    pxyz = hdrout[2].split()
    if len(nxyz) < 3 or len(pxyz) < 3:
        err = "header " + file + ": too few numbers on lines"
        raise ImodpyError, [err]
    ix = int(nxyz[0])
    iy = int(nxyz[1])
    iz = int(nxyz[2])
    mode = int(hdrout[1])
    px = float(pxyz[0])
    py = float(pxyz[1])
    pz = float(pxyz[2])
    
    return (ix,iy,iz,mode,px,py,pz)


def getmrcsize(file):
    """getmrcsize(file)    - run the 'header' command on <file>
    
    Returns a triple of x,y,z size integers."""
    (ix,iy,iz,mode,px,py,pz) = getmrc(file)
    return(ix,iy,iz)


