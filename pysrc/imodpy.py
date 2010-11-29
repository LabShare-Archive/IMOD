#!/usr/bin/python
# imodpy.py module
#
# Authors: Tor Mohling and David Mastronarde
#
# $Id$
# Log at end
#
 
"""A collection of useful functions for use in IMOD scripts

This module provides the following functions:
  runcmd(cmd[, input][,outfile]) - spawn a command with optional input, return
                                   its output or print to stdout or a file
  getmrcsize(file)     - run the 'header' command on <file>.
                         returns a triple of x,y,z size integers
  getmrc(file)         - run the 'header' command on <file>.
                         returns a 'tuple' of x,y,z,mode,px,py,pz
  makeBackupFile(file)   - renames file to file~, deleting old file~
  exitFromImodError(pn, errout) - prints the error strings in errout and
                                  prepends 'ERROR: pn - ' to the last one
  parselist(line)      -  convert list entry in <line> into list of integers
  readTextFile(filename[ , descrip])  - reads in text file, strips line endings
                                        returns a list of strings
  optionValue(linelist, option, type, ignorecase = False) - finds option value
                                                              in list of lines
"""

# other modules needed by imodpy
import sys, os, exceptions, re
from pip import exitError


# Use the subprocess module if available except on cygwin where broken
# pipes occurred occasionally; require it on win32
# but in 2.6 on cygwin is complains that this popen2 is deprecated, so try it
useSubprocess = True
pyversion = 100 * sys.version_info[0] + 10 * sys.version_info[1]
if (sys.platform.find('cygwin') < 0 and pyversion >= 240) or pyversion >= 260:
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
#   def __str__(self):
#      return repr(self.args)

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
   """getmrc(file)     - run the 'header' command on <file>
    
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


def makeBackupFile(filename):
   """makeBackupFile(file)   - rename file to file~, deleting old file~"""
   if os.path.exists(filename):
      backname = filename + '~'
      try:
         if os.path.exists(backname):
            os.remove(backname)
         os.rename(filename, backname)
      except:
         print 'WARNING: Failed to rename existing file %s to %s' % \
               (filename, backname)

def exitFromImodError(pn, errout):
   """exitFromImodError(pn, errout) - prints the error strings in errout
      and prepends 'ERROR: pn - ' to the last one"""
   line = None
   for l in errout:
      if line:
         print line,
      line = l
   print "ERROR: " + pn + " - " + line,
   sys.exit(1)


def parselist (line):
   """parselist(line)   - convert list entry in <line> into list of integers
 
   Returns the list of integers, or None for an error. An example of a list is
   1-5,7,9,11,15-20.  Numbers separated by dashes are replaced by
   all of the numbers in the range.  Numbers need not be in any order,
   and backward ranges (10-5) are handled.  Only comma and space
   are valid separators.  A / at the beginning of the string will
   return an error.  Negative numbers can be entered provided
   that the minus sign immediately precedes the number.  E.g.: -3 - -1
   or -3--1 will give -3,-2,-1; -3, -1,1 or -3,-1,1 will give -3,-1,1. """

   list = [];
   dashlast = False;
   negnum = False;
   gotcomma = False;
   nchars = len(line);

   if not nchars:
      return list
   if (line[0] == '/'):
      return None
   nlist = 0;
   ind = 0;
   lastnum = 0;

   #   find next digit and look for '-', but error out on non -,space
   while (ind < nchars):
      next = line[ind];
      if (next.isdigit()):

         #   got a digit: save ind, find next non-digit
         numst = ind;
         while (1):
            ind += 1
            next = ''
            if (ind >= nchars):
               break
            next = line[ind]
            if (not next.isdigit()):
               break

         # Convert the number
         if (negnum):
            numst -= 1
         number = int(line[numst:ind])

         # set up loop to add to list
         loopst = number;
         idir = 1;
         if (dashlast):
            if (lastnum > number):
               idir = -1;
            loopst = lastnum + idir
            
         
         i = loopst
         while(idir * i <= idir * number):
            list.append(i)
            nlist += 1
            i += idir

         lastnum = number;
         negnum = False;
         dashlast = False;
         gotcomma = False;
         continue;
   
      if (next != ',' and next != ' ' and next != '-'):
         return None
      if (next == ','):
         gotcomma = True;
      if (next == '-'):
         if (dashlast or (ind == 0) or gotcomma):
            negnum = True;
         else:
            dashlast = True;
   
      ind += 1

   return list;

# Function to read in a text file and strip line endings
def readTextFile(filename, descrip = None):
   """readTextFile(filename[ , descrip])  - read in text file, strip endings

   Reads in the text file in <filename>, strips the line endings and blanks
   from the end of each line, and returns a list of strings.  Exits on an
   error opening or reading the file, and adds the optional description in
   <descrip> to the error message
   """
   if not descrip:
      descrip = " "
   try:
      errString = "Opening"
      textfile = open(filename, 'r')
      errString = "Reading"
      lines = textfile.readlines()
   except IOError, (errno, strerror):
      exitError("%s %s %s: %s" % (errString, descrip, filename, strerror))
   except:
      exitError("%s %s %s: %s" % (errString, descrip, filename, \
                           sys.exc_info()[0]))
   textfile.close()
   for i in range(len(lines)):
      lines[i] = lines[i].rstrip(' \t\r\n')
   return lines

# Function to find an option value from a list of strings
def optionValue(linelist, option, type, ignorecase = False):
   """optionValue(linelist, option, type[, nocase] - find option value in strings

   Given a list of strings in <linelist>, searches for one(s) that contain the
   text in <option> and that are not commented out.  The search is
   case-insensitive if optional argument <nocase> is supplied and is not False
   or None.  The return value depends on the value of <type>:
   0 : a string with white space stripped and a comment removed from the end
   1 : an array of integers
   2 : an array of floats
   3 : a boolean value, True or false
   It returns None if the option is not found or if its value is empty or
   contains inappropriate characters; in the latter case it issues a WARNING:.
   If the option occurs more than once, the latest value applies.
   """
   flags = 0
   if ignorecase:
      flags = re.IGNORECASE
   optre = re.compile(option, flags)
   subre = re.compile('.*' + option + r'[^\s]*([^#]*).*', flags)
   comre = re.compile(r'\s*#\s*' + option, flags)
   retval = None
   for line in linelist:
      if optre.search(line) and not comre.search(line):
         valstr = subre.sub(r'\1', line).strip()
         if type > 2:
            bval = valstr.lower()
            if bval == '0' or bval == 'f' or bval == 'off' or bval == 'false':
               retval = False
            elif bval == '1' or bval == 't' or bval == 'on' or bval == 'true' \
                   or bval == "":
               retval = True
            else:
               print "WARNING: optionValue - Boolean entry found with " + \
                     "improper value in: " + line
               
         elif valstr == "":
            print "WARNING: optionValue - No value for option in: " + line
         elif type <= 0:
            retval = valstr
         else:
            retval = []
            splits = valstr.split()
            try:
               for val in splits:
                  if type == 1:
                     retval.append(int(val))
                  else:
                     retval.append(float(val))
            except:
               print "WARNING: optionValue - Bad character in numeric " + \
                     + "entry in: " + line
               
   return retval


#  $Log$
#  Revision 1.6  2010/02/22 06:22:06  mast
#  Added optionValue
#
#  Revision 1.5  2009/10/22 05:46:30  mast
#  Add readTextFile
#
#  Revision 1.4  2008/01/05 17:21:17  mast
#  Added parselist since pip allows lists to be read
#
#  Revision 1.3  2006/10/03 14:50:23  mast
#  Added exit function to print lines from command error
#
#  Revision 1.2  2006/10/01 13:36:41  mast
#  Added backup file function
#
#  Revision 1.1  2006/09/26 23:02:48  mast
#  Added to package
#
#
