#!/usr/bin/python
# pysed.py module
#
# Author: David Mastronarde
#
# $Id$
#
#  $Log$
#  Revision 1.2  2009/10/22 05:47:22  mast
#  Fixed writing to file
#
#  Revision 1.1  2006/09/26 23:02:48  mast
#  Added to package
#
#

import sys, re

def pysed(sedregsIn, src, dstfile = None, nocase = False, delim = '/'):
   """A module to take one or more sed regular expressions and apply them to 
the list of strings in src, or, if src is a single string, to all lines of the
file whose name is given in src.  The result is written 
to the file whose name is given in dstfile, if any, or returned as a list of
strings including line endings.  nocase can be used to have searches be
case-insensitive, and delim can be used to provide an alternate 
delimiter, since this function will not handle escaped slashes.
sedregsIn can be a single string or a list of strings.

Handles constructs of the following form:
    s/pattern/replace/[g]
    /pattern/s//replace/[gp]
    /pattern/s//replace/[gp]
    /pattern/d
    /pattern/p
    /pattern/a/added line/
In other words, actions can be s, d, a, or p, and an expression can have a
g (global) or p (print) modifier.
The append action is completely non-standard to provide a convenient way to add
a line after a matched line.
Will not yet handle groups and parentheses, where \( and \) need to be 
converted to ( and ), ( and ) need to be converted to \( and \), and 
\1 in the replacement string needs to be converted to \\1
"""

   # Set error prefix and try to open input and output files
   try:
      prefix = "ERROR: " + progname + " -"
   except:
      prefix = "ERROR: pysed -"
   if isinstance(src, str):
      try:
         sedin = open(src)
         srclines = sedin.readlines()
         sedin.close()
      except IOError, (errno, strerror):
         print "%s Opening or reading from %s: %s" % (prefix, src, strerror)
         sys.exit(1)
      except:
         print "%s Opening or reading from %s: %s" % (prefix, src, \
                                                      sys.exc_info()[0])
         sys.exit(1)
   else:
      srclines = src
   

   if (dstfile) :
      try:
         sedout = open(dstfile, 'w')
      except IOError, (errno, strerror):
         print "%s Opening %s: %s" % (prefix, dstfile, strerror)
         sys.exit(1)
      except:
         print "%s Opening %s: %s" % (prefix, dstfile, sys.exc_info()[0])
         sys.exit(1)

   if isinstance(sedregsIn, str):
      sedregs = (sedregsIn, )
   else:
      sedregs = sedregsIn
   
   # Initialize lists to be built up
   action = []
   numsub = []
   pattern = []
   substr = []
   replace = []
   printind = -1
   flags = 0
   if nocase:
      flags = re.IGNORECASE

   # Loop on expressions, parsing them for action, pattern, etc
   for i in range(len(sedregs)):
      splt = sedregs[i].split(delim)
      if len(splt) < 3 or len(splt) > 6:
         print "%s Expression too short or too long: %s" % \
             (prefix, sedregs[i])
         sys.exit(1)

      # Initialize entries for this expression
      sea = None
      rep = None
      pat = None
      nsub = 1
      if (splt[0] == 's'):
         if len(splt) != 4 or (splt[3] and splt[3] != 'g'):
            print '%s Incorrect s/// entry: %s' % (prefix, sedregs[i])
            sys.exit(1)

         sea = re.compile(splt[1], flags)
         rep = splt[2]
         act = 's'
         if splt[3] == "g" :
            nsub = 0

      elif splt[0] :
         print "%s Only s can preceed patterns: %s" % (prefix, sedregs[i])
         sys.exit(1)

      else:
         act = splt[2]
         if len(act) > 1 :
            print "%s Action must be a single letter: %s" % \
                 (prefix, sedregs[i])
            sys.exit(1)
         if act == 'd' or act == 'p':
            if len(splt) > 3 :
               print "%s d or p must not be followed by pattern: %s" % \
                    (prefix, sedregs[i])
               sys.exit(1)
            pat = re.compile(splt[1], flags)
            if act == 'p' :
               printind = i

         elif act == 'a':
            if len(splt) != 5 or splt[4] :
               print "%s Incorrect 'a' entry: %s" % (prefix, sedregs[i])
               sys.exit(1)
            pat = re.compile(splt[1], flags)
            rep = splt[3]
            
         elif act == 's':
            if len(splt) < 6 :
               print "%s Too few elements for s command: %s" % \
                   (prefix, sedregs[i])
               sys.exit(1)
            rep = splt[4]
            for mod in splt[5] :
               if mod == 'g' :
                  nsub = 0
               elif mod == 'p' :
                  printind = i
               else:
                  print "%s Only p and g are allowed modifiers: %s" % \
                      (prefix, sedregs[i])
                  sys.exit(1)

            rep = splt[4]
            if not splt[3] :
               sea = re.compile(splt[1], flags)
               if printind == i :
                  pat = sea
            else:
               sea = re.compile(splt[3], flags)
               pat = re.compile(splt[1], flags)

         else:
            print "%s Only s, d, a, and p are allowed actions: %s" % \
                (prefix, sedregs[i])
            sys.exit(1)
               
      pattern.append(pat)
      substr.append(sea)
      replace.append(rep)
      action.append(act)
      numsub.append(nsub)

   outlines = []
   for line in srclines:
      line = line.rstrip('\r\n')
      delete = printind >= 0
      addlines = []
      for i in range(len(sedregs)):
         if pattern[i] :
            if pattern[i].search(line) :
               if action[i] == 's' :
                  line = substr[i].sub(replace[i], line, numsub[i])
               elif action[i] == 'd' :
                  delete = 1
               elif action[i] == 'a' :
                  addlines.append(replace[i])
               if printind == i :
                  delete = 0
         else :
            line = substr[i].sub(replace[i], line, numsub[i])

      if not delete :
         outlines.append(line)
      for l in addlines:
         outlines.append(l)
      

   if dstfile :
      try:
         for line in outlines:
            print >> sedout, line
      except:
         print "%s Writing to output file" % prefix
         sys.exit(1)
      sedout.close()
      return None
   return outlines
