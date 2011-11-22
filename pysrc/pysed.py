#!/usr/bin/python
# pysed.py module
#
# Author: David Mastronarde
#
# $Id$
#

import sys, re
from imodpy import *
escslash = '#escSlash^'

# A function to swap \( and (, and \) and ), and take care of escaping other characters,
# before compiling a regexp
def swapParenCompile(pattern, flags):
   global caretMatch, dollarMatch

   # Escape any ^ except one at the start or after [, and any $ except one at the end
   # Also escape any + because that is not supported by sed
   pat = pattern
   pat = re.sub(caretMatch, r'\1\\^', pattern)
   pat = re.sub(dollarMatch, r'\\$\1', pat)
   pat = pat.replace('+', '\+')
   if pat.find('(') >= 0:
      pat = pat.replace('\(', escslash)
      pat = pat.replace('(', '\(')
      pat = pat.replace(escslash, '(')
      # print 'replacement 1:', pattern, pat

   if pat.find(')') >= 0:
      pat = pat.replace('\)', escslash)
      pat = pat.replace(')', '\)')
      pat = pat.replace(escslash, ')')
      # print 'replacement 2:', pattern, pat

   return re.compile(pat, flags)

# The main routine
def pysed(sedregsIn, src, dstfile = None, nocase = False, delim = '/'):
   """A module to take one or more sed regular expressions and apply them to 
the list of strings in src, or, if src is a single string, to all lines of the
file whose name is given in src.  The result is written 
to the file whose name is given in dstfile, if any, or returned as a list of
strings excluding line endings.  nocase can be used to have searches be
case-insensitive, and delim can be used to provide an alternate delimiter,
which is unneeded since this function will now handle escaped slashes.
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
As of IMOD 4.2.16, it will handle groups and parentheses in sed format by
converting \( and \) to ( and ) while escaping unescaped ( and ).  The replacement
reference can be in the form \\1 (\1 in a raw string) or \\g<1> to isolate from
numbers that follow.  As of IMOD 4.4.2, when constructing a pattern-matching
string, it will escape ^ except at the start of the string or after [, $
except at the end, and + anywhere in the string, for compatibility with sed.
"""
   global caretMatch, dollarMatch

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
      except IOError:
         prnstr(fmtstr("{} Opening or reading from {}: {}", prefix, src,
                       sys.exc_info()[1]))
         sys.exit(1)
   else:
      srclines = src
   

   if (dstfile) :
      try:
         sedout = open(dstfile, 'w')
      except IOError:
         prnstr(fmtstr("{} Opening {}: {}", prefix, dstfile, sys.exc_info()[1]))
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
   caretMatch = re.compile(r'([^\\\[])\^')
   dollarMatch = re.compile(r'\$(.)')

   # Loop on expressions, parsing them for action, pattern, etc
   for i in range(len(sedregs)):
      if delim == '/':
         line = sedregs[i].replace('\/', escslash)
         splt = line.split(delim)
         for j in range(len(splt)):
            splt[j] = splt[j].replace(escslash, '/')
      else:
         splt = sedregs[i].split(delim)
      if len(splt) < 3 or len(splt) > 6:
         prnstr(fmtstr("{} Expression too short or too long: {}", prefix, sedregs[i]))
         sys.exit(1)

      # Initialize entries for this expression
      sea = None
      rep = None
      pat = None
      nsub = 1
      if (splt[0] == 's'):
         if len(splt) != 4 or (splt[3] and splt[3] != 'g'):
            prnstr(fmtstr('{} Incorrect s/// entry: {}', prefix, sedregs[i]))
            sys.exit(1)

         sea = swapParenCompile(splt[1], flags)
         rep = splt[2]
         act = 's'
         if splt[3] == "g" :
            nsub = 0

      elif splt[0] :
         prnstr(fmtstr("{} Only s can preceed patterns: {}", prefix,
                       sedregs[i]))
         sys.exit(1)

      else:
         act = splt[2]
         if len(act) > 1 :
            prnstr(fmtstr("{} Action must be a single letter: {}", prefix, sedregs[i]))
            sys.exit(1)
         if act == 'd' or act == 'p':
            if len(splt) > 3 :
               prnstr(fmtstr("{} d or p must not be followed by pattern: {}", \
                             prefix, sedregs[i]))
               sys.exit(1)
            pat = swapParenCompile(splt[1], flags)
            if act == 'p' :
               printind = i

         elif act == 'a':
            if len(splt) != 5 or splt[4] :
               prnstr(fmtstr("{} Incorrect 'a' entry: {}", prefix, sedregs[i]))
               sys.exit(1)
            pat = swapParenCompile(splt[1], flags)
            rep = splt[3]
            
         elif act == 's':
            if len(splt) < 6 :
               prnstr(fmtstr("{} Too few elements for s command: {}", prefix, sedregs[i]))
               sys.exit(1)
            rep = splt[4]
            for mod in splt[5] :
               if mod == 'g' :
                  nsub = 0
               elif mod == 'p' :
                  printind = i
               else:
                  prnstr(fmtstr("{} Only p and g are allowed modifiers: {}", \
                                prefix, sedregs[i]))
                  sys.exit(1)

            rep = splt[4]
            if not splt[3] :
               sea = swapParenCompile(splt[1], flags)
               if printind == i :
                  pat = sea
            else:
               sea = swapParenCompile(splt[3], flags)
               pat = swapParenCompile(splt[1], flags)

         else:
            prnstr(fmtstr("{} Only s, d, a, and p are allowed actions: {}", \
                          prefix, sedregs[i]))
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
            prnstr(line, file=sedout)
      except:
         prnstr(fmtstr("{} Writing to output file", prefix))
         sys.exit(1)
      sedout.close()
      return None
   return outlines
