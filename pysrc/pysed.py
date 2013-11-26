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
pysedReturnOnErr = False

def psReportErr(error):
   prnstr(error)
   if pysedReturnOnErr:
      return error
   sys.exit(1)

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
def pysed(sedregsIn, src, dstfile = None, nocase = False, delim = '/', retErr = False):
   """A module to take one or more sed regular expressions and apply them to 
the list of strings in src, or, if src is a single string, to all lines of the
file whose name is given in src.  The result is written 
to the file whose name is given in dstfile, if any, or returned as a list of
strings excluding line endings.  nocase can be used to have searches be
case-insensitive, and delim can be used to provide an alternate delimiter,
which is unneeded since this function will now handle escaped slashes.
sedregsIn can be a single string or a list of strings.  Setting retErr true will
make it print an string and return the string instead of exiting.
Handles constructs of the following form:
    s/pattern/replace/[g]
    /pattern/s//replace/[gp]
    /pattern/d
    /pattern/p
    /pattern/a/added line/
In other words, actions can be s, d, a, or p, and an expression can have a
g (global) or p (print) modifier.  If p is present on any expression, all
lines not matching a pattern with p will be deleted, as when using "sed -n".
The append action is completely non-standard to provide a convenient way to add
a line after a matched line.
As of IMOD 4.2.16, it will handle groups and parentheses in sed format by
converting \( and \) to ( and ) while escaping unescaped ( and ).  The replacement
reference can be in the form \\1 (\1 in a raw string) or \\g<1> to isolate from
numbers that follow.  As of IMOD 4.4.2, when constructing a pattern-matching
string, it will escape ^ except at the start of the string or after [, $
except at the end, and + anywhere in the string, for compatibility with sed.
"""
   global caretMatch, dollarMatch, pysedReturnError
   pysedReturnError = retErr

   # Set error prefix and try to open input file
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
         return psReportErr(fmtstr("{} Opening or reading from {}: {}", prefix, src,
                       sys.exc_info()[1]))
   else:
      srclines = src
   
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
   printind = []
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
         return psReportErr(fmtstr("{} Expression too short or too long: {}", prefix,
                                   sedregs[i]))

      # Initialize entries for this expression
      sea = None
      rep = None
      pat = None
      nsub = 1
      if (splt[0] == 's'):
         if len(splt) != 4 or (splt[3] and splt[3] != 'g'):
            return psReportErr(fmtstr('{} Incorrect s/// entry: {}', prefix, sedregs[i]))

         sea = swapParenCompile(splt[1], flags)
         rep = splt[2]
         act = 's'
         if splt[3] == "g" :
            nsub = 0

      elif splt[0] :
         return psReportErr(fmtstr("{} Only s can preceed patterns: {}", prefix,
                                   sedregs[i]))

      else:
         act = splt[2]
         if len(act) > 1 :
            return psReportErr(fmtstr("{} Action must be a single letter: {}", prefix,
                                      sedregs[i]))
         if act == 'd' or act == 'p':
            if len(splt) > 3 :
               return psReportErr(fmtstr("{} d or p must not be followed by pattern: {}",
                                         prefix, sedregs[i]))
            pat = swapParenCompile(splt[1], flags)
            if act == 'p' :
               printind.append(i)

         elif act == 'a':
            if len(splt) != 5 or splt[4] :
               return psReportErr(fmtstr("{} Incorrect 'a' entry: {}", prefix,
                                         sedregs[i]))
            pat = swapParenCompile(splt[1], flags)
            rep = splt[3]
            
         elif act == 's':
            if len(splt) < 6 :
               return psReportErr(fmtstr("{} Too few elements for s command: {}",
                                         prefix, sedregs[i]))
            rep = splt[4]
            for mod in splt[5] :
               if mod == 'g' :
                  nsub = 0
               elif mod == 'p' :
                  printind.append(i)
               else:
                  return psReportErr(fmtstr("{} Only p and g are allowed modifiers: {}",
                                            prefix, sedregs[i]))

            rep = splt[4]
            if not splt[3] :
               sea = swapParenCompile(splt[1], flags)
               if i in printind:
                  pat = sea
            else:
               sea = swapParenCompile(splt[3], flags)
               pat = swapParenCompile(splt[1], flags)

         else:
            return psReportErr(fmtstr("{} Only s, d, a, and p are allowed actions: {}", 
                                      prefix, sedregs[i]))
               
      pattern.append(pat)
      substr.append(sea)
      replace.append(rep)
      action.append(act)
      numsub.append(nsub)

   outlines = []
   for line in srclines:
      line = line.rstrip('\r\n')
      delete = len(printind) > 0
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
               if i in printind:
                  delete = 0
         else :
            line = substr[i].sub(replace[i], line, numsub[i])

      if not delete :
         outlines.append(line)
      for l in addlines:
         outlines.append(l)
      

   if dstfile :
      try:
         sedout = open(dstfile, 'w')
      except IOError:
         return psReportErr(fmtstr("{} Opening {}: {}", prefix, dstfile,
                                   sys.exc_info()[1]))
      try:
         for line in outlines:
            prnstr(line, file=sedout)
      except IOError:
         return psReportErr(fmtstr("{} Writing to output file", prefix))
      sedout.close()
      return None
   
   return outlines


# Two convenience routines that work if "option" is required to be at the beginning
# of a line and result in more readable code than direct conversion of sed commands

# Make two sed commands for removing an existing line and adding one with new value
def sedDelAndAdd(option, value, afterLine, delim = '/'):
   return [fmtstr('{0}^{1}{0}d', delim, option),
           fmtstr('{0}^{1}{0}a{0}{2}	{3}{0}', delim, afterLine, option, value)]

# Return a sed command (a string) for modifying the value for an option
def sedModify(option, value, delim = '/'):
   return fmtstr('{0}^{1}{0}s{0}[ 	].*{0}	{2}{0}', delim, option, value)
   
