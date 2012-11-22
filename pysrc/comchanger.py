#!/usr/bin/python
# comchanger.py module
# Functions for changing com files
#
# Author: David Mastronarde
#
#  $Id$

import re
from imodpy import *
from pysed import *

def modifyForChangeList(sedlines, srcRoot, setlet, changeList, returnOnErr = False):
   # Analyze the lines for command blocks
   procStarts = []
   procMatch = re.compile('^ *\$ *(\w+).*$')
   for ind in range(len(sedlines)):
      line = sedlines[ind]
      if re.search(procMatch, line):
         process = re.sub(procMatch, r'\1', line)
         procStarts.append((ind, process))
   numProcs = len(procStarts)
   procStarts.append((len(sedlines), ''))

   # Start output with any lines up to the first command
   outlines = []
   if procStarts[0][0]:
      outlines += sedlines[0:procStarts[0][0]]

   # Loop on the processes, for each one, run through the change lines and compile the
   # list of relevant changes
   for ind in range(numProcs):
      proclines = sedlines[procStarts[ind][0]: procStarts[ind + 1][0]]
      process = procStarts[ind][1]
      if process == 'if':
         outlines += proclines
         continue
      
      srcAxis = srcRoot + setlet
      procChanges = []
      for change in changeList:
         if (change[0] == srcAxis or change[0] == srcRoot) and change[1] == process:
            for ichan in range(len(procChanges)):
               if procChanges[ichan][2] == change[2]:

                  # If single axis, ignore a change that is B-specific
                  if setlet == '' and change[0] == srcRoot + 'b':
                     break
                  
                  # Otherwise, replace with a later entry unless previous one was
                  # axis-specific while this one is generic
                  if setlet == '' or not \
                         (procChanges[ichan][0] == srcAxis and change[0] == srcRoot):
                     procChanges[ichan] = change
                  break
            else:

               # If the loop does not break by replacement or ignoring, add the change
               procChanges.append(change)


      # build a sed command
      sedcom2 = []
      if procChanges:
         for change in procChanges:

            # Modify an existing line or append a new one after the process command
            # if there is a value
            if change[3] != '':
               for line in proclines:
                  if line.startswith(change[2]):
                     sedcom2.append(fmtstr('/^{}/s/[ 	].*/	{}/', change[2],
                                           change[3]))
                     break
               else:
                  sedcom2.append(fmtstr('/ *$ *{}/a/{}	{}/', process, change[2],
                                        change[3]))

            else:

               # Or delete the line if there is no value
               sedcom2.append('/^' + change[2] + '/d')

         tmplines = pysed(sedcom2, proclines, retErr = returnOnErr)
         if isinstance(tmplines, str):
            return tmplines
         outlines += tmplines

      else:
          outlines += proclines

   return outlines


def changeToAddToList(changeList, line, changeFile, prefix, numComponents, \
                         valNeeded = False):
   lsplit = line.split('=')
   keysplit = lsplit[0].split('.')
   errtext = 'entry: '
   if changeFile:
      errtext = 'line from ' + changeFile + ': '
   if keysplit[0] == prefix:
      if len(lsplit) < 2:
         exitError('Missing = sign in ' + errtext + line)
      if valNeeded and lsplit[1].strip() == '':
         exitError('Empty value in ' + errtext + line)
      if len(keysplit) < numComponents:
         exitError('Not enough components in ' + errtext + line)
      change = []
      for comp in keysplit[1:numComponents]:
         change.append(comp.strip())
      change.append(lsplit[1].strip())
      changeList.append(change)


