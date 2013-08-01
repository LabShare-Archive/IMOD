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
from pip import *

# Modify a set of command lines for relevant changes in a potentially large changeList
# comlines are the lines, comRoot is the root of the com file name exclusive of axis
# axisLet is a, b, or empty.
def modifyForChangeList(comlines, comRoot, axisLet, changeList, returnOnErr = False):

   # Analyze the lines for command blocks
   procStarts = []
   procMatch = re.compile('^ *\$ *(\w+).*$')
   for ind in range(len(comlines)):
      line = comlines[ind]
      if re.search(procMatch, line):
         process = re.sub(procMatch, r'\1', line)
         procStarts.append((ind, process))
   numProcs = len(procStarts)
   procStarts.append((len(comlines), ''))

   # Start output with any lines up to the first command
   outlines = []
   if procStarts[0][0]:
      outlines += comlines[0:procStarts[0][0]]

   # Loop on the processes, for each one, run through the change lines and compile the
   # list of relevant changes
   for ind in range(numProcs):
      proclines = comlines[procStarts[ind][0]: procStarts[ind + 1][0]]
      process = procStarts[ind][1]
      if process == 'if':
         outlines += proclines
         continue
      
      comAxis = comRoot + axisLet
      procChanges = []
      for change in changeList:
         if (change[0] == comAxis or change[0] == comRoot) and change[1] == process:
            for ichan in range(len(procChanges)):
               if procChanges[ichan][2] == change[2]:

                  # If single axis, ignore a change that is B-specific
                  if axisLet == '' and change[0] == comRoot + 'b':
                     break
                  
                  # Otherwise, replace with a later entry unless previous one was
                  # axis-specific while this one is generic
                  if axisLet == '' or not \
                         (procChanges[ichan][0] == comAxis and change[0] == comRoot):
                     procChanges[ichan] = change
                  break
            else:

               # If the loop does not break by replacement or ignoring, add the change
               procChanges.append(change)


      # build a sed command
      sedcom = []
      if procChanges:
         for change in procChanges:

            # Modify an existing line or append a new one after the process command
            # if there is a value
            if change[3] != '':
               for line in proclines:
                  if line.startswith(change[2]):
                     sedcom.append(fmtstr('|^{}|s|[ 	].*|	{}|', change[2],
                                           change[3]))
                     break
               else:
                  sedcom.append(fmtstr('|^ *$ *{}|a|{}	{}|', process, change[2],
                                        change[3]))

            else:

               # Or delete the line if there is no value
               sedcom.append('|^' + change[2] + '|d')

         tmplines = pysed(sedcom, proclines, delim = '|', retErr = returnOnErr)
         if isinstance(tmplines, str):
            return tmplines
         outlines += tmplines

      else:
          outlines += proclines

   return outlines


# Add one change to the list with the given prefix and number of components
# Store a change as a list of all components but the first and the value, which
# can be blank unless valNeed is true
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


# Process changes from two kinds of options, specified by fileOption for a file,
# and oneChangeOpt for an entry of one change, and look for changes starting with prefix
def processChangeOptions(fileOption, oneChangeOpt, prefix, numComponents = 4,
                         valNeeded = False):
   changeList = []
   if fileOption:
      numChangers = PipNumberOfEntries(fileOption)
      for chan in range(numChangers):
         changeFile = PipGetString(fileOption, '')
         changeLines = readTextFile(changeFile)
         for line in changeLines:
            changeToAddToList(changeList, line, changeFile, prefix, numComponents,
                              valNeeded)

   if oneChangeOpt:
      numChangers = PipNumberOfEntries(oneChangeOpt)
      for chan in range(numChangers):
         line = PipGetString(oneChangeOpt, '')
         changeToAddToList(changeList, line, '', prefix, numComponents, valNeeded)

   return changeList
