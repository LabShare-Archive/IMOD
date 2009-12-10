#!/usr/bin/python
# supermont.py module
# Functions for working with supermontage data
#
# Author: David Mastronarde
#
#  $Id$
#  Log at end

import re, sys, os, os.path
from imodpy import *

# define key names as variables
kPiece = 'Piece'
kFrame = 'Frame'
kSize = 'size'
kZlimit = 'Zlimits'
kModel = 'Model'
kEdge = 'Edge'
kShift = 'shift'
kLower = 'lower'
kXorY = 'XorY'
kShiftDone = 'shiftDone'
kPatch = 'patches'
kReduce = 'reducePatch'
kResid = 'residPatch'
kRedEdit = 'reduceEdited'
kResEdit = 'residEdited'
kMatxf = 'matxf'
kNoZvals = 'noZvals'
kSlice = 'Section'
kZvalue = 'zvalue'
kSpacing = 'spacing'
kOutsize = 'outsize' 
kSample = 'intervalFactor'
kVectors = 'vectors'
kXstacked = 'xStacked'
kYstacked = 'yStacked'
kGoodlim = 'goodLimits'
kPatchParam = 'patchParams'

smpref = "ERROR: readMontInfo - "
backedUp = False


def setSMErrorPrefix(prefix):
   global smpref
   smpref = prefix

# Remove an extension and add a new one
def changeExtension(name, newext):
   ind = name.rfind('.')
   if ind < 0:
      ind = len(name)
   return name[0:ind] + newext

# Function to convert a value to an array of numbers
def convertValues(line, value, numVal, func, defaultLast):
   value = re.sub(' +', ' ', value.strip()).split(' ')
   if len(value) < numVal - 1 or (not defaultLast and len(value) < numVal):
      print "%s Too few values in entry: %s" % (smpref, line)
      sys.exit(1)
   retval = []
   for i in range(min(numVal, len(value))):
      retval.append(func(value[i]))
   if len(value) < numVal:
      retval.append(0)
   return retval

# Function to check for duplicate entries under a key
def checkDuplicate(dicty, key, message):
   if dicty.has_key(key):
      print "%s More than one entry for %s in %s" % (smpref, key, message)
      sys.exit(1)

# Function to update a patch file from a model as long as the model has fewer
# patches
def updatePatchFromModel(patchname, modelname):
   retval = 0
   if os.path.exists(modelname):
      tmpname = changeExtension(patchname, ".tmppatch")
      try:
         runcmd('imod2patch ' + modelname + ' ' + tmpname, None)
      except ImodpyError, errout:
         exitFromImodError(progname, errout)

      try:
         patchfile = open(patchname, 'r')
         patchline = patchfile.readline()
         patchfile.close()
      except:
         print "Opening or reading from " + patchname
         sys.exit(1)

      try:
         tmpfile = open(tmpname, 'r')
         tmpline = tmpfile.readline()
         tmpfile.close()
      except:
         print "Opening or reading from " + tmpname
         sys.exit(1)

      # Use split without an argument to throw away separators
      numstr = patchline.split()
      oldnum = int(numstr[0])
      numstr = tmpline.split()
      newnum = int(numstr[0])
      if (newnum < oldnum):
         makeBackupFile(patchname)
         try:
            os.rename(tmpname, patchname)
         except:
            print "Renaming " + tmpname + " to " + patchname
            sys.exit(1)
            
         print "Replaced %s from model: the model has %d fewer vectors" %\
               (patchname, oldnum - newnum)
         retval = 1
      else:
         os.remove(tmpname)

   return retval


def readMontInfo(filename, predata, slices, pieces, edges):
   gotSection = 0
   inPiece = 0
   inEdge = 0
   inSlice = 0
   try:
      infile = open(filename)
   except IOError, (errno, strerror):
      print "%s Opening %s: %s" % (smpref, filename, strerror)
      sys.exit(1)
   except:
      print "%s Opening %s: %s" % (smpref, filename, sys.exc_info()[0])
      sys.exit(1)
      
   secMatch = re.compile('^\[(\S+) *= *(.*\S) *\]')
   keyMatch = re.compile('^(\S+) *= *(.*\S) *$')
   nozvals = "0"
   if predata.has_key(kNoZvals):
      nozvals = predata[kNoZvals] 
   for line in infile.readlines():
      line = re.sub('[\r\n]', '', line)
      if re.search(secMatch, line):
         gotSection = 1
         section = re.sub(secMatch, '\\1', line)
         value = re.sub(secMatch, '\\2', line).strip()

         # Test for whether conditions are met in section just entered
         if inPiece and not piece.has_key(kFrame):
            print "%s Piece %s must have a %s entry" % \
                  (smpref, piece["file"], kFrame)
            sys.exit(1)
         if inEdge and not (edge.has_key(kShift) and edge.has_key(kLower)\
            and edge.has_key(kXorY)):
            print "%s Edge %s is missing %s, %s, or %s entry" % \
                 (smpref, edge["name"], kLower, kXorY, kShift)
            sys.exit(1)
         if inSlice and not slice.has_key(kZvalue):
            print "%s Section %s must have a %s entry" % \
                 (smpref, slice["name"], kZvalue)
            sys.exit(1)

         inPiece = 0
         inEdge = 0
         inSlice = 0
         if section == kPiece:
            piece = { }
            pieces.append(piece)
            inPiece = 1
            piece["file"] = value

         elif section == kEdge:
            edge = { }
            edges.append(edge)
            edge["name"] = value;
            inEdge = 1

         elif section == kSlice:
            slice = { }
            slices.append(slice)
            slice["name"] = value;
            inSlice = 1

      # Not a new section, add key-value to section
      elif re.match(keyMatch, line):
         key = re.sub(keyMatch, '\\1', line)
         value = re.sub(keyMatch, '\\2', line).strip()
         if inPiece:
            checkDuplicate(piece, key, 'piece' + piece['file'])
            if key == kFrame:
               piece[key] = convertValues(line, value, 3, int, \
                  nozvals == '1')
            elif key == kSize:
               piece[key] = convertValues(line, value, 3, int, 0)
            elif key == kZlimit:
               piece[key] = convertValues(line, value, 2, int, 0)
            else:
               piece[key] = value

         elif inEdge:
            checkDuplicate(edge, key, 'edge' + edge['name'])
            if key == kShift:            
               edge[key] = convertValues(line, value,3, float, False)
            elif key == kLower:
               edge[key] = convertValues(line, value, 3, int, False)
            elif key == kGoodlim or key == kZlimit:
               edge[key] = convertValues(line, value, 2, int, False)
            else:
               edge[key] = value

         elif inSlice:
            checkDuplicate(slice, key, 'section' + slice['name'])
            if key == kOutsize:
               slice[key] = convertValues(line, value, 3, int, False)
            elif key == kSpacing or key == kXstacked or key == kYstacked:
               slice[key] = convertValues(line, value, 2, int, False)
            else:
               slice[key] = value

         elif not gotSection:
            predata[key] = value
            if key == kNoZvals:
               nozvals = value
            else:
               checkDuplicate(predata, key, "global data")
            

def writeMontInfo(filename, predata, slices, pieces, edges):
   global backedUp
   if not backedUp:
      makeBackupFile(filename)
      backedUp = True
   try:
      out = open(filename, 'w')
   except:
      print >> out, '%s Opening output file %s' % (smpref, filename)
      sys.exit(1)
   
   for key in predata.keys():
      print >> out, key + ' = ' + predata[key]

   print >> out, ''

   for slice in slices:
      print >> out, '[Section = %s]' % slice['name']
      for key in slice.keys():
         val = slice[key]
         if key == 'name':
            pass
         elif key == kOutsize:
            print >> out, '%s = %d %d %d' % (key, val[0], val[1], val[2])
         elif key == kSpacing or key == kXstacked or key == kYstacked:
            print >> out, '%s = %d %d' % (key, val[0], val[1])
         else:
            print >> out, '%s = %s' % (key, val)

   print >> out, ''

   for piece in pieces:
      print >> out, '[Piece = %s]' % piece['file']
      for key in piece.keys():
         val = piece[key]
         if key == 'file':
            pass
         elif key == kFrame or key == kSize:
            print >> out, '%s = %d %d %d' % (key, val[0], val[1], val[2])
         elif key == kZlimit:
            print >> out, '%s = %d %d' % (key, val[0], val[1])
         else:
            print >> out, '%s = %s' % (key, val)

   print >> out, ''

   for edge in edges:
      print >> out, '[Edge = %s]' % edge['name']
      for key in edge.keys():
         val = edge[key]
         if key == 'name':
            pass
         elif key == kLower:
            print >> out, '%s = %d %d %d' % (key, val[0], val[1], val[2])
         elif key == kShift:
            print >> out, '%s = %f %f %f' % (key, val[0], val[1], val[2])
         elif key == kZlimit:
            print >> out, '%s = %d %d' % (key, val[0], val[1])
         else:
            print >> out, '%s = %s' % (key, val)

# Function to get min and max of pieces, and list of Z values
def montMinMax(pieces):
   xmin, ymin, zmin = 10000, 10000, 10000
   xmax, ymax, zmax = -10000, -10000, -10000
   zlist = []
   for piece in pieces:
      xmin = min(xmin, piece[kFrame][0])
      xmax = max(xmax, piece[kFrame][0])
      ymin = min(ymin, piece[kFrame][1])
      ymax = max(ymax, piece[kFrame][1])
      zmin = min(zmin, piece[kFrame][2])
      zmax = max(zmax, piece[kFrame][2])
      if not  piece[kFrame][2] in zlist:
         zlist.append( piece[kFrame][2])
   return xmin, xmax, ymin, ymax, zmin, zmax, zlist

#
#  $Log$
#  Revision 1.4  2009/09/08 23:28:29  mast
#  Added some keys, added extension function, made it backup file only once
#
#  Revision 1.3  2008/12/18 06:24:00  mast
#  Fixed problem reading Frame with only 2 numbers
#
#  Revision 1.2  2007/04/11 15:45:25  mast
#  Added Z limits for edges, wrote out Z limits for pieces properly
#
#  Revision 1.1  2007/04/08 16:23:57  mast
#  Added to package
#

