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
kEcdStub = 'ecdStub'
kBlendShift = 'blendShift'
kOrigShift = 'originalShift'
kLastShift = 'lastCorrShift'

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
   value = value.split()
   if len(value) < numVal - 1 or (not defaultLast and len(value) < numVal):
      prnstr(fmtstr("{} Too few values in entry: {}", smpref, line))
      sys.exit(1)
   retval = []
   for i in range(min(numVal, len(value))):
      retval.append(func(value[i]))
   if len(value) < numVal:
      retval.append(0)
   return retval

# Function to check for duplicate entries under a key
def checkDuplicate(dicty, key, message):
   if key in dicty:
      prnstr(fmtstr("{} More than one entry for {} in {}", smpref, key,
                    message))
      sys.exit(1)

# Function to update a patch file from a model as long as the model has fewer
# patches
def updatePatchFromModel(patchname, modelname):
   retval = 0
   if os.path.exists(modelname):
      tmpname = changeExtension(patchname, ".tmppatch")
      try:
         runcmd('imod2patch ' + modelname + ' ' + tmpname, None)
      except ImodpyError:
         exitFromImodError(progname)

      try:
         patchfile = open(patchname, 'r')
         patchline = patchfile.readline()
         patchfile.close()
      except:
         prnstr("Opening or reading from " + patchname)
         sys.exit(1)

      try:
         tmpfile = open(tmpname, 'r')
         tmpline = tmpfile.readline()
         tmpfile.close()
      except:
         prnstr("Opening or reading from " + tmpname)
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
            prnstr("Renaming " + tmpname + " to " + patchname)
            sys.exit(1)
            
         prnstr(fmtstr("Replaced {} from model: the model has {} fewer" +\
                       " vectors", patchname, oldnum - newnum))
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
   except IOError:
      prnstr(fmtstr("{} Opening {}: {}", smpref, filename, sys.exc_info()[1]))
      sys.exit(1)
      
   secMatch = re.compile('^\[(\S+)\s*=\s*(.*\S)\s*\]')
   keyMatch = re.compile('^(\S+)\s*=\s*(.*\S)\s*$')
   nozvals = "0"
   if kNoZvals in predata:
      nozvals = predata[kNoZvals] 
   for line in infile.readlines():
      line = re.sub('[\r\n]', '', line)
      if re.search(secMatch, line):
         gotSection = 1
         section = re.sub(secMatch, '\\1', line)
         value = re.sub(secMatch, '\\2', line).strip()

         # Test for whether conditions are met in section just entered
         if inPiece and not kFrame in piece:
            prnstr(fmtstr("{} Piece {} must have a {} entry", 
                  smpref, piece["file"], kFrame))
            sys.exit(1)
         if inEdge and not (kShift in edge and kLower in edge\
            and kXorY in edge):
            prnstr(fmtstr("{} Edge {} is missing {}, {}, or {} entry", \
                 smpref, edge["name"], kLower, kXorY, kShift))
            sys.exit(1)
         if inSlice and not kZvalue in slice:
            prnstr(fmtstr("{} Section {} must have a {} entry", \
                 smpref, slice["name"], kZvalue))
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
            if key == kShift or key == kOrigShift:
               edge[key] = convertValues(line, value, 3, float, False)
            elif key == kLower:
               edge[key] = convertValues(line, value, 3, int, False)
            elif key == kGoodlim or key == kZlimit:
               edge[key] = convertValues(line, value, 2, int, False)
            elif key == kBlendShift or key == kLastShift:
               edge[key] = convertValues(line, value, 2, float, False)
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
      prnstr(fmtstr('{} Opening output file {}: {}', smpref, filename,
                    sys.exc_info()[1]))
      sys.exit(1)
   
   for key in predata.keys():
      prnstr(key + ' = ' + predata[key], file=out)

   prnstr('', file=out)

   for slice in slices:
      prnstr(fmtstr('[Section = {}]', slice['name']), file=out)
      for key in slice.keys():
         val = slice[key]
         if key == 'name':
            pass
         elif key == kOutsize:
            prnstr(fmtstr('{} = {} {} {}', key, val[0], val[1], val[2]),
                   file=out)
         elif key == kSpacing or key == kXstacked or key == kYstacked:
            prnstr(fmtstr('{} = {} {}', key, val[0], val[1]), file=out)
         else:
            prnstr(fmtstr('{} = {}', key, val), file=out)

   for piece in pieces:
      prnstr('', file=out)
      prnstr(fmtstr('[Piece = {}]', piece['file']), file=out)
      for key in piece.keys():
         val = piece[key]
         if key == 'file':
            pass
         elif key == kFrame or key == kSize:
            prnstr(fmtstr('{} = {} {} {}', key, val[0], val[1], val[2]), file=out)
         elif key == kZlimit:
            prnstr(fmtstr('{} = {} {}', key, val[0], val[1]), file=out)
         else:
            prnstr(fmtstr('{} = {}', key, val), file=out)

   for edge in edges:
      prnstr('', file=out)
      prnstr(fmtstr('[Edge = {}]', edge['name']), file=out)
      for key in edge.keys():
         val = edge[key]
         if key == 'name':
            pass
         elif key == kLower:
            prnstr(fmtstr('{} = {} {} {}', key, val[0], val[1], val[2]), file=out)
         elif key == kShift or key == kOrigShift:
            prnstr(fmtstr('{} = {:f} {:f} {:f}', key, val[0], val[1], val[2]), file=out)
         elif key == kZlimit or key == kGoodlim:
            prnstr(fmtstr('{} = {} {}', key, val[0], val[1]), file=out)
         elif key == kBlendShift or key == kLastShift:
            prnstr(fmtstr('{} = {:f} {:f}', key, val[0], val[1]), file=out)
         else:
            prnstr(fmtstr('{} = {}', key, val), file=out)

            
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


# Function to build a piece map and also return needed sizes
def buildPieceMap(pieces, xmin, xmax, ymin, ymax, zmin, zmax, addedData):
   xsize = xmax + 1 - xmin
   xysize = xsize * (ymax + 1 - ymin)
   maxPieces = (zmax + 1 - zmin) * xysize

   pieceMap = []
   for i in range(maxPieces):
      pieceMap.append(-1)
   for i in range(len(pieces)):
      fxyz = pieces[i][kFrame]
      pieceMap[fxyz[0] - xmin + xsize * (fxyz[1] - ymin) + \
             xysize * (fxyz[2] - zmin)] = i
      if not kSize in pieces[i]:
         addedData = 1
         try:
            pieces[i][kSize] = getmrcsize(pieces[i]['file'])
         except ImodpyError:
            exitFromImodError(progname)

   return (xsize, xysize, addedData, pieceMap)
