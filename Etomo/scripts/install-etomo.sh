#!/bin/sh
#
# Description: installation script for eTomo beta
# 
# Copyright: Copyright (c) 2002, 2003
# Boulder Laboratory for 3D Fine Structure, University of Colorado
# 
# $Author$
# 
# $Revision$
# 
# $Log$

# Check to see if IMOD_DIR is defined, notify the user if it not
if [ -z "$IMOD_DIR" ]; then
  echo "The IMOD_DIR environement variable has not been set.  IMOD_DIR must"
  echo "point to where IMOD has been installed.  When that is done rerun "
  echo $0 "to install eTomo."
  exit -1
fi
# Copy the JRE, etomo.jar and startup script to the installation directory
if [ -e $IMOD_DIR/jre ]; then
  echo -n "Removing existing Java Runtime Environment from the IMOD directory..."
  rm -rf $IMOD_DIR/jre
  echo "done"
fi

echo -n "Copying the Java Runtime Environment into the IMOD directory..."
cp -a jre $IMOD_DIR
echo "done"

echo -n "Copying eTomo to the IMOD bin directory..."
cp -a bin $IMOD_DIR
echo "done"

# Copy the menu entries for KDE and Gnome (this is probably highly distribution
# dependent
echo ""
echo "Success, eTomo is installed in $IMOD_DIR"
echo "Use the command etomo to start the program"
