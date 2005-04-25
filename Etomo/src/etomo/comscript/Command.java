package etomo.comscript;

import java.io.File;

import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.5  2005/01/08 00:29:34  sueh
* <p> bug# 578 Rename functions to avoid conflicting with child class function
* <p> names.  Remove getBinning() because it is not generic enough.
* <p>
* <p> Revision 1.4  2004/12/08 21:17:00  sueh
* <p> bug# 564 Added getBooleanValue() to get a misc boolean value.
* <p>
* <p> Revision 1.3  2004/12/04 00:34:11  sueh
* <p> bug# 569 Handling directory paths with spaces:  converting from a
* <p> command line to a command array to prevent the command line from
* <p> being split on white space.
* <p>
* <p> Revision 1.2  2004/11/19 22:39:52  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.3  2004/11/12 22:45:52  sueh
* <p> bug# 520 Added getBinning() to get the binning that a command was run
* <p> with.  Added getIntegerValue(int) to get a miscelanious value that a
* <p> command was run with.
* <p>
* <p> Revision 1.1.2.2  2004/11/08 22:08:52  sueh
* <p> bug# 520 Add a function to query current mode.
* <p>
* <p> Revision 1.1.2.1  2004/10/06 01:28:49  sueh
* <p> bug# 520 An interface that allow BackgroundProcess to take a param
* <p> object rather then just a command line.  This allows BackgroundProcess
* <p> to be used by non-generic post-processing functions.
* <p> </p>
*/
public interface Command {
  public static  final String  rcsid =  "$Id$";
  
  public String getCommandName();
  public String getCommandLine();
  public String[] getCommandArray();
  public int getCommandMode();
  public File getCommandOutputFile();
  public int getIntegerValue(int name);
  public boolean getBooleanValue(int name);
  public AxisID getAxisID();
}
