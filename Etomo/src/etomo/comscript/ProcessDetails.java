package etomo.comscript;

import java.io.File;

import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005 - 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public interface ProcessDetails extends Command {
  public static  final String  rcsid =  "$Id$";

  public int getCommandMode();
  public File getCommandOutputFile();
  public int getIntegerValue(int name);
  public boolean getBooleanValue(int name);
  public AxisID getAxisID();
}
/**
* <p> $Log$
* <p> Revision 1.1  2005/11/14 21:22:27  sueh
* <p> bug# 744 An interface for params that are used in post processing.
* <p> Command can be used for params which need to be passed to the
* <p> process but aren't used in post processing.
* <p> </p>
*/