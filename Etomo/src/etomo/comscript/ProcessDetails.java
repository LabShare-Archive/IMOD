package etomo.comscript;

import java.io.File;

import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
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
* <p> $Log$ </p>
*/