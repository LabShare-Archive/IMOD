package etomo.process;

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
public interface BackgroundComScriptMonitor extends ProcessMonitor {
  public static  final String  rcsid =  "$Id$";
  
  abstract public boolean isSuccessful();
  abstract public void kill();
  abstract public boolean isProcessRunning();
  abstract public AxisID getAxisID();
}
/**
* <p> $Log$ </p>
*/