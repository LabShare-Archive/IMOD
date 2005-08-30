package etomo.ui;

import etomo.process.LoadAverageMonitor;

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
public interface LoadAverageDisplay {
  public static  final String  rcsid =  "$Id$";
  
  public LoadAverageMonitor getLoadAverageMonitor();
  public void setLoadAverage(String computer, double load1, double load5, double load15);
  public void loadAverageFailed(String computer);
}
/**
* <p> $Log$
* <p> Revision 1.1  2005/08/22 17:55:06  sueh
* <p> bug# 532 Interface for a display that can communicate with a
* <p> LoadAverageMonitor and can set the load average.
* <p> </p>
*/