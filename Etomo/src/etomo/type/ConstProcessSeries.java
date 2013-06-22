package etomo.type;

import etomo.process.ProcessData;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.1  2008/05/03 00:44:39  sueh
* <p> bug# 847 Interface for ProcessSeries to be used outside of manager andi expert classes and in process functions which to do not add to a process
* <p> series.
* <p> </p>
*/
public interface ConstProcessSeries {
  public static final String rcsid = "$Id$";

  public void dumpState();

  public String peekNextProcess();

  public boolean startNextProcess(AxisID axisID);

  public boolean startNextProcess(AxisID axisID, ProcessResultDisplay processResultDisplay);

  public DialogType getDialogType();

  public String getLastProcess();

  public String toString();

  public boolean willProcessBeDropped(ProcessData processData);

  public void startFailProcess(final AxisID axisID);

  public void startFailProcess(final AxisID axisID,
      final ProcessResultDisplay processResultDisplay);

  public boolean startPauseProcess(final AxisID axisID,
      final ProcessResultDisplay processResultDisplay);

  public boolean willProcessListBeDropped();
}
