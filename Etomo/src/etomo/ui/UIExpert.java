package etomo.ui;

import etomo.type.ProcessResultDisplay;

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
 */
public interface UIExpert {
  public static final String rcsid = "$Id$";

  public void startNextProcess(ProcessResultDisplay processResultDisplay);
  public void openDialog();
  public void doneDialog();
  public void saveDialog();
}
/**
 * <p> $Log$ </p>
 */
