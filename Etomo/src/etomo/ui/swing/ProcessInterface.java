package etomo.ui.swing;

import etomo.type.ProcessingMethod;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2010</p>
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
* <p> Revision 1.1  2011/02/03 06:22:16  sueh
* <p> bug# 1422 Control of the processing method has been centralized in the
* <p> processing method mediator class.  Implementing ProcessInterface.
* <p> Supplying processes with the current processing method.
* <p> </p>
*/
public interface ProcessInterface {
  public static final String rcsid = "$Id$";

  /**
   * Tell the interface that the user has checked or unchecked the parallel panel's
   * cluster checkbox.
   * This only has an effect on a dialog which can do parallel GPU processing.
   * @param processingMethod
   */
  public void disableGpu(final boolean disable);

  /**
   * Get the processing method based on the dialogs settings.  This is the
   * display processing method; it should change depending on the tab.  In most
   * cases the display processing method and the run processing method are the
   * same (the parallel table is displayed when the user runs a parallel
   * processing), but not in every case.  Use a different function when the
   * display processing method does match the one you need to run with.  An
   * example if this situation is running from the Initial tab in Tomogram
   * Combination.
   * 
   * This function should never return QUEUE because interfaces don't need to
   * know if QUEUE is in use in the parallel panel.
   * @return
   */
  public ProcessingMethod getProcessingMethod();

  /**
   * When lock is true,disable check boxes that can change the processing
   * method.  When lock is false, enable these check boxes.
   * @param lock
   */
  public void lockProcessingMethod(boolean lock);
}
