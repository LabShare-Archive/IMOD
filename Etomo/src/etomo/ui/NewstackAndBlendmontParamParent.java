package etomo.ui;

import etomo.type.ConstEtomoNumber;

/**
 * <p>Description: Interface for the parent class of NewstackAndBlendmontParam</p>
 * 
 * <p>Copyright: Copyright 2009</p>
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
 */
interface NewstackAndBlendmontParamParent {
  public static final String rcsid = "$Id$";

  /**
   * Returns the NewstackAndBlendmont which contains values to be copied.
   * @return
   */
  public NewstackAndBlendmontParamPanel getMainInstance();

  public ConstEtomoNumber getUnbinnedBeadPixels();
  
  public boolean validate();
}
