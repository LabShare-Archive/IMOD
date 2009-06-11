package etomo.ui;

import etomo.comscript.CCDEraserParam;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$ </p>
 */
public interface CCDEraserDisplay extends ProcessDisplay {
  public static final String rcsid = "$Id$";

  public void getParameters(final CCDEraserParam ccdEraserParams);
}
