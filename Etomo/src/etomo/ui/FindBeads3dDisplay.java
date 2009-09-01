package etomo.ui;

import etomo.comscript.FindBeads3dParam;

/**
 * <p>Description: </p>
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
 * <p> $Log$ </p>
 */
public interface FindBeads3dDisplay {
  public static final String rcsid = "$Id$";

  void getParameters(FindBeads3dParam param);
}
