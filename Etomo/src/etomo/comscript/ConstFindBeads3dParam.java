package etomo.comscript;

import etomo.type.ConstEtomoNumber;

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
public interface ConstFindBeads3dParam extends CommandDetails{
  public static final String rcsid = "$Id$";

  String getBeadSize();

  String getMinRelativeStrength();

  String getThresholdForAveraging();

  ConstEtomoNumber getStorageThreshold();

  String getMinSpacing();

  String getGuessNumBeads();

  String getMaxNumBeads();
}
