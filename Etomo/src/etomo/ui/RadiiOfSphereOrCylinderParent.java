package etomo.ui;

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
interface RadiiOfSphereOrCylinderParent {
  public static final String rcsid = "$Id$";

  boolean isMaskTypeSphereSelected();

  boolean isMaskTypeCylinderSelected();

  String getMaskTypeLabel();

  String getMaskTypeSphereLabel();

  String getMaskTypeCylinderLabel();
}
