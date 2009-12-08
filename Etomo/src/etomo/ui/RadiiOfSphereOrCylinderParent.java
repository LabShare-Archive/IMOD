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
 * <p> $Log$
 * <p> Revision 1.1  2009/12/02 04:42:23  sueh
 * <p> Factored RadiiOfSphereOrCylinder panel out of PeetDialog.
 * <p> </p>
 */
interface RadiiOfSphereOrCylinderParent {
  public static final String rcsid = "$Id$";

  boolean isMaskTypeSphereSelected();

  boolean isMaskTypeCylinderSelected();
}
