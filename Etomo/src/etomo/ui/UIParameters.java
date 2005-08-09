/**
 * <p>Description: Provides a static store for UI parameters</p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.3  2004/04/26 03:17:17  rickg
 * <p> Add a norrow button dimension
 * <p>
 * <p> Revision 3.2  2004/03/24 03:02:31  rickg
 * <p> Changed spinner size to only specify spinner region.  The
 * <p> panel and label should be handled automatically
 * <p>
 * <p> Revision 3.1  2004/02/20 23:52:10  sueh
 * <p> bug# 386 added FileField dimension and spinner dimension
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.1  2003/10/16 21:49:05  rickg
 * <p> Initial revision
 * <p> </p>
 */
package etomo.ui;

import java.awt.Dimension;

import javax.swing.JCheckBox;

public class UIParameters {
	public static final String rcsid = "$Id$";
	
	private static Dimension dimButton = new Dimension();
	private static Dimension dimNarrowButton = new Dimension();
  private static Dimension dimSpinner = new Dimension();
  private static Dimension dimFileField = new Dimension();
  private static boolean recalcRun = false;
  
	/**
	 * Return the size of a standard button
	 * @return
	 */
  static Dimension getButtonDimension() {
    if (!recalcRun) {
      recalc();
    }
	  //  Return a safe copy of the Dimension
	  return new Dimension(dimButton);
	}
  static Dimension getNarrowButtonDimension() {
    if (!recalcRun) {
      recalc();
    }
	  //  Return a safe copy of the Dimension
	  return new Dimension(dimNarrowButton);
	}
  static Dimension getSpinnerDimension() {
    if (!recalcRun) {
      recalc();
    }
    return new Dimension(dimSpinner);
  }
  
  static Dimension getFileFieldDimension() {
    if (!recalcRun) {
      recalc();
    }
    return new Dimension(dimFileField);
  }

	
	/**
	 * Recalculate the size of objects given the current UI state.
	 *
	 */
	public static void recalc() {
    recalcRun = true;
	  //  Create a temporary check box and get its height
	  JCheckBox temp = new JCheckBox();
		double height = temp.getPreferredSize().getHeight();
	  dimButton.setSize(7 * height, 2 * height);
	  dimNarrowButton.setSize(4 * height, 2 * height);
    dimSpinner.setSize(2 * height, 1.05 * height);
    dimFileField.setSize(20 * height, 2 * height);
	}
}
