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
	
	static Dimension dimButton = new Dimension();
	static Dimension dimNarrowButton = new Dimension();
  static Dimension dimSpinner = new Dimension();
  static Dimension dimFileField = new Dimension();
	
	/**
	 * Default constructor
	 */
	public UIParameters(){
	  recalc();
	}
	
	/**
	 * Return the size of a standard button
	 * @return
	 */
	public static Dimension getButtonDimension(){
	  //  Return a safe copy of the Dimension
	  return new Dimension(dimButton);
	}
	public static Dimension getNarrowButtonDimension(){
	  //  Return a safe copy of the Dimension
	  return new Dimension(dimNarrowButton);
	}
  public static Dimension getSpinnerDimension() {
    return new Dimension(dimSpinner);
  }
  
  public static Dimension getFileFieldDimension() {
    return new Dimension(dimFileField);
  }

	
	/**
	 * Recalculate the size of objects given the current UI state.
	 *
	 */
	public static void recalc() {
	  //  Create a temporary check box and get its height
	  JCheckBox temp = new JCheckBox();
		double height = temp.getPreferredSize().getHeight();
	  dimButton.setSize(7 * height, 2 * height);
	  dimNarrowButton.setSize(4 * height, 2 * height);
    dimSpinner.setSize(2 * height, 1.05 * height);
    dimFileField.setSize(20 * height, 2 * height);
	}
}
