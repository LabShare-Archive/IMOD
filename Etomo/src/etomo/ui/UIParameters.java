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
	
	/**
	 * Recalculate the size of objects given the current UI state.
	 *
	 */
	public static void recalc() {
	  //  Create a temporary check box and get its height
	  JCheckBox temp = new JCheckBox();
		double height = temp.getPreferredSize().getHeight();
	  dimButton.setSize(7 * height, 2 * height);
	}
}
