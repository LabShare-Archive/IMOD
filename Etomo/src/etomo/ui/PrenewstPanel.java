/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.2  2004/03/12 00:11:02  rickg
 * <p> Disables binning until xfproduct command is implemented
 * <p>
 * <p> Revision 1.1  2004/02/05 04:36:44  rickg
 * <p> Bug# 390 Initial revision
 * <p> </p>
 */

package etomo.ui;

import java.awt.event.MouseEvent;

import javax.swing.JPanel;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;

import etomo.comscript.ConstNewstParam;
import etomo.comscript.NewstParam;
import etomo.type.AxisID;

public class PrenewstPanel implements ContextMenu {
  public static final String rcsid = "$Id$";

  private JPanel pnlPrenewst = new JPanel();

  private LabeledSpinner spinBinning;

  AxisID axisID;

  public PrenewstPanel(AxisID id) {
    axisID = id;
    //  Construct the binning spinner
    SpinnerModel integerModel = new SpinnerNumberModel(1, 1, 50, 1);
    spinBinning = new LabeledSpinner("Pre-aligned image stack binning ",
      integerModel);
    pnlPrenewst.setBorder(new EtchedBorder("Newstack Parameters").getBorder());
    pnlPrenewst.add(spinBinning.getContainer());

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlPrenewst.addMouseListener(mouseAdapter);
  }

  JPanel getPanel() {
    return pnlPrenewst;
  }

  public void setParameters(ConstNewstParam prenewstParams) {
    int binning = prenewstParams.getBinByFactor();
    if (binning > 1) {
      spinBinning.setValue(binning);
    }
  }

  public void getParameters(NewstParam prenewstParams) {
  	int binning = ((Integer) spinBinning.getValue()).intValue();

		// Only explcitly write out the binning if its value is something other than
		// the default of 1 to keep from cluttering up the com script  
  	if(binning > 1) {
  	  prenewstParams.setBinByFactor(binning);
  	}
  	else{
  		prenewstParams.setBinByFactor(Integer.MIN_VALUE);
  	}
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = {"Newstack"};
    String[] manPage = {"newstack.html"};
    String[] logFileLabel = {"Prenewst"};
    String[] logFile = new String[1];
    logFile[0] = "prenewst" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(pnlPrenewst, mouseEvent,
      "COARSE ALIGNMENT", manPagelabel, manPage, logFileLabel, logFile);
  }

}
