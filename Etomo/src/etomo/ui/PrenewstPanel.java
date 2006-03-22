/**
 * <p>Description: Panel to modify the newstack parameters in prenewst</p>
 * 
 * <p>Copyright: Copyright (c) 2002-2004</p>
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
 * <p> Revision 1.14  2006/03/22 18:37:22  sueh
 * <p> bug# 803 Added cbMeanFloatDensities.
 * <p>
 * <p> Revision 1.13  2006/03/22 18:00:52  sueh
 * <p> bug# 803 Added cbByteModeToOutput.
 * <p>
 * <p> Revision 1.12  2005/07/14 22:09:03  sueh
 * <p> bug# 626 Enabling binning for montage view.  Setting binning in
 * <p> set and getParameters(BlendmontParam).
 * <p>
 * <p> Revision 1.11  2005/03/11 01:37:43  sueh
 * <p> bug# 533 Change title of panel to Blendmont Parameters when montage is
 * <p> set.
 * <p>
 * <p> Revision 1.10  2005/03/02 00:12:45  sueh
 * <p> bug# 533 disabled binning for montaging.
 * <p>
 * <p> Revision 1.9  2004/12/02 20:41:58  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
 * <p> Revision 1.8  2004/11/20 00:01:23  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.7.4.1  2004/10/11 02:16:51  sueh
 * <p> bug# 520 Passed the manager to the ContextPopup object in order to get
 * <p> the propertyUserDir.
 * <p>
 * <p> Revision 1.7  2004/06/17 18:49:05  sueh
 * <p> bug# 472
 * <p>
 * <p> Revision 1.6  2004/05/25 23:25:28  rickg
 * <p> Bug #391 moved fiducialess parameters to the parent dialog
 * <p>
 * <p> Revision 1.5  2004/04/07 21:03:10  rickg
 * <p> Fixed layout using UIUtilities
 * <p>
 * <p> Revision 1.4  2004/04/06 17:00:21  rickg
 * <p> Implemented basic fiducialess alignment interface
 * <p>
 * <p> Revision 1.3  2004/03/13 00:33:13  rickg
 * <p> Bug# 390 Add set/get Parameters and context menu
 * <p>
 * <p> Revision 1.2  2004/03/12 00:11:02  rickg
 * <p> Disables binning until xfproduct command is implemented
 * <p>
 * <p> Revision 1.1  2004/02/05 04:36:44  rickg
 * <p> Bug# 390 Initial revision
 * <p> </p>
 */

package etomo.ui;

import java.awt.Component;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.NewstParam;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ViewType;

public final class PrenewstPanel implements ContextMenu {
  public static final String rcsid = "$Id$";

  private final JPanel pnlPrenewst = new JPanel();
  private final ApplicationManager applicationManager;

  private final LabeledSpinner spinBinning;
  private final CheckBox cbByteModeToOutput = new CheckBox("Convert to bytes");
  private final CheckBox cbMeanFloatDensities = new CheckBox(
      "Float intensities to mean");

  private final AxisID axisID;

  public PrenewstPanel(ApplicationManager applicationManager, AxisID id) {
    axisID = id;
    this.applicationManager = applicationManager;
    pnlPrenewst.setLayout(new BoxLayout(pnlPrenewst, BoxLayout.Y_AXIS));

    //  Construct the binning spinner
    SpinnerModel integerModel = new SpinnerNumberModel(1, 1, 8, 1);
    spinBinning = new LabeledSpinner("Coarse aligned image stack binning ",
        integerModel);
    spinBinning.setTextMaxmimumSize(UIParameters.getSpinnerDimension());
    //if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
    //  spinBinning.setEnabled(false);
    //}
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      pnlPrenewst.setBorder(new EtchedBorder("Blendmont Parameters")
          .getBorder());
    }
    else {
      pnlPrenewst
          .setBorder(new EtchedBorder("Newstack Parameters").getBorder());
    }
    UIUtilities.addWithYSpace(pnlPrenewst, spinBinning.getContainer());
    UIUtilities.addWithYSpace(pnlPrenewst, cbByteModeToOutput);
    UIUtilities.addWithYSpace(pnlPrenewst, cbMeanFloatDensities);
    //  Align the UI objects along their left sides
    UIUtilities.alignComponentsX(pnlPrenewst, Component.LEFT_ALIGNMENT);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlPrenewst.addMouseListener(mouseAdapter);
    setToolTipText();
  }

  JPanel getPanel() {
    return pnlPrenewst;
  }

  void setAlignmentX(float align) {
    pnlPrenewst.setAlignmentX(align);
  }

  public void setParameters(ConstNewstParam prenewstParams) {
    int binning = prenewstParams.getBinByFactor();
    if (binning > 1) {
      spinBinning.setValue(binning);
    }
    cbByteModeToOutput
        .setSelected(prenewstParams.getModeToOutput() == ConstNewstParam.DATA_MODE_BYTE);
    cbMeanFloatDensities
        .setSelected(prenewstParams.getFloatDensities() == ConstNewstParam.FLOAT_DENSITIES_MEAN);
  }

  public void setParameters(BlendmontParam blendmontParams) {
    ConstEtomoNumber binning = blendmontParams.getBinByFactor();
    if (!binning.isNull()) {
      spinBinning.setValue(binning);
    }
  }

  public void getParameters(NewstParam prenewstParams) {
    int binning = ((Integer) spinBinning.getValue()).intValue();

    // Only explcitly write out the binning if its value is something other than
    // the default of 1 to keep from cluttering up the com script  
    if (binning > 1) {
      prenewstParams.setBinByFactor(binning);
    }
    else {
      prenewstParams.setBinByFactor(Integer.MIN_VALUE);
    }
    if (cbByteModeToOutput.isSelected()) {
      prenewstParams.setModeToOutput(ConstNewstParam.DATA_MODE_BYTE);
    }
    else {
      prenewstParams.setModeToOutput(ConstNewstParam.DATA_MODE_DEFAULT);
    }
    if (cbMeanFloatDensities.isSelected()) {
      prenewstParams.setFloatDensities(ConstNewstParam.FLOAT_DENSITIES_MEAN);
    }
    else {
      prenewstParams.setFloatDensities(ConstNewstParam.FLOAT_DENSITIES_DEFAULT);
    }
  }

  public void getParameters(BlendmontParam blendmontParam) {
    blendmontParam
        .setBinByFactor(((Integer) spinBinning.getValue()).intValue());
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Newstack" };
    String[] manPage = { "newstack.html" };
    String[] logFileLabel = { "Prenewst" };
    String[] logFile = new String[1];
    logFile[0] = "prenewst" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(pnlPrenewst, mouseEvent,
        "COARSE ALIGNMENT", ContextPopup.TOMO_GUIDE, manPagelabel, manPage,
        logFileLabel, logFile, applicationManager);
  }

  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    text = "Binning for the image stack used to generate and fix the fiducial model.";
    spinBinning.setToolTipText(tooltipFormatter.setText(text).format());
    cbByteModeToOutput
        .setToolTipText(tooltipFormatter
            .setText(
                "Set the storage mode of the output file to bytes.  When unchecked the storage mode is the same as that of the first input file.  Command:  "
                    + ConstNewstParam.DATA_MODE_OPTION
                    + " "
                    + ConstNewstParam.DATA_MODE_BYTE).format());
    cbMeanFloatDensities
        .setToolTipText(tooltipFormatter
            .setText(
                "Adjust densities of sections individually.  Scale sections to common mean and standard deviation.  This option is needed when the dynamic range is still too poor after X ray removal.  Command:  "
                    + ConstNewstParam.FLOAT_DENSITIES_OPTION
                    + " "
                    + ConstNewstParam.FLOAT_DENSITIES_MEAN).format());
  }

}