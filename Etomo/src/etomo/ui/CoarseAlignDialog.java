package etomo.ui;

import java.awt.Component;
import java.awt.event.*;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.type.AxisID;

import etomo.comscript.ConstTiltxcorrParam;
import etomo.comscript.TiltxcorrParam;
import etomo.comscript.FortranInputSyntaxException;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 2.7  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.6  2003/10/28 23:35:48  rickg
 * <p> Bug# 336 Context menu label capitalization
 * <p>
 * <p> Revision 2.5  2003/10/20 20:08:37  sueh
 * <p> Bus322 corrected labels
 * <p>
 * <p> Revision 2.4  2003/10/14 21:56:05  sueh
 * <p> Bug273 add tooltips
 * <p>
 * <p> Revision 2.3  2003/05/23 22:14:55  rickg
 * <p> Added xcorr log file to context menu
 * <p>
 * <p> Revision 2.2  2003/05/19 22:06:43  rickg
 * <p> Fixed cross correlation button text
 * <p>
 * <p> Revision 2.1  2003/04/28 23:25:25  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.5.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.5  2002/12/19 00:28:41  rickg
 * <p> Advanced handling implemented
 * <p>
 * <p> Revision 1.4  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.3  2002/10/17 22:38:59  rickg
 * <p> Added fileset name to window title
 * <p> this reference removed applicationManager messages
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class CoarseAlignDialog extends ProcessDialog implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private JPanel panelCoarseAlign = new JPanel();

  private BeveledBorder borderA = new BeveledBorder("Coarse Alignment");
  private CrossCorrelationPanel panelCrossCorrelation;
  private JToggleButton buttonCrossCorrelate =
    new JToggleButton("<html><b>Calculate<br>Cross-Correlation</b>");
  private JToggleButton buttonCoarseAlign =
    new JToggleButton("<html><b>Generate Coarse<br>Aligned Stack</b>");
  private JToggleButton buttonImod =
    new JToggleButton("<html><b>View Aligned<br>Stack In 3dmod</b>");
  private JToggleButton buttonMidas =
    new JToggleButton("<html><b>Fix Alignment<br>With Midas</b>");

  public CoarseAlignDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID);
    setToolTipText();
    fixRootPanel(rootSize);

    panelCrossCorrelation = new CrossCorrelationPanel(axisID);

    buttonExecute.setText("Done");

    buttonCrossCorrelate.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonCrossCorrelate.setPreferredSize(FixedDim.button2Line);
    buttonCrossCorrelate.setMaximumSize(FixedDim.button2Line);

    buttonCoarseAlign.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonCoarseAlign.setPreferredSize(FixedDim.button2Line);
    buttonCoarseAlign.setMaximumSize(FixedDim.button2Line);

    buttonImod.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonImod.setPreferredSize(FixedDim.button2Line);
    buttonImod.setMaximumSize(FixedDim.button2Line);

    buttonMidas.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonMidas.setPreferredSize(FixedDim.button2Line);
    buttonMidas.setMaximumSize(FixedDim.button2Line);

    panelCoarseAlign.setLayout(
      new BoxLayout(panelCoarseAlign, BoxLayout.Y_AXIS));
    panelCoarseAlign.setBorder(borderA.getBorder());
    panelCoarseAlign.add(panelCrossCorrelation.getPanel());
    panelCoarseAlign.add(Box.createRigidArea(FixedDim.x0_y10));
    panelCoarseAlign.add(buttonCrossCorrelate);
    panelCoarseAlign.add(Box.createRigidArea(FixedDim.x0_y10));
    panelCoarseAlign.add(buttonCoarseAlign);
    panelCoarseAlign.add(Box.createRigidArea(FixedDim.x0_y10));
    panelCoarseAlign.add(buttonImod);
    panelCoarseAlign.add(Box.createRigidArea(FixedDim.x0_y10));
    panelCoarseAlign.add(buttonMidas);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.add(panelCoarseAlign);
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(panelExitButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Action listener assignment for the buttons
    ActionListener actionListener = new CoarseAlignActionListener(this);
    buttonCrossCorrelate.addActionListener(actionListener);
    buttonCoarseAlign.addActionListener(actionListener);
    buttonImod.addActionListener(actionListener);
    buttonMidas.addActionListener(actionListener);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    panelCoarseAlign.addMouseListener(mouseAdapter);

    // Set the default advanced state for the window
    updateAdvanced();
  }

  /**
   * Set the parameters for the cross correlation panel
   */
  public void setCrossCorrelationParams(ConstTiltxcorrParam tiltXcorrParams) {
    panelCrossCorrelation.setParameters(tiltXcorrParams);
  }

  /**
   * Get the parameters from the specified cross correlation panel
   */
  public void getCrossCorrelationParams(TiltxcorrParam tiltXcorrParams)
    throws FortranInputSyntaxException {
    panelCrossCorrelation.getParameters(tiltXcorrParams);
  }

  /**
   * Return the rootPanel object
   */
  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    updateAdvanced();
  }

  void updateAdvanced() {
    panelCrossCorrelation.setAdvanced(isAdvanced);
    applicationManager.packMainWindow();
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Xftoxg", "Newst", "Newstack", "3dmod", "Midas" };
    String[] manPage =
      {
        "xftoxg.html",
        "newst.html",
        "newstack.html",
        "3dmod.html",
        "midas.html" };

    String[] logFileLabel = {"Xcorr", "Prenewst"};
    String[] logFile = new String[2];
    logFile[0] = "xcorr" + axisID.getExtension() + ".log";
    logFile[1] = "prenewst" + axisID.getExtension() + ".log";

    ContextPopup contextPopup =
      new ContextPopup(
        panelCoarseAlign,
        mouseEvent,
        "COARSE ALIGNMENT",
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
  }
  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    
    text = 
      "Find alignment transformations between successive images by cross-correlation.";
    buttonCrossCorrelate.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Use transformations to produce stack of aligned images.";
    buttonCoarseAlign.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Use 3dmod to view the coarsely aligned images.";
    buttonImod.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Use Midas to adjust bad alignments.";
    buttonMidas.setToolTipText(tooltipFormatter.setText(text).format());
  }

  //  Action function for stack buttons
  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals(buttonCrossCorrelate.getActionCommand())) {
      applicationManager.crossCorrelate(axisID);
    }

    else if (command.equals(buttonCoarseAlign.getActionCommand())) {
      applicationManager.coarseAlign(axisID);
    }

    else if (command.equals(buttonImod.getActionCommand())) {
      applicationManager.imodAlign(axisID);
    }

    else if (command.equals(buttonMidas.getActionCommand())) {
      applicationManager.midasRawStack(axisID);
    }
  }

  //  Action function overides for buttons
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneCoarseAlignDialog(axisID);
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneCoarseAlignDialog(axisID);
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneCoarseAlignDialog(axisID);
  }

}

class CoarseAlignActionListener implements ActionListener {

  CoarseAlignDialog adaptee;

  CoarseAlignActionListener(CoarseAlignDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonAction(event);
  }
}
