package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.*;

import javax.swing.*;

import etomo.ApplicationManager;
import etomo.comscript.ConstSqueezevolParam;
import etomo.comscript.SqueezevolParam;
import etomo.comscript.TrimvolParam;
import etomo.type.AxisID;
import etomo.type.TomogramState;

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
 * <p> Revision 3.9  2005/01/10 23:56:32  sueh
 * <p> bug# 578 Modified isSqueezevolFlipped() and isTrimvolFlipped().
 * <p>
 * <p> Revision 3.8  2005/01/08 01:55:32  sueh
 * <p> bug# 578 Calling all backword compatible functions in TomogramState
 * <p> "getBackwordCompatible...".
 * <p>
 * <p> Revision 3.7  2004/12/16 02:33:05  sueh
 * <p> bug# 564 Taking whether trimvol output and squeezevol output are flipped
 * <p> or not when getting and setting Squeezevol parameters.
 * <p>
 * <p> Revision 3.6  2004/12/14 21:50:57  sueh
 * <p> bug# 557 Made separate variables for x and y reduction factors to handle
 * <p> an unflipped tomogram.
 * <p>
 * <p> Revision 3.5  2004/12/04 01:27:19  sueh
 * <p> bug# 557 Added call to imodSqueezedVolume().
 * <p>
 * <p> Revision 3.4  2004/12/02 20:41:48  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
 * <p> Revision 3.3  2004/12/02 18:30:50  sueh
 * <p> bug# 557 Added the Squeeze Volume panel.  Added an action for the
 * <p> Squeeze Volume button.
 * <p>
 * <p> Revision 3.2  2004/12/01 03:47:37  sueh
 * <p> bug# 557 Added ui fields to use with squeezevol.
 * <p>
 * <p> Revision 3.1  2004/03/15 20:33:55  rickg
 * <p> button variable name changes to btn...
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.5  2003/10/30 21:05:06  rickg
 * <p> Bug# 340 Added context menu
 * <p>
 * <p> Revision 2.4  2003/04/17 23:07:20  rickg
 * <p> Added cleanup panel
 * <p>
 * <p> Revision 2.3  2003/04/16 00:15:01  rickg
 * <p> Trimvol in progress
 * <p>
 * <p> Revision 2.2  2003/04/14 23:57:34  rickg
 * <p> Trimvol management changes
 * <p>
 * <p> Revision 2.1  2003/04/10 23:43:23  rickg
 * <p> Added trimvol panel
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.5.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.5  2002/12/19 17:45:22  rickg
 * <p> Implemented advanced dialog state processing
 * <p> including:
 * <p> default advanced state set on start up
 * <p> advanced button management now handled by
 * <p> super class
 * <p>
 * <p> Revision 1.4  2002/12/19 00:30:26  rickg
 * <p> app manager and root pane moved to super class
 * <p>
 * <p> Revision 1.3  2002/10/17 22:39:55  rickg
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
public class PostProcessingDialog
  extends ProcessDialog
  implements ContextMenu {
  public static final String rcsid =
    "$Id$";
  
  private TrimvolPanel trimvolPanel;
  private CleanupPanel cleanupPanel;
  
  private LabeledTextField ltfReductionFactorXY;
  private LabeledTextField ltfReductionFactorZ;
  private JCheckBox cbLinearInterpolation;
  
  private MultiLineToggleButton btnSqueezeVolume;
  private MultiLineButton btnImodSqueezedVolume;
  
  private PostProcessingDialogActionListener actionListener = new PostProcessingDialogActionListener(this);
  
  public PostProcessingDialog(ApplicationManager appMgr) {
    super(appMgr, AxisID.ONLY);
    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));

    trimvolPanel = new TrimvolPanel(applicationManager);
    rootPanel.add(trimvolPanel.getContainer());
    rootPanel.add(createSqueezeVolPanel());
    cleanupPanel = new CleanupPanel(applicationManager);
    rootPanel.add(cleanupPanel.getContainer());
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(pnlExitButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    btnAdvanced.setVisible(false);
    btnExecute.setText("Done");


		//  Mouse adapter for context menu
		GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
		rootPanel.addMouseListener(mouseAdapter);

    // Set the default advanced dialog state
    updateAdvanced();
  }
  
  private Container createSqueezeVolPanel() {
    SpacedPanel squeezeVolPanel = new SpacedPanel(FixedDim.x0_y5, true, false);
    squeezeVolPanel.setLayout(new BoxLayout(squeezeVolPanel.getContainer(), BoxLayout.Y_AXIS));
    squeezeVolPanel.setBorder(new EtchedBorder("Squeeze Volume").getBorder());
    //first component
    SpacedPanel squeezeVolPanel1 = new SpacedPanel(FixedDim.x5_y0);
    squeezeVolPanel1.setLayout(new BoxLayout(squeezeVolPanel1.getContainer(), BoxLayout.X_AXIS));
    ltfReductionFactorXY = new LabeledTextField("Reduction factor in X and Y ");
    squeezeVolPanel1.add(ltfReductionFactorXY);
    ltfReductionFactorZ = new LabeledTextField("in Z ");
    squeezeVolPanel1.add(ltfReductionFactorZ);
    squeezeVolPanel.add(squeezeVolPanel1);
    //second component
    cbLinearInterpolation = new JCheckBox("Linear interpolation");
    cbLinearInterpolation.setAlignmentX(Component.RIGHT_ALIGNMENT);
    squeezeVolPanel.add(cbLinearInterpolation);
    //third component
    SpacedPanel squeezeVolPanel2 = new SpacedPanel(SpacedPanel.HORIZONTAL_GLUE, true);
    squeezeVolPanel2.setLayout(new BoxLayout(squeezeVolPanel2.getContainer(), BoxLayout.X_AXIS));
    btnSqueezeVolume = new MultiLineToggleButton("Squeeze Volume");
    btnSqueezeVolume.addActionListener(actionListener);
    squeezeVolPanel2.addMultiLineButton(btnSqueezeVolume);
    btnImodSqueezedVolume = new MultiLineButton("Open Squeezed Volume in 3dmod");
    btnImodSqueezedVolume.addActionListener(actionListener);
    squeezeVolPanel2.addMultiLineButton(btnImodSqueezedVolume);
    squeezeVolPanel.add(squeezeVolPanel2);
    
    return squeezeVolPanel.getContainer();
  }
  
  /**
   * Set the panel values with the specified parameters
   * @param squeezevolParam
   */
  public void setParameters(ConstSqueezevolParam squeezevolParam) {
    ltfReductionFactorXY.setText(squeezevolParam.getReductionFactorX().toString());
    if (isSqueezevolFlipped()) {
      ltfReductionFactorZ.setText(squeezevolParam.getReductionFactorZ().toString());
    }
    else {
      ltfReductionFactorZ.setText(squeezevolParam.getReductionFactorY().toString());
    }
    cbLinearInterpolation.setSelected(squeezevolParam.isLinearInterpolation());
  }
  
  /**
   * Get the panel values
   * @param squeezevolParam
   */
  public void getParameters(SqueezevolParam squeezevolParam) {
    squeezevolParam.setReductionFactorX(ltfReductionFactorXY.getText());
    TomogramState state = applicationManager.getState();
    boolean flipped = squeezevolParam.setFlipped(isTrimvolFlipped());
    if (flipped) {
      squeezevolParam.setReductionFactorY(ltfReductionFactorXY.getText());
      squeezevolParam.setReductionFactorZ(ltfReductionFactorZ.getText());
    }
    else {
      squeezevolParam.setReductionFactorY(ltfReductionFactorZ.getText());
      squeezevolParam.setReductionFactorZ(ltfReductionFactorXY.getText());
    }
    squeezevolParam.setLinearInterpolation(cbLinearInterpolation.isSelected());
  }
  
  /**
   * return true if the result of squeezevol is flipped.
   * If squeezevol hasn't been done, return true if the result of trimvol is
   * flipped.
   * @return
   */
  public boolean isSqueezevolFlipped() {
    TomogramState state = applicationManager.getState();
    if (state.getSqueezevolFlipped().isSet()) {
      return state.getSqueezevolFlipped().is();
    }
    return isTrimvolFlipped();
  }
  
  /**
   * return true if the result of squeezevol is flipped.
   * If squeezevol hasn't been done, return true if the result of trimvol is
   * flipped.
   * @return
   */
  public boolean isTrimvolFlipped() {
    TomogramState state = applicationManager.getState();
    if (!state.getTrimvolFlipped().isSet()) {
      return state.getBackwardCompatibleTrimvolFlipped();
    }
    return state.getTrimvolFlipped().is();
  }

  /**
   * Set the trimvol panel values with the specified parameters
   * @param trimvolParam
   */
  public void setTrimvolParams(TrimvolParam trimvolParam) {
    trimvolPanel.setParameters(trimvolParam);
  }

  /**
   * Get the trimvol parameter values from the panel 
   * @param trimvolParam
   */
  public void getTrimvolParams(TrimvolParam trimvolParam) {
    trimvolPanel.getParameters(trimvolParam);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Trimvol"};
    String[] manPage = { "trimvol.html" };

    //    ContextPopup contextPopup =
    new ContextPopup(
      rootPanel,
      mouseEvent,
      "POST-PROCESSING", ContextPopup.TOMO_GUIDE,
      manPagelabel,
      manPage);
  }

  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    applicationManager.packMainWindow();
  }
  
  private void action(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(btnSqueezeVolume.getActionCommand())) {
      applicationManager.squeezevol();
    }
    else if (command.equals(btnImodSqueezedVolume.getActionCommand())) {
      applicationManager.imodSqueezedVolume();
    }
    else {
      throw new IllegalStateException("Unknown command " + command);
    }
  }

  
  //
  //  Action function overides for buttons
  //
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.donePostProcessing();
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.donePostProcessing();
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.donePostProcessing();
  }
  
  class PostProcessingDialogActionListener implements ActionListener {
    PostProcessingDialog adaptee;

    PostProcessingDialogActionListener(PostProcessingDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.action(event);
    }
  }

}
