package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstMatchorwarpParam;
import etomo.comscript.ConstPatchcrawl3DParam;
import etomo.comscript.MatchorwarpParam;
import etomo.comscript.Patchcrawl3DParam;

/**
 * <p>
 * Description:
 * 
 * Note: The Y and Z parameters are presented to the user in swapped format,
 * all other representations of those parameters are as they appear in the
 * commands. Specifically Y contains the depth dimension.
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002
 * </p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3D Fine Structure, University of
 * Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p>
 * $Log$
 * Revision 1.17  2003/10/17 15:44:24  rickg
 * Bug# 303 Re-layout of UI items
 * Get button sizes from UIParameters static object
 * Revision 1.16 2003/10/16 21:03:35 rickg
 * Bug# 303 Label changes and re-layout
 * 
 * <p>
 * Revision 1.15 2003/10/15 22:45:40 rickg
 * <p>
 * Button size change
 * <p>
 * <p>
 * Revision 1.14 2003/06/05 04:41:31 rickg
 * <p>
 * Label change
 * <p>
 * <p>
 * Revision 1.13 2003/05/15 04:28:48 rickg
 * <p>
 * Removed test button for volcombine
 * <p>
 * <p>
 * Revision 1.12 2003/05/14 14:38:08 rickg
 * <p>
 * Temporary button
 * <p>
 * <p>
 * Revision 1.11 2003/04/09 23:37:46 rickg
 * <p>
 * Moved CheckBoxTextPanel out
 * <p>
 * <p>
 * Revision 1.10 2003/03/26 00:52:56 rickg
 * <p>
 * Added button to convert patch_vector.mod to patch.out
 * <p>
 * <p>
 * Revision 1.9 2003/03/20 17:57:10 rickg
 * <p>
 * Fixed combined volume button size
 * <p>
 * <p>
 * </p>
 */
public class FinalCombinePanel implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private ApplicationManager applicationManager;

  private JPanel pnlRoot = new JPanel();

  private JPanel pnlPatchcorr = new JPanel();

  private JPanel pnlPatchsize = new JPanel();
  private JPanel pnlPatchsizeEdit = new JPanel();
  private LabeledTextField ltfXPatchSize =
    new LabeledTextField("X patch size :");
  private LabeledTextField ltfYPatchSize =
    new LabeledTextField("Z patch size :");
  private LabeledTextField ltfZPatchSize =
    new LabeledTextField("Y patch size :");
  private JPanel pnlPatchsizeButtons = new JPanel();
  private JButton btnPatchsizeIncrease =
    new JButton("<html><b>Patch Size +20%</b>");
  private JButton btnPatchsizeDecrease =
    new JButton("<html><b>Patch Size -20%</b>");

  private LabeledTextField ltfXNPatches =
    new LabeledTextField("Number of X patches :");
  private LabeledTextField ltfYNPatches =
    new LabeledTextField("Number of Z patches :");
  private LabeledTextField ltfZNPatches =
    new LabeledTextField("Number of Y patches :");

  private JPanel pnlBoundary = new JPanel();
  private LabeledTextField ltfXLow = new LabeledTextField("X Low :");
  private LabeledTextField ltfXHigh = new LabeledTextField("X high :");
  private LabeledTextField ltfYLow = new LabeledTextField("Z Low :");
  private LabeledTextField ltfYHigh = new LabeledTextField("Z high :");
  private LabeledTextField ltfZLow = new LabeledTextField("Y Low :");
  private LabeledTextField ltfZHigh = new LabeledTextField("Y high :");

  private JButton btnPatchcorrRestart =
    new JButton("<html><b>Restart at Patchcorr</b>");

  private JPanel pnlMatchorwarp = new JPanel();
  private JPanel pnlPatchRegionModel = new JPanel();
  private JCheckBox cbUsePatchRegionModel =
    new JCheckBox("Use patch region model");
  private MultiLineButton btnPatchRegionModel =
    new MultiLineButton("<html><b>Create/Edit Patch Region Model</b>");
  private LabeledTextField ltfWarpLimit =
    new LabeledTextField("Warping residual limits: ");
  private LabeledTextField ltfRefineLimit =
    new LabeledTextField("Residual limit for single transform: ");

  private LabeledTextField cbtfXLowerExclude =
    new LabeledTextField("Number of columns to exclude on left (in X): ");
  private LabeledTextField cbtfXUpperExclude =
    new LabeledTextField("Number of columns to exclude on right (in X): ");
  private LabeledTextField cbtfZLowerExclude =
    new LabeledTextField("Number of rows to exclude on bottom (in Y): ");
  private LabeledTextField cbtfZUpperExclude =
    new LabeledTextField("Number of rows to exclude on top (in Y): ");
  private JPanel pnlMatchorwarpButtons = new JPanel();
  private JButton btnMatchorwarpRestart =
    new JButton("<html><b>Restart at Matchorwarp</b>");
  private JButton btnMatchorwarpTrial =
    new JButton("<html><b>Matchorwarp Trial Run</b>");

  private JPanel pnlButton = new JPanel();

  private JButton btnPatchVectorModel =
    new JButton("<html><b>Examine Patch Vector Model</b>");
  private JButton btnCommitPatchOut =
    new JButton("<html><b>Replace Patch Vector Model</b>");
  private JButton btnImodMatchedTo =
    new JButton("<html><b>Open Volume Being Matched To</b>");
  private JButton btnImodCombined =
    new JButton("<html><b>Open Combined Volume</b>");

  /**
	 * Default constructor
	 * 
	 * @param appMgr
	 */
  public FinalCombinePanel(ApplicationManager appMgr) {

    applicationManager = appMgr;

    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));

    //  Set the button sizes
    Dimension dimButton = UIParameters.getButtonDimension();
    btnPatchRegionModel.setPreferredSize(dimButton);
    btnPatchRegionModel.setMaximumSize(dimButton);
    btnPatchVectorModel.setPreferredSize(dimButton);
    btnPatchVectorModel.setMaximumSize(dimButton);
    btnCommitPatchOut.setPreferredSize(dimButton);
    btnCommitPatchOut.setMaximumSize(dimButton);
    btnImodMatchedTo.setPreferredSize(dimButton);
    btnImodMatchedTo.setMaximumSize(dimButton);
    btnImodCombined.setPreferredSize(dimButton);
    btnImodCombined.setMaximumSize(dimButton);

    btnPatchcorrRestart.setPreferredSize(dimButton);
    btnPatchcorrRestart.setMaximumSize(dimButton);
    btnPatchsizeIncrease.setPreferredSize(dimButton);
    btnPatchsizeIncrease.setMaximumSize(dimButton);
    btnPatchsizeDecrease.setPreferredSize(dimButton);
    btnPatchsizeDecrease.setMaximumSize(dimButton);

    btnMatchorwarpRestart.setPreferredSize(dimButton);
    btnMatchorwarpRestart.setMaximumSize(dimButton);
    btnMatchorwarpTrial.setPreferredSize(dimButton);
    btnMatchorwarpTrial.setMaximumSize(dimButton);

    // Layout Patch region model panel
    pnlPatchRegionModel.setLayout(
      new BoxLayout(pnlPatchRegionModel, BoxLayout.X_AXIS));
    pnlPatchRegionModel.setBorder(
      new EtchedBorder("Patch Region Model").getBorder());
    pnlPatchRegionModel.add(cbUsePatchRegionModel);
		pnlPatchRegionModel.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlPatchRegionModel.add(btnPatchRegionModel);
		pnlPatchRegionModel.add(Box.createHorizontalGlue());


    // Layout the Patchcorr panel
    pnlPatchcorr.setLayout(new BoxLayout(pnlPatchcorr, BoxLayout.Y_AXIS));
    pnlPatchcorr.setBorder(
      new EtchedBorder("Patchcorr Parameters").getBorder());

    pnlPatchsizeButtons.setLayout(
      new BoxLayout(pnlPatchsizeButtons, BoxLayout.Y_AXIS));
    pnlPatchsizeButtons.add(btnPatchsizeIncrease);
    pnlPatchsizeButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlPatchsizeButtons.add(btnPatchsizeDecrease);

    pnlPatchsizeEdit.setLayout(
      new BoxLayout(pnlPatchsizeEdit, BoxLayout.Y_AXIS));

    pnlPatchsizeEdit.add(ltfXPatchSize.getContainer());
    pnlPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlPatchsizeEdit.add(ltfZPatchSize.getContainer());
    pnlPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlPatchsizeEdit.add(ltfYPatchSize.getContainer());
    pnlPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlPatchsize.setLayout(new BoxLayout(pnlPatchsize, BoxLayout.X_AXIS));
    pnlPatchsize.add(pnlPatchsizeEdit);
    pnlPatchsize.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlPatchsize.add(pnlPatchsizeButtons);
    pnlPatchcorr.add(pnlPatchsize);

    pnlPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlBoundary.setLayout(new GridLayout(3, 3, 5, 5));
    pnlBoundary.add(ltfXNPatches.getContainer());
    pnlBoundary.add(ltfXLow.getContainer());
    pnlBoundary.add(ltfXHigh.getContainer());
    pnlBoundary.add(ltfZNPatches.getContainer());
    pnlBoundary.add(ltfZLow.getContainer());
    pnlBoundary.add(ltfZHigh.getContainer());
    pnlBoundary.add(ltfYNPatches.getContainer());
    pnlBoundary.add(ltfYLow.getContainer());
    pnlBoundary.add(ltfYHigh.getContainer());
    pnlPatchcorr.add(pnlBoundary);
    pnlPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));
    btnPatchcorrRestart.setAlignmentX(Component.CENTER_ALIGNMENT);

    pnlPatchcorr.add(btnPatchcorrRestart);
    pnlPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));


    //  Layout the Matchorwarp panel
    pnlMatchorwarp.setLayout(new BoxLayout(pnlMatchorwarp, BoxLayout.Y_AXIS));
    pnlMatchorwarp.setBorder(
      new EtchedBorder("Matchorwarp Parameters").getBorder());

		pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y10));
		pnlMatchorwarp.add(ltfRefineLimit.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlMatchorwarp.add(ltfWarpLimit.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y10));

    pnlMatchorwarp.add(cbtfXLowerExclude.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarp.add(cbtfXUpperExclude.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarp.add(cbtfZLowerExclude.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarp.add(cbtfZUpperExclude.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlMatchorwarpButtons.setLayout(
      new BoxLayout(pnlMatchorwarpButtons, BoxLayout.X_AXIS));
    pnlMatchorwarpButtons.add(Box.createHorizontalGlue());
    pnlMatchorwarpButtons.add(btnMatchorwarpRestart);
    pnlMatchorwarpButtons.add(Box.createHorizontalGlue());
    pnlMatchorwarpButtons.add(btnMatchorwarpTrial);
    pnlMatchorwarpButtons.add(Box.createHorizontalGlue());
    pnlMatchorwarp.add(pnlMatchorwarpButtons);
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));

    //  Create the button panel
    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnPatchVectorModel);
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnCommitPatchOut);
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodMatchedTo);
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodCombined);
    pnlButton.add(Box.createHorizontalGlue());

    //  Root panel layout
		pnlRoot.add(pnlPatchRegionModel);
		pnlRoot.add(pnlPatchcorr);
		pnlRoot.add(pnlMatchorwarp);		
    pnlRoot.add(Box.createVerticalGlue());
    pnlRoot.add(pnlButton);

    // Bind the buttons to action listener
    ButtonActionListener actionListener = new ButtonActionListener(this);
    btnPatchcorrRestart.addActionListener(actionListener);
    btnPatchsizeIncrease.addActionListener(actionListener);
    btnPatchsizeDecrease.addActionListener(actionListener);
    btnPatchRegionModel.addActionListener(actionListener);
    btnMatchorwarpRestart.addActionListener(actionListener);
    btnMatchorwarpTrial.addActionListener(actionListener);
    btnPatchVectorModel.addActionListener(actionListener);
    btnCommitPatchOut.addActionListener(actionListener);
    btnImodMatchedTo.addActionListener(actionListener);
    btnImodCombined.addActionListener(actionListener);

    // Mouse listener for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlRoot.addMouseListener(mouseAdapter);
  }

  void setAdvanced(boolean state) {
    pnlBoundary.setVisible(state);
    ltfRefineLimit.setVisible(state);
  }

  /**
	 * Return the pnlRoot reference
	 * 
	 * @return Container
	 */
  public Container getContainer() {
    return pnlRoot;
  }

  /**
	 * Set the values of the patchcrawl3D UI objects from the
	 * ConstPatchcrawl3DParam object.
	 * 
	 * @param patchrawlParam
	 */
  public void setPatchcrawl3DParams(ConstPatchcrawl3DParam patchrawlParam) {
    ltfXPatchSize.setText(patchrawlParam.getXPatchSize());
    ltfYPatchSize.setText(patchrawlParam.getYPatchSize());
    ltfZPatchSize.setText(patchrawlParam.getZPatchSize());
    ltfXNPatches.setText(patchrawlParam.getNX());
    ltfYNPatches.setText(patchrawlParam.getNY());
    ltfZNPatches.setText(patchrawlParam.getNZ());
    ltfXLow.setText(patchrawlParam.getXLow());
    ltfXHigh.setText(patchrawlParam.getXHigh());
    ltfYLow.setText(patchrawlParam.getYLow());
    ltfYHigh.setText(patchrawlParam.getYHigh());
    ltfZLow.setText(patchrawlParam.getZLow());
    ltfZHigh.setText(patchrawlParam.getZHigh());
  }

  /**
	 * Set the Patchcrawl3DParam object values from the UI values.
	 * 
	 * @param patchrawlParam
	 * @throws NumberFormatException
	 */
  public void getPatchcrawl3DParams(Patchcrawl3DParam patchcrawl3DParam)
    throws NumberFormatException {
    String badParameter = "";

    try {
      badParameter = ltfXPatchSize.getLabel();
      patchcrawl3DParam.setXPatchSize(
        Integer.parseInt(ltfXPatchSize.getText()));
      badParameter = ltfYPatchSize.getLabel();
      patchcrawl3DParam.setYPatchSize(
        Integer.parseInt(ltfYPatchSize.getText()));
      badParameter = ltfZPatchSize.getLabel();
      patchcrawl3DParam.setZPatchSize(
        Integer.parseInt(ltfZPatchSize.getText()));
      badParameter = ltfXNPatches.getLabel();
      patchcrawl3DParam.setNX(Integer.parseInt(ltfXNPatches.getText()));
      badParameter = ltfYNPatches.getLabel();
      patchcrawl3DParam.setNY(Integer.parseInt(ltfYNPatches.getText()));
      badParameter = ltfZNPatches.getLabel();
      patchcrawl3DParam.setNZ(Integer.parseInt(ltfZNPatches.getText()));
      badParameter = ltfXLow.getLabel();
      patchcrawl3DParam.setXLow(Integer.parseInt(ltfXLow.getText()));
      badParameter = ltfXHigh.getLabel();
      patchcrawl3DParam.setXHigh(Integer.parseInt(ltfXHigh.getText()));
      badParameter = ltfYLow.getLabel();
      patchcrawl3DParam.setYLow(Integer.parseInt(ltfYLow.getText()));
      badParameter = ltfYHigh.getLabel();
      patchcrawl3DParam.setYHigh(Integer.parseInt(ltfYHigh.getText()));
      badParameter = ltfZLow.getLabel();
      patchcrawl3DParam.setZLow(Integer.parseInt(ltfZLow.getText()));
      badParameter = ltfZHigh.getLabel();
      patchcrawl3DParam.setZHigh(Integer.parseInt(ltfZHigh.getText()));

    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
  }

  /**
	 * Set the values of the matchorwarp UI objects from the
	 * ConstMatchorwarpParam object.
	 * 
	 * @param matchorwarpParam
	 */
  public void setMatchorwarpParams(ConstMatchorwarpParam matchorwarpParam) {
    cbUsePatchRegionModel.setSelected(
      !matchorwarpParam.getModelFile().equals(""));
    ltfWarpLimit.setText(matchorwarpParam.getWarpLimit());
    ltfRefineLimit.setText(matchorwarpParam.getRefineLimit());

    if (matchorwarpParam.getXLowerExclude() > 0) {
      cbtfXLowerExclude.setText(matchorwarpParam.getXLowerExclude());
    }
    if (matchorwarpParam.getXUpperExclude() > 0) {
      cbtfXUpperExclude.setText(matchorwarpParam.getXUpperExclude());
    }

    if (matchorwarpParam.getZLowerExclude() > 0) {
      cbtfZLowerExclude.setText(matchorwarpParam.getZLowerExclude());
    }

    if (matchorwarpParam.getZUpperExclude() > 0) {
      cbtfZUpperExclude.setText(matchorwarpParam.getZUpperExclude());
    }
  }

  /**
	 * Set the MatchorwarpParam object values from the UI values.
	 * 
	 * @param matchorwarpParam
	 * @throws NumberFormatException
	 */
  public void getMatchorwarpParams(MatchorwarpParam matchorwarpParam)
    throws NumberFormatException {
    String badParameter = "";

    try {
      badParameter = cbUsePatchRegionModel.getText();
      if (cbUsePatchRegionModel.isSelected()) {
        matchorwarpParam.setDefaultModelFile();
      }
      else {
        matchorwarpParam.setModelFile("");
      }

      badParameter = ltfWarpLimit.getLabel();
      matchorwarpParam.setWarpLimit(ltfWarpLimit.getText());

      badParameter = ltfRefineLimit.getLabel();
      matchorwarpParam.setRefineLimit(
        Double.parseDouble(ltfRefineLimit.getText()));

      badParameter = cbtfXLowerExclude.getLabel();
      String text = cbtfXLowerExclude.getText();
      if (text.matches("\\S+")) {
        matchorwarpParam.setXLowerExclude(Integer.parseInt(text));
      }
      else {
        matchorwarpParam.setXLowerExclude(0);
      }

      badParameter = cbtfXUpperExclude.getLabel();
      text = cbtfXUpperExclude.getText();
      if (text.matches("\\S+")) {
        matchorwarpParam.setXUpperExclude(Integer.parseInt(text));
      }
      else {
        matchorwarpParam.setXUpperExclude(0);
      }

      badParameter = cbtfZLowerExclude.getLabel();
      text = cbtfZLowerExclude.getText();
      if (text.matches("\\S+")) {
        matchorwarpParam.setZLowerExclude(Integer.parseInt(text));
      }
      else {
        matchorwarpParam.setZLowerExclude(0);
      }

      badParameter = cbtfZUpperExclude.getLabel();
      text = cbtfZUpperExclude.getText();
      if (text.matches("\\S+")) {
        matchorwarpParam.setZUpperExclude(Integer.parseInt(text));
      }
      else {
        matchorwarpParam.setZUpperExclude(0);
      }
    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
  } /**
		 * Right mouse button context menu
		 */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Patchcrawl3d", "Matchorwarp" };
    String[] manPage = { "patchcrawl3d.html", "matchorwarp.html" };
    String[] logFileLabel = { "Patchcorr", "Matchorwarp", "Volcombine" };
    String[] logFile = { "patchcorr.log", "matchorwarp.log", "volcombine.log" };
    ContextPopup contextPopup =
      new ContextPopup(
        pnlRoot,
        mouseEvent,
        "Patch Problems in Combining",
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
  }

  private void buttonAction(ActionEvent event) { // Decrease patch sizes by 20%
    // and then round to ints
    // since they are in
    // pixels
    if (event
      .getActionCommand()
      .equals(btnPatchsizeDecrease.getActionCommand())) {
      ltfXPatchSize.setText(
        Math.round(Integer.parseInt(ltfXPatchSize.getText()) / 1.2f));
      ltfYPatchSize.setText(
        Math.round(Integer.parseInt(ltfYPatchSize.getText()) / 1.2f));
      ltfZPatchSize.setText(
        Math.round(Integer.parseInt(ltfZPatchSize.getText()) / 1.2f));
    } //  Increase patch sizes by 20% and then round to ints since they are
    // in
    // pixels
    if (event
      .getActionCommand()
      .equals(btnPatchsizeIncrease.getActionCommand())) {
      ltfXPatchSize.setText(
        Math.round(Integer.parseInt(ltfXPatchSize.getText()) * 1.2f));
      ltfYPatchSize.setText(
        Math.round(Integer.parseInt(ltfYPatchSize.getText()) * 1.2f));
      ltfZPatchSize.setText(
        Math.round(Integer.parseInt(ltfZPatchSize.getText()) * 1.2f));
    }

    if (event
      .getActionCommand()
      .equals(btnPatchcorrRestart.getActionCommand())) {
      applicationManager.patchcorrCombine();
    }

    if (event
      .getActionCommand()
      .equals(btnPatchRegionModel.getActionCommand())) {
      applicationManager.imodPatchRegionModel();
    }

    if (event
      .getActionCommand()
      .equals(btnMatchorwarpRestart.getActionCommand())) {
      applicationManager.matchorwarpCombine();
    }

    if (event
      .getActionCommand()
      .equals(btnMatchorwarpTrial.getActionCommand())) {
      applicationManager.matchorwarpTrial();
    }

    if (event
      .getActionCommand()
      .equals(btnPatchVectorModel.getActionCommand())) {
      applicationManager.imodPatchVectorModel();
    }

    if (event
      .getActionCommand()
      .equals(btnCommitPatchOut.getActionCommand())) {
      applicationManager.modelToPatch();
    }

    if (event.getActionCommand().equals(btnImodMatchedTo.getActionCommand())) {
      applicationManager.imodMatchedToTomogram();
    }

    if (event.getActionCommand().equals(btnImodCombined.getActionCommand())) {
      applicationManager.imodCombinedTomogram();
    }
  }

  class ButtonActionListener implements ActionListener {
    FinalCombinePanel listenee;
    ButtonActionListener(FinalCombinePanel finalCombinePanel) {
      listenee = finalCombinePanel;
    }

    public void actionPerformed(ActionEvent event) {
      listenee.buttonAction(event);
    }
  }
}
