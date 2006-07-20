package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.CombineParams;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.ConstSolvematchParam;
import etomo.comscript.SolvematchParam;
import etomo.storage.autodoc.Autodoc;
import etomo.storage.autodoc.Section;
import etomo.type.AxisID;
import etomo.type.EtomoAutodoc;
import etomo.type.FiducialMatch;
import etomo.type.Run3dmodMenuOptions;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
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
 * <p> Revision 3.23  2006/07/05 23:26:19  sueh
 * <p> Added tooltips.
 * <p>
 * <p> Revision 3.22  2006/06/09 17:06:31  sueh
 * <p> bug# 869 Enabling/disabling the tabs doesn't using this class.
 * <p> UseCorrespondingPoints is always visible, except when there is not
 * <p> transferfid.coord file.
 * <p>
 * <p> Revision 3.21  2006/05/16 21:37:50  sueh
 * <p> bug# 856 Added useCorrespondingPoints and useList.  Added isChanged(),
 * <p> which looks at useCorrespondingPoints.
 * <p>
 * <p> Revision 3.20  2006/04/28 21:05:06  sueh
 * <p> bug# 787 PanelHeader:  Removed the member variable title, which was
 * <p> not used.
 * <p>
 * <p> Revision 3.19  2006/03/27 21:07:03  sueh
 * <p> bug# 836 Added DialogType to PanelHeader get instances functions so
 * <p> that the buttons in PanelHeader could save themselves.
 * <p>
 * <p> Revision 3.18  2006/03/16 01:59:33  sueh
 * <p> bug# 828 SolvematchPanel doesn't need to implement InitialCombineFields.
 * <p>
 * <p> Revision 3.17  2006/01/12 17:38:03  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 3.16  2006/01/03 23:53:52  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox.  Converted JRadioButton's
 * <p> toRadioButton.
 * <p>
 * <p> Revision 3.15  2005/11/14 22:21:15  sueh
 * <p> bug# 762 Made buttonAction() protected.
 * <p>
 * <p> Revision 3.14  2005/10/13 22:36:24  sueh
 * <p> Bug# 532 In synchronized(), always copying all fields
 * <p>
 * <p> Revision 3.13  2005/09/29 19:11:02  sueh
 * <p> bug# 532 Add panel headers to all of the sections in Combine.  Hide the
 * <p> sections in the tabs that are not visible so that the visible tab can become
 * <p> small.  Added an expand() function to each tab to handle the
 * <p> expand/contract requests of the panel header buttons.  Added set and get
 * <p> parameters for ReconScreenState to set and get the state of the panel
 * <p> headers.
 * <p>
 * <p> Revision 3.12  2005/08/27 22:42:22  sueh
 * <p> bug# 532 Changed Autodoc.get() to getInstance().
 * <p>
 * <p> Revision 3.11  2005/08/12 00:00:55  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Prevent context menu from popping up when button is
 * <p> disabled.  Get rid of duplicate code by running the 3dmods from a private
 * <p> function called run3dmod(String, Run3dmodMenuOptions).  It can be
 * <p> called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and the
 * <p> action function.
 * <p>
 * <p> Revision 3.10  2005/08/09 21:00:01  sueh
 * <p> bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * <p> Changed 3dmod buttons to Run3dmodButton.  No longer inheriting
 * <p> MultiLineButton from JButton.
 * <p>
 * <p> Revision 3.9  2005/04/25 21:39:05  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.8  2005/03/01 20:59:58  sueh
 * <p> Removed print statement.
 * <p>
 * <p> Revision 3.7  2005/02/24 00:52:18  sueh
 * <p> bug# 600 Removed unnecessary import.
 * <p>
 * <p> Revision 3.6  2005/02/23 01:44:35  sueh
 * <p> bug# 600 Getting solvematch tooltips from autodoc.
 * <p>
 * <p> Revision 3.5  2004/08/31 17:43:01  sueh
 * <p> bug# 542 Calling TomogramCombinationDialog.setBinningWarning(true)
 * <p> when Bin by 2 checkbox is first checked.
 * <p>
 * <p> Revision 3.4  2004/06/17 20:43:50  sueh
 * <p> bug# 472
 * <p>
 * <p> Revision 3.3  2004/06/15 21:37:16  rickg
 * <p> Bug #383 Correct synchronization of solvematch sub-panel
 * <p>
 * <p> Revision 3.2  2004/06/14 23:39:53  rickg
 * <p> Bug #383 Transitioned to using solvematch
 * <p>
 * <p> Revision 3.1  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p> </p>
 */
public final class SolvematchPanel implements Run3dmodButtonContainer, Expandable {

  //private static final String POINT_LIST_A = "Corresponding fiducial list A: ";

  private ApplicationManager applicationManager;

  private JPanel pnlRoot = new JPanel();
  private JPanel pnlBody = new JPanel();
  private JPanel pnlFiducialRadio = new JPanel();
  private JPanel pnlFiducialSelect = new JPanel();
  private ButtonGroup bgFiducialParams = new ButtonGroup();
  private RadioButton rbBothSides = new RadioButton("Fiducials on both sides");
  private RadioButton rbOneSide = new RadioButton(
      "Fiducials on one side, NOT inverted");
  private RadioButton rbOneSideInverted = new RadioButton(
      "Fiducials on one side, inverted");
  private RadioButton rbUseModel = new RadioButton(
      "Use matching models and fiducials");
  private RadioButton rbUseModelOnly = new RadioButton(
      "Use matching models only");

  private JPanel pnlImodMatchModels = new JPanel();
  private CheckBox cbBinBy2 = new CheckBox("Load binned by 2");
  private Run3dmodButton btnImodMatchModels = new Run3dmodButton(
      "<html><b>Create Matching Models in 3dmod</b>", this);
  private LabeledTextField ltfFiducialMatchListA = new LabeledTextField(
      "Corresponding fiducial list A: ");
  private LabeledTextField ltfFiducialMatchListB = new LabeledTextField(
      "Corresponding fiducial list B: ");
  private LabeledTextField ltfUseList = new LabeledTextField(
  "Starting points to use from A: ");

  private LabeledTextField ltfResidulThreshold = new LabeledTextField(
      "Limit on maximum residual: ");
  private CheckBox cbUseCorrespondingPoints = new CheckBox(
      "Specify corresponding points instead of using coordinate file");

  private TomogramCombinationDialog tomogramCombinationDialog;
  private String parentTitle;
  private boolean binningWarning = false;
  private final PanelHeader header;
  private final String headerGroup;

  public SolvematchPanel(TomogramCombinationDialog parent, String title,
      ApplicationManager appMgr, String headerGroup) {
    tomogramCombinationDialog = parent;
    parentTitle = title;
    applicationManager = appMgr;
    this.headerGroup = headerGroup;
    //  Create the fiducial relationship panel
    pnlFiducialRadio
        .setLayout(new BoxLayout(pnlFiducialRadio, BoxLayout.Y_AXIS));

    rbBothSides.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbOneSide.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbOneSideInverted.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbUseModel.setAlignmentX(Component.LEFT_ALIGNMENT);
    bgFiducialParams.add(rbBothSides);
    bgFiducialParams.add(rbOneSide);
    bgFiducialParams.add(rbOneSideInverted);
    bgFiducialParams.add(rbUseModel);
    bgFiducialParams.add(rbUseModelOnly);
    pnlFiducialRadio.add(rbBothSides);
    pnlFiducialRadio.add(rbOneSide);
    pnlFiducialRadio.add(rbOneSideInverted);
    pnlFiducialRadio.add(rbUseModel);
    pnlFiducialRadio.add(rbUseModelOnly);

    pnlImodMatchModels.setLayout(new BoxLayout(pnlImodMatchModels,
        BoxLayout.Y_AXIS));
    pnlImodMatchModels.add(cbBinBy2);
    pnlImodMatchModels.add(btnImodMatchModels.getComponent());
    UIUtilities.setButtonSizeAll(pnlImodMatchModels, UIParameters.INSTANCE
        .getButtonDimension());

    pnlFiducialSelect.setLayout(new BoxLayout(pnlFiducialSelect,
        BoxLayout.X_AXIS));
    UIUtilities.addWithSpace(pnlFiducialSelect, pnlFiducialRadio,
        FixedDim.x20_y0);
    pnlFiducialSelect.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlFiducialSelect.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlFiducialSelect.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlFiducialSelect.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlFiducialSelect.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlFiducialSelect.add(pnlImodMatchModels);

    pnlBody.setLayout(new BoxLayout(pnlBody, BoxLayout.Y_AXIS));
    UIUtilities.addWithSpace(pnlBody, pnlFiducialSelect, FixedDim.x0_y10);
    UIUtilities.addWithYSpace(pnlBody, cbUseCorrespondingPoints);
    UIUtilities.addWithYSpace(pnlBody, ltfUseList.getContainer());
    UIUtilities.addWithYSpace(pnlBody, ltfFiducialMatchListA.getContainer());
    UIUtilities.addWithYSpace(pnlBody, ltfFiducialMatchListB.getContainer());
    UIUtilities.addWithYSpace(pnlBody, ltfResidulThreshold.getContainer());
    pnlRoot.setBorder(BorderFactory.createEtchedBorder());
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    header = PanelHeader.getInstance("Solvematch Parameters", this, parent
        .getDialogType());
    pnlRoot.add(header.getContainer());
    pnlRoot.add(pnlBody);
    setToolTipText();

    if (!appMgr.coordFileExists()) {
      cbUseCorrespondingPoints.setSelected(true);
      cbUseCorrespondingPoints.setVisible(false);
      ltfUseList.setVisible(false);
      updateUseCorrespondingPoints();
    }

    //  Bind the ui elements to their listeners
    SolvematchPanelActionListener actionListener = new SolvematchPanelActionListener(
        this);
    btnImodMatchModels.addActionListener(actionListener);
    cbBinBy2.addActionListener(actionListener);
    cbUseCorrespondingPoints.addActionListener(actionListener);
    RBFiducialListener rbFiducialListener = new RBFiducialListener(this);
    rbBothSides.addActionListener(rbFiducialListener);
    rbOneSide.addActionListener(rbFiducialListener);
    rbOneSideInverted.addActionListener(rbFiducialListener);
    rbUseModel.addActionListener(rbFiducialListener);
    rbUseModelOnly.addActionListener(rbFiducialListener);
  }

  public Container getContainer() {
    return pnlRoot;
  }

  // FIXME there are current two ways to get the parameters into and out of the
  // panel.  Does this need to be the case?  It seem redundant.
  void setParameters(ConstCombineParams combineParams) {
    if (combineParams.getFiducialMatch() == FiducialMatch.BOTH_SIDES) {
      rbBothSides.setSelected(true);
    }
    if (combineParams.getFiducialMatch() == FiducialMatch.ONE_SIDE) {
      rbOneSide.setSelected(true);
    }
    if (combineParams.getFiducialMatch() == FiducialMatch.ONE_SIDE_INVERTED) {
      rbOneSideInverted.setSelected(true);
    }
    if (combineParams.getFiducialMatch() == FiducialMatch.USE_MODEL) {
      rbUseModel.setSelected(true);
    }
    if (combineParams.getFiducialMatch() == FiducialMatch.USE_MODEL_ONLY) {
      rbUseModelOnly.setSelected(true);
    }
    ltfFiducialMatchListA.setText(combineParams.getFiducialMatchListA());
    ltfFiducialMatchListB.setText(combineParams.getFiducialMatchListB());
    ltfUseList.setText(combineParams.getUseList());
    if (cbUseCorrespondingPoints.isVisible()) {
      cbUseCorrespondingPoints.setSelected(!combineParams.isTransfer());
      updateUseCorrespondingPoints();
    }
  }

  public void expand(ExpandButton button) {
    if (header != null && header.equalsOpenClose(button)) {
      pnlBody.setVisible(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(AxisID.ONLY, applicationManager);
  }

  public void setVisible(boolean visible) {
    pnlRoot.setVisible(visible);
  }

  PanelHeader getHeader() {
    return header;
  }

  /**
   * Get the parameters from the ui and filling in the appropriate fields in the
   * CombineParams object 
   * @param combineParams
   */
  void getParameters(CombineParams combineParams) {
    if (rbBothSides.isSelected()) {
      combineParams.setFiducialMatch(FiducialMatch.BOTH_SIDES);
    }
    if (rbOneSide.isSelected()) {
      combineParams.setFiducialMatch(FiducialMatch.ONE_SIDE);
    }
    if (rbOneSideInverted.isSelected()) {
      combineParams.setFiducialMatch(FiducialMatch.ONE_SIDE_INVERTED);
    }
    if (rbUseModel.isSelected()) {
      combineParams.setFiducialMatch(FiducialMatch.USE_MODEL);
    }
    if (rbUseModelOnly.isSelected()) {
      combineParams.setFiducialMatch(FiducialMatch.USE_MODEL_ONLY);
    }
    combineParams.setTransfer(!cbUseCorrespondingPoints.isSelected());
    combineParams.setFiducialMatchListA(ltfFiducialMatchListA.getText());
    combineParams.setFiducialMatchListB(ltfFiducialMatchListB.getText());
    combineParams.setUseList(ltfUseList.getText());
  }

  void setParameters(ConstSolvematchParam solvematchParam) {
    setSurfacesOrModels(solvematchParam.getSurfacesOrModel());
    if (solvematchParam.isMatchBToA()) {
      ltfFiducialMatchListA.setText(solvematchParam.getToCorrespondenceList()
          .toString());
      ltfFiducialMatchListB.setText(solvematchParam.getFromCorrespondenceList()
          .toString());
    }
    else {
      ltfFiducialMatchListB.setText(solvematchParam.getToCorrespondenceList()
          .toString());
      ltfFiducialMatchListA.setText(solvematchParam.getFromCorrespondenceList()
          .toString());
    }
    ltfResidulThreshold.setText(solvematchParam.getMaximumResidual());
    ltfUseList.setText(solvematchParam.getUsePoints().toString());
  }

  void visibleResidual(boolean state) {
    ltfResidulThreshold.setVisible(state);
  }

  /**
   * Get the parameters from the ui and filling in the appropriate fields in the
   * SolvematchParam object 
   * @param combineParams
   */
  void getParameters(SolvematchParam solvematchParam) {
    solvematchParam.setSurfacesOrModel(getSurfacesOrModels());

    if (solvematchParam.isMatchBToA()) {
      solvematchParam.setToCorrespondenceList(ltfFiducialMatchListA.getText());
      solvematchParam
          .setFromCorrespondenceList(ltfFiducialMatchListB.getText());
    }
    else {
      solvematchParam
          .setFromCorrespondenceList(ltfFiducialMatchListA.getText());
      solvematchParam.setToCorrespondenceList(ltfFiducialMatchListB.getText());
    }
    solvematchParam.setMaximumResidual(ltfResidulThreshold.getText());
    solvematchParam.setTransferCoordinateFile(cbUseCorrespondingPoints.isSelected());
    solvematchParam.setUsePoints(ltfUseList.getText());
  }

  /**
   * 
   * @return
   */
  public FiducialMatch getSurfacesOrModels() {
    if (rbBothSides.isSelected()) {
      return FiducialMatch.BOTH_SIDES;
    }
    if (rbOneSide.isSelected()) {
      return FiducialMatch.ONE_SIDE;
    }
    if (rbOneSideInverted.isSelected()) {
      return FiducialMatch.ONE_SIDE_INVERTED;
    }
    if (rbUseModel.isSelected()) {
      return FiducialMatch.USE_MODEL;
    }
    if (rbUseModelOnly.isSelected()) {
      return FiducialMatch.USE_MODEL_ONLY;
    }
    return FiducialMatch.NOT_SET;
  }

  public void setSurfacesOrModels(FiducialMatch value) {
    if (value == FiducialMatch.USE_MODEL_ONLY) {
      rbUseModelOnly.setSelected(true);
    }
    if (value == FiducialMatch.ONE_SIDE_INVERTED) {
      rbOneSideInverted.setSelected(true);
    }
    if (value == FiducialMatch.USE_MODEL) {
      rbUseModel.setSelected(true);
    }
    if (value == FiducialMatch.ONE_SIDE) {
      rbOneSide.setSelected(true);
    }
    if (value == FiducialMatch.BOTH_SIDES) {
      rbBothSides.setSelected(true);
    }
    updateUseFiducialModel();
  }

  public boolean isBinBy2() {
    return cbBinBy2.isSelected();
  }

  public void setBinBy2(boolean state) {
    cbBinBy2.setSelected(state);
  }
  
  void setUseList(String useList) {
   ltfUseList.setText(useList);
  }

  public void setFiducialMatchListA(String fiducialMatchListA) {
    ltfFiducialMatchListA.setText(fiducialMatchListA);
  }
  
  String getUseList() {
    return ltfUseList.getText();
  }

  public String getFiducialMatchListA() {
    return ltfFiducialMatchListA.getText();
  }

  public void setFiducialMatchListB(String fiducialMatchListB) {
    ltfFiducialMatchListB.setText(fiducialMatchListB);
  }

  public String getFiducialMatchListB() {
    return ltfFiducialMatchListB.getText();
  }

  public void run3dmod(Run3dmodButton button, Run3dmodMenuOptions menuOptions) {
    run3dmod(button.getActionCommand(), menuOptions);
  }

  public void run3dmod(String command, Run3dmodMenuOptions menuOptions) {
    if (command.equals(btnImodMatchModels.getActionCommand())) {
      applicationManager.imodMatchingModel(cbBinBy2.isSelected(), menuOptions);
    }
  }

  //  Action functions for setup panel buttons
  protected void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(cbUseCorrespondingPoints.getActionCommand())) {
      updateUseCorrespondingPoints();
    }
    //  Synchronize this panel with the others
    tomogramCombinationDialog.synchronize(parentTitle, true);
    if (command.equals(cbBinBy2.getActionCommand())) {
      if (!binningWarning && cbBinBy2.isSelected()) {
        tomogramCombinationDialog.setBinningWarning(true);
        binningWarning = true;
      }
    }
    else {
      run3dmod(command, new Run3dmodMenuOptions());
    }
  }

  /**
   * Manage fiducial radio button action
   * 
   * @param event
   */
  protected void rbFiducialAction(ActionEvent event) {
    updateUseFiducialModel();
  }

  /**
   * Enable/disable the matching model button
   */
  void updateUseFiducialModel() {
    boolean enable = rbUseModel.isSelected() || rbUseModelOnly.isSelected();
    btnImodMatchModels.setEnabled(enable);
    cbBinBy2.setEnabled(enable);
  }

  void updateUseCorrespondingPoints() {
    if (cbUseCorrespondingPoints.isSelected()) {
      ltfFiducialMatchListA.setVisible(true);
      ltfFiducialMatchListB.setVisible(true);
      ltfUseList.setVisible(false);
    }
    else {
      ltfFiducialMatchListA.setVisible(false);
      ltfFiducialMatchListB.setVisible(false);
      ltfUseList.setVisible(true);
    }
    tomogramCombinationDialog.updateDisplay();
  }

  public boolean isUseCorrespondingPoints() {
    return cbUseCorrespondingPoints.isSelected();
  }

  public void setUseCorrespondingPoints(boolean selected) {
    cbUseCorrespondingPoints.setSelected(selected);
    updateUseCorrespondingPoints();
  }

  /**
   * Manage the matching models button
   */
  class SolvematchPanelActionListener implements ActionListener {

    SolvematchPanel adaptee;

    public SolvematchPanelActionListener(SolvematchPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }
  }

  class RBFiducialListener implements ActionListener {

    SolvematchPanel adaptee;

    public RBFiducialListener(SolvematchPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.rbFiducialAction(event);
    }
  }

  /**
   * Initialize the tooltip text
   */
  private void setToolTipText() {
    String text;
    Section section;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    Autodoc autodoc = null;
    try {
      autodoc = Autodoc.getInstance(Autodoc.SOLVEMATCH, AxisID.ONLY);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME,
        SolvematchParam.SURFACE_OR_USE_MODELS);
    
    cbUseCorrespondingPoints.setToolTipText(tooltipFormatter.setText("Check to use the points in A and B in the transferfid log file.  "+"Leave unchecked to use transferfid.coord.").format());

    if (section != null) {
      rbBothSides.setToolTipText(tooltipFormatter.setText(
          EtomoAutodoc.getTooltip(section, SolvematchParam.BOTH_SIDES_OPTION))
          .format());
      rbOneSide.setToolTipText(tooltipFormatter.setText(
          EtomoAutodoc.getTooltip(section, SolvematchParam.ONE_SIDE_OPTION))
          .format());
      rbOneSideInverted.setToolTipText(tooltipFormatter.setText(
          EtomoAutodoc.getTooltip(section,
              SolvematchParam.ONE_SIDE_INVERTED_OPTION)).format());
      rbUseModel.setToolTipText(tooltipFormatter.setText(
          EtomoAutodoc.getTooltip(section, SolvematchParam.USE_MODEL_OPTION))
          .format());
      rbUseModelOnly.setToolTipText(tooltipFormatter.setText(
          EtomoAutodoc.getTooltip(section,
              SolvematchParam.USE_MODEL_ONLY_OPTION)).format());
    }

    text = "Use binning by 2 when opening matching models to allow the two 3dmods "
        + "to fit into the computer's memory.";
    cbBinBy2.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Create models of corresponding points.";
    btnImodMatchModels.setToolTipText(tooltipFormatter.setText(text).format());

    ltfFiducialMatchListA.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc
            .getTooltip(autodoc, SolvematchParam.TO_CORRESPONDENCE_LIST))
        .format());
    ltfFiducialMatchListB.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            SolvematchParam.FROM_CORRESPONDENCE_LIST)).format());
    ltfUseList.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc
            .getTooltip(autodoc, SolvematchParam.USE_POINTS))
        .format());
    ltfResidulThreshold.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, SolvematchParam.MAXIMUM_RESIDUAL))
        .format());
  }
}