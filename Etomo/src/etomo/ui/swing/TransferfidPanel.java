package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.TransferfidParam;
import etomo.process.ImodManager;
import etomo.type.AxisID;
import etomo.type.BaseScreenState;
import etomo.type.DialogType;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.util.DatasetFiles;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002, 2003, 2004, 2005</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 */
final class TransferfidPanel implements Expandable, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private final EtomoPanel panelTransferfid = new EtomoPanel();
  private final JPanel panelTransferfidBody = new JPanel();
  private final CheckBox cbRunMidas = new CheckBox("Run midas");
  private final LabeledTextField ltfCenterViewA = new LabeledTextField(FieldType.INTEGER,
      "Center view A: ");
  private final LabeledTextField ltfCenterViewB = new LabeledTextField(FieldType.INTEGER,
      "Center view B: ");
  private final LabeledTextField ltfNumberViews = new LabeledTextField(FieldType.INTEGER,
      "Number of views in the search: ");
  private final CheckBox cbMirrorInX = new CheckBox("Mirror one image around the X axis");
  private final EtomoPanel panelSearchDirection = new EtomoPanel();
  private final ButtonGroup bgSearchDirection = new ButtonGroup();
  private final RadioButton rbSearchBoth = new RadioButton("Both directions");
  private final RadioButton rbSearchPlus90 = new RadioButton("+90 (CCW) only");
  private final RadioButton rbSearchMinus90 = new RadioButton("-90 (CW) only");
  private final TransferfidPanelActionListener actionListener = new TransferfidPanelActionListener(
      this);
  private final Run3dmodButton btn3dmodSeed = Run3dmodButton.get3dmodInstance(
      "Open Seed Model", this);

  private final PanelHeader header;
  private final Run3dmodButton buttonTransferfid;
  private final AxisID axisID;
  private final ApplicationManager manager;
  private final DialogType dialogType;

  private TransferfidPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, GlobalExpandButton globalAdvancedButton) {
    this.dialogType = dialogType;
    this.manager = manager;
    this.axisID = axisID;
    header = PanelHeader.getAdvancedBasicInstance("Transfer Fiducials", this, dialogType,
        globalAdvancedButton);
    panelTransferfidBody.setLayout(new BoxLayout(panelTransferfidBody, BoxLayout.Y_AXIS));
    JPanel pnlRunMidas = new JPanel();
    pnlRunMidas.setLayout(new BoxLayout(pnlRunMidas, BoxLayout.X_AXIS));
    pnlRunMidas.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlRunMidas.add(cbRunMidas);
    pnlRunMidas.add(Box.createHorizontalGlue());
    cbRunMidas.setAlignmentX(Component.RIGHT_ALIGNMENT);
    panelTransferfidBody.add(pnlRunMidas);

    // Add a horizontal strut to keep the panel a minimum size
    panelTransferfidBody.add(Box.createHorizontalStrut(300));
    panelTransferfidBody.add(ltfCenterViewA.getContainer());
    panelTransferfidBody.add(ltfCenterViewB.getContainer());
    panelTransferfidBody.add(ltfNumberViews.getContainer());

    bgSearchDirection.add(rbSearchBoth.getAbstractButton());
    bgSearchDirection.add(rbSearchPlus90.getAbstractButton());
    bgSearchDirection.add(rbSearchMinus90.getAbstractButton());
    JPanel opnlSearchDirection = new JPanel();
    opnlSearchDirection.setLayout(new BoxLayout(opnlSearchDirection, BoxLayout.X_AXIS));
    opnlSearchDirection.setAlignmentX(Box.CENTER_ALIGNMENT);
    opnlSearchDirection.add(Box.createHorizontalGlue());
    opnlSearchDirection.add(panelSearchDirection);
    opnlSearchDirection.add(Box.createHorizontalGlue());
    panelSearchDirection.setLayout(new BoxLayout(panelSearchDirection, BoxLayout.Y_AXIS));
    panelSearchDirection.setBorder(new EtchedBorder("Search Direction").getBorder());
    panelSearchDirection.add(rbSearchBoth.getComponent());
    panelSearchDirection.add(rbSearchPlus90.getComponent());
    panelSearchDirection.add(rbSearchMinus90.getComponent());
    panelSearchDirection.setAlignmentX(Component.CENTER_ALIGNMENT);
    panelTransferfidBody.add(opnlSearchDirection);
    cbMirrorInX.setAlignmentX(Component.RIGHT_ALIGNMENT);
    panelTransferfidBody.add(cbMirrorInX);
    panelTransferfidBody.add(Box.createRigidArea(FixedDim.x0_y5));

    JPanel pnlTransferfid = new JPanel();
    pnlTransferfid.setLayout(new BoxLayout(pnlTransferfid, BoxLayout.X_AXIS));
    pnlTransferfid.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlTransferfid.add(Box.createHorizontalGlue());
    buttonTransferfid = (Run3dmodButton) manager.getProcessResultDisplayFactory(axisID)
        .getTransferFiducials();
    buttonTransferfid.setContainer(this);
    buttonTransferfid.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonTransferfid.setSize();
    btn3dmodSeed.setSize();
    buttonTransferfid.setDeferred3dmodButton(btn3dmodSeed);
    pnlTransferfid.add(buttonTransferfid.getComponent());
    pnlTransferfid.add(Box.createHorizontalGlue());
    pnlTransferfid.add(btn3dmodSeed.getComponent());
    pnlTransferfid.add(Box.createHorizontalGlue());
    panelTransferfidBody.add(pnlTransferfid);
    panelTransferfidBody.add(Box.createRigidArea(FixedDim.x0_y5));
    panelTransferfid.setLayout(new BoxLayout(panelTransferfid, BoxLayout.Y_AXIS));
    panelTransferfid.setBorder(BorderFactory.createEtchedBorder());
    panelTransferfid.add(header);
    panelTransferfid.add(panelTransferfidBody);

    setToolTipText();
  }

  static TransferfidPanel getInstance(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, GlobalExpandButton globalAdvancedButton) {
    TransferfidPanel instance = new TransferfidPanel(manager, axisID, dialogType,
        globalAdvancedButton);
    instance.addListeners();
    return instance;
  }

  public void action(Run3dmodButton button, Run3dmodMenuOptions menuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(), menuOptions);
  }

  /**
   * Executes the action associated with command.  Deferred3dmodButton is null
   * if it comes from the dialog's ActionListener.  Otherwise is comes from a
   * Run3dmodButton which called action(Run3dmodButton, Run3dmoMenuOptions).  In
   * that case it will be null unless it was set in the Run3dmodButton.
   * @param command
   * @param deferred3dmodButton
   * @param run3dmodMenuOptions
   */
  private void action(final String command, Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(buttonTransferfid.getActionCommand())) {
      manager.transferfid(axisID, buttonTransferfid, null, deferred3dmodButton,
          run3dmodMenuOptions, dialogType);
    }
    else if (command.equals(btn3dmodSeed.getActionCommand())) {
      manager.imodSeedModel(axisID, run3dmodMenuOptions, btn3dmodSeed,
          ImodManager.COARSE_ALIGNED_KEY, DatasetFiles.getSeedFileName(manager, axisID),
          DatasetFiles.getRawTiltFile(manager, axisID), dialogType);
    }
  }

  Container getContainer() {
    return panelTransferfid;
  }

  public void expand(GlobalExpandButton button) {
  }

  public void expand(ExpandButton button) {
    if (header.equalsOpenClose(button)) {
      panelTransferfidBody.setVisible(button.isExpanded());
    }
    else if (header.equalsAdvancedBasic(button)) {
      updateAdvanced(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  private void setup() {
  }

  void setVisible(final boolean visible) {
    panelTransferfid.setVisible(visible);
  }

  /**
   * Set the values of the panel using a TransferfidParam parameter
   * object
   */
  void setParameters() {
    TransferfidParam params = new TransferfidParam(manager, axisID);
    params.initialize();
    if (axisID == AxisID.SECOND) {
      manager.getMetaData().getTransferfidBFields(params);
    }
    else {
      manager.getMetaData().getTransferfidAFields(params);
    }
    cbRunMidas.setSelected(params.getRunMidas().is());
    ltfCenterViewA.setText(params.getCenterViewA().toString());
    ltfCenterViewB.setText(params.getCenterViewB().toString());
    ltfNumberViews.setText(params.getNumberViews().toString());

    if (params.getSearchDirection().isNull()) {
      rbSearchBoth.setSelected(true);
    }
    if (params.getSearchDirection().isNegative()) {
      rbSearchMinus90.setSelected(true);
    }
    if (params.getSearchDirection().isPositive()) {
      rbSearchPlus90.setSelected(true);
    }
    cbMirrorInX.setSelected(params.getMirrorInX().is());
  }

  void setParameters(ReconScreenState screenState) {
    header.setButtonStates(screenState);
  }

  void getParameters(BaseScreenState screenState) {
    header.getButtonStates(screenState);
  }

  boolean getParameters(final boolean doValidation) {
    return getParameters(new TransferfidParam(manager, axisID), doValidation);
  }

  /**
   * Get the values from the panel filling in the TransferfidParam object
   */
  boolean getParameters(TransferfidParam params, final boolean doValidation) {
    try {
      params.setRunMidas(cbRunMidas.isSelected());
      params.setCenterViewA(ltfCenterViewA.getText(doValidation));
      params.setCenterViewB(ltfCenterViewB.getText(doValidation));
      if (rbSearchBoth.isSelected()) {
        params.getSearchDirection().reset();
      }
      if (rbSearchPlus90.isSelected()) {
        params.setSearchDirection(1);
      }
      if (rbSearchMinus90.isSelected()) {
        params.setSearchDirection(-1);
      }
      params.setNumberViews(ltfNumberViews.getText(doValidation));
      params.setMirrorInX(cbMirrorInX.isSelected());
      if (axisID == AxisID.SECOND) {
        manager.getMetaData().setTransferfidBFields(params);
      }
      else {
        manager.getMetaData().setTransferfidAFields(params);
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  private void addListeners() {
    buttonTransferfid.addActionListener(actionListener);
    btn3dmodSeed.addActionListener(actionListener);
  }

  void done() {
    buttonTransferfid.removeActionListener(actionListener);
  }

  void updateAdvanced(boolean isAdvanced) {
    ltfCenterViewA.setVisible(isAdvanced);
    ltfCenterViewB.setVisible(isAdvanced);
    ltfNumberViews.setVisible(isAdvanced);
    panelSearchDirection.setVisible(isAdvanced);
    cbMirrorInX.setVisible(isAdvanced);
  }

  void setEnabled(boolean isEnabled) {
    buttonTransferfid.setEnabled(isEnabled);
    cbRunMidas.setEnabled(isEnabled);
    ltfCenterViewA.setEnabled(isEnabled);
    ltfCenterViewB.setEnabled(isEnabled);
    ltfNumberViews.setEnabled(isEnabled);
    rbSearchBoth.setEnabled(isEnabled);
    rbSearchPlus90.setEnabled(isEnabled);
    rbSearchMinus90.setEnabled(isEnabled);
    cbMirrorInX.setEnabled(isEnabled);
  }

  // ToolTip string setup
  private void setToolTipText() {
    cbRunMidas.setToolTipText("Run Midas to adjust initial alignment manually.");
    ltfCenterViewA
        .setToolTipText("View from A around which to search for the best pair of views.");
    ltfCenterViewB
        .setToolTipText("View from B around which to search for the best pair of views.");
    ltfNumberViews
        .setToolTipText("Number of views from each axis to consider in searching for best pair.");
    rbSearchBoth
        .setToolTipText("Try both +90 and -90 degree rotations in searching for best pair of "
            + "views.");
    rbSearchPlus90
        .setToolTipText("Try only +90 degree rotations in searching for best pair of views.");
    rbSearchMinus90
        .setToolTipText("Try only -90 degree rotations in searching for best pair of views.");
    buttonTransferfid
        .setToolTipText("Run Transferfid to make a seed model for this axis from fiducial model for "
            + "the other axis.");
    cbMirrorInX
        .setToolTipText("Mirror one image around the X axis before rotating by 90 degrees.");
  }

  private final class TransferfidPanelActionListener implements ActionListener {
    private final TransferfidPanel adaptee;

    private TransferfidPanelActionListener(final TransferfidPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}

/**
 * <p> $Log$
 * <p> Revision 1.3  2011/02/22 21:41:30  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:24:13  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.21  2010/03/03 05:10:14  sueh
 * <p> bug# 1311 Added setVisible.
 * <p>
 * <p> Revision 3.20  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.19  2009/01/20 20:32:54  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 3.18  2008/05/28 02:52:04  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 3.17  2008/05/13 23:09:03  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 3.16  2008/05/07 00:28:15  sueh
 * <p> bug#847 Running deferred 3dmods by using the button that usually calls
 * <p> them.  This avoids having to duplicate the calls and having a
 * <p> startNextProcess function just for 3dmods.  This requires that the 3dmod
 * <p> button be passed to the function that starts the process.  Make transfer
 * <p> fid panel responsible for its own actions.
 * <p>
 * <p> Revision 3.15  2007/09/10 20:43:59  sueh
 * <p> bug# 925 Should only load button states once.  Changed
 * <p> ProcessResultDisplayFactory to load button states immediately, so removing
 * <p> button state load in the dialogs.
 * <p>
 * <p> Revision 3.14  2007/03/07 21:16:45  sueh
 * <p> bug# 981 Turned RadioButton into a wrapper rather then a child of JRadioButton,
 * <p> because it is getting more complicated.
 * <p>
 * <p> Revision 3.13  2007/02/09 00:54:58  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.12  2006/07/04 18:48:01  sueh
 * <p> bug# 893 Added updateAdvanced(boolean) to change the header when the
 * <p> advanced button is pressed.
 * <p>
 * <p> Revision 3.11  2006/06/16 15:27:07  sueh
 * <p> bug# 734 Added open/close and adv/basic buttons.
 * <p>
 * <p> Revision 3.10  2006/06/15 16:20:03  sueh
 * <p> bug# 873 Added tooltip for mirrorInX.
 * <p>
 * <p> Revision 3.9  2006/06/14 21:25:33  sueh
 * <p> bug# 873 Added cbMirrorInX.
 * <p>
 * <p> Revision 3.8  2006/02/06 21:22:53  sueh
 * <p> bug# 521 Getting toggle buttons through ProcessResultDisplayFactory.
 * <p>
 * <p> Revision 3.7  2006/01/26 22:09:09  sueh
 * <p> bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * <p> the buttons turned on each they are run, unless the process fails or is
 * <p> killed.
 * <p>
 * <p> Revision 3.6  2006/01/04 00:02:28  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox.  Converted
 * <p> JRadioButton's to RadioButton.
 * <p>
 * <p> Revision 3.5  2005/08/10 20:48:38  sueh
 * <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * <p> of MultiLineButton.
 * <p>
 * <p> Revision 3.4  2005/07/29 00:54:52  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.3  2005/06/01 21:28:58  sueh
 * <p> bug# 667 Getting meta data from the manager instead of EtomoDirector.
 * <p>
 * <p> Revision 3.2  2005/01/26 00:07:10  sueh
 * <p> Removed script oriented functionality from EtomoNumber.
 * <p>
 * <p> Revision 3.1  2005/01/22 04:08:36  sueh
 * <p> bug# 509, bug# 591  In getParameters(), set transferfid fields in MetaData
 * <p> after updating the TransferfidParam parameter.  In setParameters(),
 * <p> create the TransferfidParam instance, initialize it, copy fields from
 * <p> MetaData, and then set the screen fields.
 * <p>
 */

