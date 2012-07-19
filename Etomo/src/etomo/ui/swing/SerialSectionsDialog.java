package etomo.ui.swing;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.AutoAlignmentController;
import etomo.SerialSectionsManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.MidasParam;
import etomo.comscript.XftoxgParam;
import etomo.type.AxisID;
import etomo.type.ConstSerialSectionsMetaData;
import etomo.type.DialogType;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.SerialSectionsMetaData;
import etomo.type.ViewType;
import etomo.ui.AutoAlignmentDisplay;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;
import etomo.util.Utilities;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public final class SerialSectionsDialog implements ContextMenu, Run3dmodButtonContainer,
    AutoAlignmentDisplay {
  public static final String rcsid = "$Id:$";

  private static final DialogType DIALOG_TYPE = DialogType.SERIAL_SECTIONS;

  private final JPanel pnlRoot = new JPanel();
  private final JPanel[] pnlTabArray = new JPanel[Tab.NUM_TABS];
  private final JPanel[] pnlTabBodyArray = new JPanel[Tab.NUM_TABS];
  private final TabbedPane tabPane = new TabbedPane();
  private final CheckBox cbVerySloppyMontage = new CheckBox("Treat as very sloppy blend");
  private final Run3dmodButton btnPreblend = Run3dmodButton.getDeferred3dmodInstance(
      "Run Initial Blend", this);
  private final Run3dmodButton btn3dmodPreblend = Run3dmodButton.get3dmodInstance(
      "Open Initial Blend Result", this);
  private final MultiLineButton btnFixEdges = new MultiLineButton("Fix Edges With Midas");
  private final MultiLineButton btn3dmodAlignedStack = Run3dmodButton.get3dmodInstance(
      "Open Aligned Stack", this);
  private final ButtonGroup bgXftoxgAlignment = new ButtonGroup();
  private final RadioButton rbNoOptions = new RadioButton(
      "Local fitting (retain trends)", bgXftoxgAlignment);
  private final RadioButton rbHybridFitsTranslations = new RadioButton(
      "Remove trends in translation", XftoxgParam.HybridFits.TRANSLATIONS,
      bgXftoxgAlignment);
  private final RadioButton rbHybridFitsTranslationsRotations = new RadioButton(
      "Remove trends in translation & rotation",
      XftoxgParam.HybridFits.TRANSLATIONS_ROTATIONS, bgXftoxgAlignment);
  private final RadioButton rbNumberToFitGlobalAlignment = new RadioButton(
      "Global transforms (remove all trends)", XftoxgParam.NumberToFit.GLOBAL_ALIGNMENT,
      bgXftoxgAlignment);
  private final Run3dmodButton btnMakeStack = Run3dmodButton.getDeferred3dmodInstance(
      "Make Stack", this);
  private final Run3dmodButton btn3dmodStack = Run3dmodButton.get3dmodInstance(
      "Open Stack", this);
  private CheckBox cbReferenceSection = new CheckBox("Reference section for alignment: ");
  private Spinner spReferenceSection = Spinner.getInstance(cbReferenceSection.getName());
  private LabeledTextField ltfSizeX = new LabeledTextField("Size in X: ");
  private LabeledTextField ltfSizeY = new LabeledTextField("Y: ");
  private LabeledTextField ltfShiftX = new LabeledTextField("Shift in X: ");
  private LabeledTextField ltfShiftY = new LabeledTextField("Y: ");
  private Spinner spBinning = Spinner.getLabeledInstance("Binning: ", 1, 1, 8);
  private CheckBox cbFillWithZero = new CheckBox("Fill empty areas with 0");
  private CheckTextField ctfRobustFitCriterion = CheckTextField
      .getInstance("Robust fitting");
  private final Spinner spMidasBinning = Spinner.getLabeledInstance("Midas binning: ", 1,
      1, 8);

  private final AutoAlignmentPanel autoAlignmentPanel;
  private final AxisID axisID;
  private final SerialSectionsManager manager;
  private final ConstSerialSectionsMetaData metaData;

  private Tab curTab = null;

  private SerialSectionsDialog(final SerialSectionsManager manager, final AxisID axisID) {
    System.err.println(Utilities.getDateTimeStamp() + "\nDialog: " + DIALOG_TYPE);
    this.manager = manager;
    this.axisID = axisID;
    metaData = manager.getMetaData();
    autoAlignmentPanel = AutoAlignmentPanel.getSerialSectionsInstance(manager);
  }

  public static SerialSectionsDialog getInstance(final SerialSectionsManager manager,
      final AxisID axisID) {
    SerialSectionsDialog instance = new SerialSectionsDialog(manager, axisID);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  public void setAutoAlignmentController(
      final AutoAlignmentController autoAlignmentController) {
    autoAlignmentPanel.setController(autoAlignmentController);
  }

  private void createPanel() {
    // panels
    JPanel pnlInitialBlendButtons = new JPanel();
    JPanel pnlVerySloppyBlend = new JPanel();
    JPanel pnlOpenAlignedStack = new JPanel();
    JPanel pnlXftoxgAlignment = new JPanel();
    JPanel pnlXftoxgAlignmentX = new JPanel();
    JPanel pnlReferenceSection = new JPanel();
    JPanel pnlSize = new JPanel();
    JPanel pnlShift = new JPanel();
    JPanel pnlMakeStackA = new JPanel();
    JPanel pnlMakeStackButtons = new JPanel();
    JPanel pnlRobustFitCriterion = new JPanel();
    JPanel pnlMidasBinning =new JPanel();
    // init
    for (int i = 0; i < Tab.NUM_TABS; i++) {
      pnlTabArray[i] = new JPanel();
      pnlTabBodyArray[i] = new JPanel();
      tabPane.add(Tab.getInstance(i).toString(), pnlTabArray[i]);
    }
    ViewType viewType = metaData.getViewType();
    tabPane.setEnabledAt(Tab.INITIAL_BLEND.index, viewType == ViewType.MONTAGE);
    curTab = Tab.getDefaultInstance(viewType);
    tabPane.setSelectedIndex(curTab.index);
    ctfRobustFitCriterion.setText("1");
    btnPreblend.setSize();
    btnPreblend.setDeferred3dmodButton(btn3dmodPreblend);
    btn3dmodPreblend.setSize();
    btnFixEdges.setSize();
    btn3dmodAlignedStack.setSize();
    // Set the maximum reference section.
    File stack = new File(metaData.getStackAbsolutePath());
    MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(),
        stack.getName(), axisID);
    try {
      header.read(manager);
      spReferenceSection.setMax(header.getNSections());
    }
    catch (IOException e) {
      e.printStackTrace();
      cbReferenceSection.setEnabled(false);
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to read" + stack.getName()
          + "\n" + e.getMessage(), "File Read Error", axisID);
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      cbReferenceSection.setEnabled(false);
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to read" + stack.getName()
          + "\n" + e.getMessage(), "File Read Error", axisID);
    }
    btnMakeStack.setSize();
    btnMakeStack.setDeferred3dmodButton(btn3dmodStack);
    btn3dmodStack.setSize();
    updateDisplay();
    // root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(new BeveledBorder("Serial Sections").getBorder());
    pnlRoot.add(tabPane);
    // tab pane
    pnlTabArray[curTab.index].add(pnlTabBodyArray[curTab.index]);
    // initial blend
    int index = Tab.INITIAL_BLEND.index;
    pnlTabBodyArray[index].setLayout(new BoxLayout(pnlTabBodyArray[index],
        BoxLayout.Y_AXIS));
    pnlTabBodyArray[index].add(pnlVerySloppyBlend);
    pnlTabBodyArray[index].add(pnlRobustFitCriterion);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTabBodyArray[index].add(pnlMidasBinning);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y15));
    pnlTabBodyArray[index].add(pnlInitialBlendButtons);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y5));
    // very sloppy blend
    pnlVerySloppyBlend.setLayout(new BoxLayout(pnlVerySloppyBlend, BoxLayout.X_AXIS));
    pnlVerySloppyBlend.add(cbVerySloppyMontage);
    pnlVerySloppyBlend.add(Box.createHorizontalGlue());
    // RobustFitCriterion
    pnlRobustFitCriterion
        .setLayout(new BoxLayout(pnlRobustFitCriterion, BoxLayout.X_AXIS));
    pnlRobustFitCriterion.add(ctfRobustFitCriterion.getRootComponent());
    pnlRobustFitCriterion.add(Box.createHorizontalStrut(295));
    //midas binning
    pnlMidasBinning.setLayout(new BoxLayout(pnlMidasBinning,BoxLayout.X_AXIS));
    pnlMidasBinning.add(spMidasBinning.getContainer());
    pnlMidasBinning.add(Box.createHorizontalStrut(325));
    // initial blend buttons
    pnlInitialBlendButtons.setLayout(new BoxLayout(pnlInitialBlendButtons,
        BoxLayout.X_AXIS));
    pnlInitialBlendButtons.add(Box.createHorizontalGlue());
    pnlInitialBlendButtons.add(btnPreblend.getComponent());
    pnlInitialBlendButtons.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlInitialBlendButtons.add(btn3dmodPreblend.getComponent());
    pnlInitialBlendButtons.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlInitialBlendButtons.add(btnFixEdges.getComponent());
    pnlInitialBlendButtons.add(Box.createHorizontalGlue());
    // align
    index = Tab.ALIGN.index;
    pnlTabBodyArray[index].setLayout(new BoxLayout(pnlTabBodyArray[index],
        BoxLayout.Y_AXIS));
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y3));
    pnlTabBodyArray[index].add(pnlOpenAlignedStack);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTabBodyArray[index].add(autoAlignmentPanel.getRootComponent());
    // open aligned stack
    pnlOpenAlignedStack.setLayout(new BoxLayout(pnlOpenAlignedStack, BoxLayout.X_AXIS));
    pnlOpenAlignedStack.add(Box.createHorizontalGlue());
    pnlOpenAlignedStack.add(btn3dmodAlignedStack.getComponent());
    pnlOpenAlignedStack.add(Box.createHorizontalGlue());
    // make stack
    index = Tab.MAKE_STACK.index;
    pnlTabBodyArray[index].setLayout(new BoxLayout(pnlTabBodyArray[index],
        BoxLayout.Y_AXIS));
    pnlTabBodyArray[index].add(pnlXftoxgAlignmentX);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y3));
    pnlTabBodyArray[index].add(pnlReferenceSection);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y10));
    pnlTabBodyArray[index].add(pnlSize);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTabBodyArray[index].add(pnlShift);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTabBodyArray[index].add(pnlMakeStackA);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y15));
    pnlTabBodyArray[index].add(pnlMakeStackButtons);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y5));
    // xftoxg alignment X panel
    pnlXftoxgAlignmentX.setLayout(new BoxLayout(pnlXftoxgAlignmentX, BoxLayout.X_AXIS));
    pnlXftoxgAlignmentX.add(pnlXftoxgAlignment);
    pnlXftoxgAlignmentX.add(Box.createHorizontalGlue());
    // xftoxg alignment
    pnlXftoxgAlignment.setLayout(new BoxLayout(pnlXftoxgAlignment, BoxLayout.Y_AXIS));
    pnlXftoxgAlignment.setBorder(new EtchedBorder("Alignment").getBorder());
    pnlXftoxgAlignment.add(rbNoOptions.getComponent());
    pnlXftoxgAlignment.add(rbHybridFitsTranslations.getComponent());
    pnlXftoxgAlignment.add(rbHybridFitsTranslationsRotations.getComponent());
    pnlXftoxgAlignment.add(rbNumberToFitGlobalAlignment.getComponent());
    UIHarness.INSTANCE.pack(axisID, manager);
    // reference section
    pnlReferenceSection.setLayout(new BoxLayout(pnlReferenceSection, BoxLayout.X_AXIS));
    pnlReferenceSection.add(cbReferenceSection);
    pnlReferenceSection.add(spReferenceSection.getContainer());
    // size
    pnlSize.setLayout(new BoxLayout(pnlSize, BoxLayout.X_AXIS));
    pnlSize.add(ltfSizeX.getContainer());
    pnlSize.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlSize.add(ltfSizeY.getContainer());
    // shift
    pnlShift.setLayout(new BoxLayout(pnlShift, BoxLayout.X_AXIS));
    pnlShift.add(ltfShiftX.getContainer());
    pnlShift.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlShift.add(ltfShiftY.getContainer());
    // make stack subpanel 1
    pnlMakeStackA.setLayout(new BoxLayout(pnlMakeStackA, BoxLayout.X_AXIS));
    pnlMakeStackA.add(spBinning.getContainer());
    pnlMakeStackA.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlMakeStackA.add(cbFillWithZero);
    // make stack buttons
    pnlMakeStackButtons.setLayout(new BoxLayout(pnlMakeStackButtons, BoxLayout.X_AXIS));
    pnlMakeStackButtons.add(btnMakeStack.getComponent());
    pnlMakeStackButtons.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlMakeStackButtons.add(btn3dmodStack.getComponent());
  }

  public Container getRootContainer() {
    return pnlRoot;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public DialogType getDialogType() {
    return DialogType.SERIAL_SECTIONS;
  }

  private void addListeners() {
    tabPane.addMouseListener(new GenericMouseAdapter(this));
    tabPane.addChangeListener(new TabChangeListener(this));
    ActionListener listener = new SerialSectionsActionListener(this);
    btnPreblend.addActionListener(listener);
    btn3dmodPreblend.addActionListener(listener);
    btnFixEdges.addActionListener(listener);
    btn3dmodAlignedStack.addActionListener(listener);
    btnMakeStack.addActionListener(listener);
    btn3dmodStack.addActionListener(listener);
  }

  public void updateDisplay() {
    spReferenceSection.setEnabled(cbReferenceSection.isEnabled()
        && cbReferenceSection.isSelected());
  }

  public void enableMidas() {
    autoAlignmentPanel.enableMidas();
  }

  public void getParameters(final SerialSectionsMetaData metaData) {
    metaData.setRobustFitCriterion(ctfRobustFitCriterion.getText());
    metaData.setMidasBinning(spMidasBinning.getValue());
    autoAlignmentPanel.getParameters(metaData.getAutoAlignmentMetaData());
    metaData.setNoOptions(rbNoOptions.isSelected());
    metaData.setHybridFitsTranslations(rbHybridFitsTranslations.isSelected());
    metaData.setHybridFitsTranslationsRotations(rbHybridFitsTranslationsRotations.isSelected());
    metaData.setNumberToFitGlobalAlignment(rbNumberToFitGlobalAlignment.isSelected());
    metaData.setReferenceSection(spReferenceSection.getValue());
  }
  
  public void setParameters(final SerialSectionsMetaData metaData) {
    ctfRobustFitCriterion.setText(metaData.getRobustFitCriterion());
    spMidasBinning.setValue(metaData.getMidasBinning());
    autoAlignmentPanel.setParameters(metaData.getAutoAlignmentMetaData());
    rbNoOptions.setSelected(metaData.isNoOptions());
    rbHybridFitsTranslations.setSelected(metaData.isHybridFitsTranslations());
    rbHybridFitsTranslationsRotations.setSelected(metaData.isHybridFitsTranslationsRotations());
    rbNumberToFitGlobalAlignment.setSelected(metaData.isNumberToFitGlobalAlignment());
    spReferenceSection.setValue(metaData.getReferenceSection());
  }
  
  public void getParameters(final MidasParam param) {
    param.setBinning(spMidasBinning.getValue());
  }

  public void setPreblendParameters(final BlendmontParam param) {
    cbVerySloppyMontage.setSelected(param.isVerySloppyMontage());
    ctfRobustFitCriterion.setSelected(param.isRobustFitCriterion());
    if (ctfRobustFitCriterion.isSelected()) {
      ctfRobustFitCriterion.setText(param.getRobustFitCriterion());
    }
  }

  public void getPreblendParameters(final BlendmontParam param) {
    param.setVerySloppyMontage(cbVerySloppyMontage.isSelected());
    if (ctfRobustFitCriterion.isSelected()) {
      param.setRobustFitCriterion(ctfRobustFitCriterion.getText());
    }
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void action(final String command, Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnPreblend.getActionCommand())) {
      manager.preblend(null, null, deferred3dmodButton, axisID, run3dmodMenuOptions,
          DialogType.SERIAL_SECTIONS);
    }
    else if (command.equals(btn3dmodPreblend.getActionCommand())) {
      manager.imodPreblend(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(btnFixEdges.getActionCommand())) {
      manager.midasFixEdges(axisID, null, null);
    }
    else if (command.equals(btn3dmodAlignedStack.getActionCommand())) {
      manager.imodAlignedStack(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(btnMakeStack.getActionCommand())) {

    }
    else if (command.equals(btn3dmodStack.getActionCommand())) {

    }
  }

  private void changeTab() {
    pnlTabArray[curTab.index].remove(pnlTabBodyArray[curTab.index]);
    curTab = Tab.getInstance(tabPane.getSelectedIndex());
    pnlTabArray[curTab.index].add(pnlTabBodyArray[curTab.index]);
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Processchunks", "3dmod" };
    String[] manPage = { "processchunks.html", "3dmod.html" };
    ContextPopup contextPopup = new ContextPopup(pnlRoot, mouseEvent, manPagelabel,
        manPage, true, manager, axisID);
  }

  private void setTooltips() {
  }

  private static final class SerialSectionsActionListener implements ActionListener {
    private final SerialSectionsDialog dialog;

    private SerialSectionsActionListener(final SerialSectionsDialog dialog) {
      this.dialog = dialog;
    }

    public void actionPerformed(final ActionEvent event) {
      dialog.action(event.getActionCommand(), null, null);
    }
  }

  private static final class TabChangeListener implements ChangeListener {
    private final SerialSectionsDialog dialog;

    public TabChangeListener(final SerialSectionsDialog dialog) {
      this.dialog = dialog;
    }

    public void stateChanged(final ChangeEvent changeEvent) {
      dialog.changeTab();
    }
  }

  private static final class Tab {
    private static final Tab INITIAL_BLEND = new Tab(0, "Initial Blend");
    private static final Tab ALIGN = new Tab(1, "Align");
    private static final Tab MAKE_STACK = new Tab(2, "Make Stack");

    private static final int NUM_TABS = 3;

    private final int index;
    private final String title;

    private Tab(final int index, final String title) {
      this.index = index;
      this.title = title;
    }

    private static Tab getInstance(final int index) {
      if (index == INITIAL_BLEND.index) {
        return INITIAL_BLEND;
      }
      if (index == ALIGN.index) {
        return ALIGN;
      }
      if (index == MAKE_STACK.index) {
        return MAKE_STACK;
      }
      return null;
    }

    private static Tab getDefaultInstance(final ViewType viewType) {
      if (viewType == ViewType.MONTAGE) {
        return INITIAL_BLEND;
      }
      return ALIGN;
    }

    public String toString() {
      return title;
    }
  }
}
