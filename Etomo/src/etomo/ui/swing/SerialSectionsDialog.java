package etomo.ui.swing;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.FileNotFoundException;
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
import etomo.comscript.ConstNewstParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.MidasParam;
import etomo.comscript.NewstParam;
import etomo.comscript.XfalignParam;
import etomo.comscript.XftoxgParam;
import etomo.logic.TomogramTool;
import etomo.logic.TransformsTool;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstSerialSectionsMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.FileType;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.SerialSectionsMetaData;
import etomo.type.ViewType;
import etomo.ui.AutoAlignmentDisplay;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;
import etomo.util.SharedConstants;
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
  private static final String SHIFT_LABEL = "Shift in ";
  private static final String SIZE_LABEL = "Size in ";

  private final JPanel pnlRoot = new JPanel();
  private final JPanel[] pnlTabArray = new JPanel[Tab.NUM_TABS];
  private final JPanel[] pnlTabBodyArray = new JPanel[Tab.NUM_TABS];
  private final TabbedPane tabPane = new TabbedPane();
  private final CheckBox cbVerySloppyMontage = new CheckBox("Treat as very sloppy blend");
  private final Run3dmodButton btnPreblend = Run3dmodButton.getDeferred3dmodInstance(
      "Make Blended Stack", this);
  private final Run3dmodButton btn3dmodPreblend = Run3dmodButton.get3dmodInstance(
      "Open Blended Stack", this);
  private final MultiLineButton btnFixEdges = new MultiLineButton("Fix Edges With Midas");
  private final MultiLineButton btn3dmodPrealign = Run3dmodButton.get3dmodInstance(
      "Open Stack", this);
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
      "Global alignments (remove all trends)", XftoxgParam.NumberToFit.GLOBAL_ALIGNMENT,
      bgXftoxgAlignment);
  private final Run3dmodButton btnAlign = Run3dmodButton.getDeferred3dmodInstance(
      "Make Aligned Stack", this);
  private final Run3dmodButton btn3dmodAlign = Run3dmodButton.get3dmodInstance(
      "Open Aligned Stack", this);
  private CheckBoxSpinner cbsReferenceSection = new CheckBoxSpinner(
      "Reference section for alignment: ");
  private LabeledTextField ltfSizeX = new LabeledTextField(FieldType.INTEGER, SIZE_LABEL
      + "X: ");
  private LabeledTextField ltfSizeY = new LabeledTextField(FieldType.INTEGER, "Y: ");
  private LabeledTextField ltfShiftX = new LabeledTextField(FieldType.FLOATING_POINT,
      SHIFT_LABEL + "X: ");
  private LabeledTextField ltfShiftY = new LabeledTextField(FieldType.FLOATING_POINT,
      "Y: ");
  private Spinner spBinByFactor = Spinner.getLabeledInstance("Binning: ", 1, 1, 8);
  private CheckBox cbFillWithZero = new CheckBox("Fill empty areas with 0");
  private CheckTextField ctfRobustFitCriterion = CheckTextField.getInstance(
      FieldType.FLOATING_POINT, "Robust fitting for criterion: ");
  private final Spinner spMidasBinning = Spinner.getLabeledInstance("Binning: ", 1, 1, 8);
  private final Run3dmodButton btn3dmodRawStack = Run3dmodButton.get3dmodInstance(
      "Open Raw Stack", this);

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
    JPanel pnlSize = new JPanel();
    JPanel pnlShift = new JPanel();
    JPanel pnlMakeStackA = new JPanel();
    JPanel pnlMakeStackButtons = new JPanel();
    JPanel pnlRobustFitCriterion = new JPanel();
    JPanel pnlMidasBinning = new JPanel();
    JPanel pnlMidas = new JPanel();
    JPanel pnlMidasX = new JPanel();
    JPanel pnlFixEdges = new JPanel();
    // init
    for (int i = 0; i < Tab.NUM_TABS; i++) {
      pnlTabArray[i] = new JPanel();
      pnlTabBodyArray[i] = new JPanel();
      tabPane.add(Tab.getInstance(i).toString(), pnlTabArray[i]);
    }
    tabPane.setEnabledAt(Tab.INITIAL_BLEND.index,
        metaData.getViewType() == ViewType.MONTAGE);
    btn3dmodRawStack.setSize();
    btnPreblend.setSize();
    btnPreblend.setDeferred3dmodButton(btn3dmodPreblend);
    btn3dmodPreblend.setSize();
    btnFixEdges.setSize();
    btn3dmodPrealign.setSize();
    // Set the maximum reference section.
    File stack = new File(manager.getPropertyUserDir(), metaData.getStack());
    MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(),
        stack.getName(), axisID);
    try {
      header.read(manager);
      cbsReferenceSection.setMax(header.getNSections());
    }
    catch (IOException e) {
      e.printStackTrace();
      cbsReferenceSection.setCheckBoxEnabled(false);
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to read" + stack.getName()
          + "\n" + e.getMessage(), "File Read Error", axisID);
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      cbsReferenceSection.setCheckBoxEnabled(false);
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to read" + stack.getName()
          + "\n" + e.getMessage(), "File Read Error", axisID);
    }
    btnAlign.setSize();
    btnAlign.setDeferred3dmodButton(btn3dmodAlign);
    btn3dmodAlign.setSize();
    // root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(new BeveledBorder("Serial Sections").getBorder());
    pnlRoot.add(tabPane);
    // initial blend
    int index = Tab.INITIAL_BLEND.index;
    pnlTabBodyArray[index].setLayout(new BoxLayout(pnlTabBodyArray[index],
        BoxLayout.Y_AXIS));
    pnlTabBodyArray[index].add(pnlVerySloppyBlend);
    pnlTabBodyArray[index].add(pnlRobustFitCriterion);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y10));
    pnlTabBodyArray[index].add(pnlInitialBlendButtons);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y10));
    pnlTabBodyArray[index].add(pnlMidasX);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y5));
    // very sloppy blend
    pnlVerySloppyBlend.setLayout(new BoxLayout(pnlVerySloppyBlend, BoxLayout.X_AXIS));
    pnlVerySloppyBlend.add(cbVerySloppyMontage);
    pnlVerySloppyBlend.add(Box.createHorizontalGlue());
    // RobustFitCriterion
    pnlRobustFitCriterion
        .setLayout(new BoxLayout(pnlRobustFitCriterion, BoxLayout.X_AXIS));
    pnlRobustFitCriterion.add(ctfRobustFitCriterion.getRootComponent());
    pnlRobustFitCriterion.add(Box.createHorizontalStrut(215));
    // initial blend buttons
    pnlInitialBlendButtons.setLayout(new BoxLayout(pnlInitialBlendButtons,
        BoxLayout.X_AXIS));
    pnlInitialBlendButtons.add(Box.createHorizontalGlue());
    pnlInitialBlendButtons.add(btn3dmodRawStack.getComponent());
    pnlInitialBlendButtons.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlInitialBlendButtons.add(btnPreblend.getComponent());
    pnlInitialBlendButtons.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlInitialBlendButtons.add(btn3dmodPreblend.getComponent());
    pnlInitialBlendButtons.add(Box.createHorizontalGlue());
    // midas X direction
    pnlMidasX.setLayout(new BoxLayout(pnlMidasX, BoxLayout.X_AXIS));
    pnlMidasX.add(Box.createHorizontalGlue());
    pnlMidasX.add(pnlMidas);
    pnlMidasX.add(Box.createHorizontalGlue());
    // midas
    pnlMidas.setLayout(new BoxLayout(pnlMidas, BoxLayout.Y_AXIS));
    pnlMidas.setBorder(new EtchedBorder("Midas").getBorder());
    pnlMidas.add(pnlMidasBinning);
    pnlMidas.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMidas.add(pnlFixEdges);
    pnlMidas.add(Box.createRigidArea(FixedDim.x0_y5));
    // midas binning
    pnlMidasBinning.setLayout(new BoxLayout(pnlMidasBinning, BoxLayout.X_AXIS));
    pnlMidasBinning.add(Box.createHorizontalGlue());
    pnlMidasBinning.add(spMidasBinning.getContainer());
    // pnlMidasBinning.add(Box.createHorizontalStrut(225));
    pnlMidasBinning.add(Box.createHorizontalGlue());
    // FixEdges
    pnlFixEdges.setLayout(new BoxLayout(pnlFixEdges, BoxLayout.X_AXIS));
    pnlFixEdges.add(Box.createHorizontalGlue());
    pnlFixEdges.add(btnFixEdges.getComponent());
    pnlFixEdges.add(Box.createHorizontalGlue());
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
    pnlOpenAlignedStack.add(btn3dmodPrealign.getComponent());
    pnlOpenAlignedStack.add(Box.createHorizontalGlue());
    // make stack
    index = Tab.MAKE_ALIGNED_STACK.index;
    pnlTabBodyArray[index].setLayout(new BoxLayout(pnlTabBodyArray[index],
        BoxLayout.Y_AXIS));
    pnlTabBodyArray[index].add(pnlXftoxgAlignmentX);
    pnlTabBodyArray[index].add(Box.createRigidArea(FixedDim.x0_y3));
    pnlTabBodyArray[index].add(cbsReferenceSection.getContainer());
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
    pnlXftoxgAlignment.setBorder(new EtchedBorder("Stack alignment transforms")
        .getBorder());
    pnlXftoxgAlignment.add(rbNoOptions.getComponent());
    pnlXftoxgAlignment.add(rbHybridFitsTranslations.getComponent());
    pnlXftoxgAlignment.add(rbHybridFitsTranslationsRotations.getComponent());
    pnlXftoxgAlignment.add(rbNumberToFitGlobalAlignment.getComponent());
    UIHarness.INSTANCE.pack(axisID, manager);
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
    pnlMakeStackA.add(spBinByFactor.getContainer());
    pnlMakeStackA.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlMakeStackA.add(cbFillWithZero);
    // make stack buttons
    pnlMakeStackButtons.setLayout(new BoxLayout(pnlMakeStackButtons, BoxLayout.X_AXIS));
    pnlMakeStackButtons.add(btnAlign.getComponent());
    pnlMakeStackButtons.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlMakeStackButtons.add(btn3dmodAlign.getComponent());
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
    btn3dmodPrealign.addActionListener(listener);
    btnAlign.addActionListener(listener);
    btn3dmodAlign.addActionListener(listener);
    btn3dmodRawStack.addActionListener(listener);
  }

  public void enableMidas() {
    autoAlignmentPanel.enableMidas();
  }

  public void getParameters(final SerialSectionsMetaData metaData) {
    boolean doValidation = false;
    try {
      metaData.setRobustFitCriterion(ctfRobustFitCriterion.getText(doValidation));
      metaData.setMidasBinning(spMidasBinning.getValue());
      autoAlignmentPanel.getParameters(metaData.getAutoAlignmentMetaData());
      metaData.setNoOptions(rbNoOptions.isSelected());
      metaData.setHybridFitsTranslations(rbHybridFitsTranslations.isSelected());
      metaData.setHybridFitsTranslationsRotations(rbHybridFitsTranslationsRotations
          .isSelected());
      metaData.setNumberToFitGlobalAlignment(rbNumberToFitGlobalAlignment.isSelected());
      metaData.setUseReferenceSection(cbsReferenceSection.isSelected());
      metaData.setReferenceSection(cbsReferenceSection.getValue());
      metaData.setSizeX(ltfSizeX.getText());
      metaData.setSizeY(ltfSizeY.getText());
      metaData.setShiftX(ltfShiftX.getText());
      metaData.setShiftY(ltfShiftY.getText());
      metaData.setTab(curTab.index);
    }
    catch (FieldValidationFailedException e) {
      // Validation is not done for meta data
      e.printStackTrace();
    }
  }

  public void setParameters(final ConstSerialSectionsMetaData metaData) {
    ctfRobustFitCriterion.setText(metaData.getRobustFitCriterion());
    spMidasBinning.setValue(metaData.getMidasBinning());
    autoAlignmentPanel.setParameters(metaData.getAutoAlignmentMetaData());
    rbNoOptions.setSelected(metaData.isNoOptions());
    rbHybridFitsTranslations.setSelected(metaData.isHybridFitsTranslations());
    rbHybridFitsTranslationsRotations.setSelected(metaData
        .isHybridFitsTranslationsRotations());
    rbNumberToFitGlobalAlignment.setSelected(metaData.isNumberToFitGlobalAlignment());
    cbsReferenceSection.setSelected(metaData.isUseReferenceSection());
    cbsReferenceSection.setValue(metaData.getReferenceSection());
    ltfSizeX.setText(metaData.getSizeX());
    ltfSizeY.setText(metaData.getSizeY());
    ltfShiftX.setText(metaData.getShiftX());
    ltfShiftY.setText(metaData.getShiftY());
    int tab = Tab.getDefaultInstance(metaData.getViewType()).index;
    if (!metaData.isTabEmpty()) {
      tab = metaData.getTab();
    }
    changeTab(tab);
  }

  public void getAutoAlignmentParameters(final MidasParam param) {
    manager.getAutoAlignmentParameters(param, axisID);
    autoAlignmentPanel.getParameters(param);
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

  public boolean getPreblendParameters(final BlendmontParam param,
      final boolean doValidation) {
    try {
      param.setVerySloppyMontage(cbVerySloppyMontage.isSelected());
      if (ctfRobustFitCriterion.isSelected()) {
        param.setRobustFitCriterion(ctfRobustFitCriterion.getText(doValidation));
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  public void getBlendParameters(final BlendmontParam param) {
    if (ltfSizeX.isEmpty() && ltfSizeY.isEmpty() && ltfShiftX.isEmpty()
        && ltfShiftY.isEmpty()) {
      param.resetStartingAndEndingXandY();
    }
    else {
      param.setStartingAndEndingXAndY(TomogramTool.getStartingAndEndingXAndY(
          FileType.PREBLEND_OUTPUT_MRC, ltfSizeX.getText(), ltfShiftX.getText(),
          ltfSizeY.getText(), ltfShiftY.getText(), manager, axisID,
          ltfSizeX.getQuotedLabel(), ltfShiftX.getQuotedLabel(),
          Utilities.quoteLabel(SIZE_LABEL + ltfSizeY.getLabel()),
          Utilities.quoteLabel(SHIFT_LABEL + ltfShiftY.getLabel()), "Entry Error"));
    }
    param.setBinByFactor(spBinByFactor.getValue());
    if (cbFillWithZero.isSelected()) {
      param.setFillValue(0);
    }
  }

  public void setBlendParameters(final BlendmontParam param) {
    spBinByFactor.setValue(param.getBinByFactor());
    cbFillWithZero.setSelected(param.fillValueEquals(0));
  }

  public void getParameters(final NewstParam param) throws FortranInputSyntaxException,
      InvalidParameterException, IOException {
    param.setSizeToOutputInXandY(ltfSizeX.getText(), ltfSizeY.getText(), spBinByFactor
        .getValue().intValue(), 0, "Size");
    param.setOffsetsInXandY(TomogramTool.convertShiftsToOffsets(ltfShiftX.getText(),
        ltfShiftY.getText()));
    param.setBinByFactor(spBinByFactor.getValue());
    if (cbFillWithZero.isSelected()) {
      param.setFillValue(0);
    }
  }

  public void setParameters(final ConstNewstParam param) {
    ltfShiftX.setText(TomogramTool.convertOffsetToShift(param.getOffsetInX()));
    ltfShiftY.setText(TomogramTool.convertOffsetToShift(param.getOffsetInY()));
    spBinByFactor.setValue(param.getBinByFactor());
    cbFillWithZero.setSelected(param.fillValueEquals(0));
  }

  public void getAutoAlignmentParameters(final XfalignParam param) {
    manager.getAutoAlignmentParameters(param, axisID);
    autoAlignmentPanel.getParameters(param);
  }

  public void getParameters(final XftoxgParam param) {
    if (rbNoOptions.isSelected()) {
      param.resetHybridFits();
      param.resetNumberToFit();
    }
    else if (rbHybridFitsTranslations.isSelected()
        || rbHybridFitsTranslationsRotations.isSelected()) {
      param.setHybridFits(((RadioButton.RadioButtonModel) bgXftoxgAlignment
          .getSelection()).getEnumeratedType());
      param.resetNumberToFit();
    }
    else if (rbNumberToFitGlobalAlignment.isSelected()) {
      param.resetHybridFits();
      param.setNumberToFit(((RadioButton.RadioButtonModel) bgXftoxgAlignment
          .getSelection()).getEnumeratedType());
    }
    if (cbsReferenceSection.isSelected()) {
      param.setReferenceSection(cbsReferenceSection.getValue());
    }
    else {
      param.resetReferenceSection();
    }
  }

  public void setParameters(final XftoxgParam param) {
    boolean hybridFitsEmpty = param.isHybridFitsEmpty();
    boolean numberToFitEmpty = param.isNumberToFitEmpty();
    if (hybridFitsEmpty && numberToFitEmpty) {
      rbNoOptions.setSelected(true);
    }
    else if (!hybridFitsEmpty) {
      int hybridFits = param.getHybridFits();
      if (XftoxgParam.HybridFits.TRANSLATIONS.equals(hybridFits)) {
        rbHybridFitsTranslations.setSelected(true);
      }
      else if (XftoxgParam.HybridFits.TRANSLATIONS_ROTATIONS.equals(hybridFits)) {
        rbHybridFitsTranslationsRotations.setSelected(true);
      }
    }
    else if (!numberToFitEmpty
        && XftoxgParam.NumberToFit.GLOBAL_ALIGNMENT.equals(param.getNumberToFit())) {
      rbNumberToFitGlobalAlignment.setSelected(true);
    }
    cbsReferenceSection.setSelected(!param.isReferenceSectionEmpty());
    if (cbsReferenceSection.isSelected()) {
      cbsReferenceSection.setValue(param.getReferenceSection());
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
      manager.preblend(null, deferred3dmodButton, axisID, run3dmodMenuOptions,
          DialogType.SERIAL_SECTIONS);
    }
    else if (command.equals(btnFixEdges.getActionCommand())) {
      manager.midasFixEdges(axisID, null);
    }
    else if (command.equals(btnAlign.getActionCommand())) {
      manager.align(axisID, deferred3dmodButton, run3dmodMenuOptions);
    }
    else if (command.equals(btn3dmodRawStack.getActionCommand())) {
      manager.imodRaw(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(btn3dmodPreblend.getActionCommand())) {
      manager.imodPreblend(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(btn3dmodPrealign.getActionCommand())) {
      manager.imodPrealign(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(btn3dmodAlign.getActionCommand())) {
      manager.imodAlign(axisID, run3dmodMenuOptions);
    }
  }

  private void changeTab(final int newTabIndex) {
    tabPane.setSelectedIndex(newTabIndex);
    changeTab();
  }

  private void changeTab() {
    Tab newTab = Tab.getInstance(tabPane.getSelectedIndex());
    if (newTab == curTab) {
      return;
    }
    if (curTab != null) {
      tabChangeWarning();
      pnlTabArray[curTab.index].remove(pnlTabBodyArray[curTab.index]);
    }
    curTab = newTab;
    pnlTabArray[curTab.index].add(pnlTabBodyArray[curTab.index]);
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  private void tabChangeWarning() {
    if (curTab == Tab.INITIAL_BLEND) {
      TransformsTool.checkUpToDateEdgeFunctionsFile(manager.getState()
          .getInvalidEdgeFunctions().is(), manager, axisID,
          Utilities.quoteLabel(Tab.INITIAL_BLEND.title), btnPreblend.getQuotedLabel());
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(final MouseEvent mouseEvent) {
    ViewType viewType = metaData.getViewType();
    String[] manPageLabel;
    String[] manPage;
    String[] logFileLabel;
    String[] logFile;
    if (viewType == ViewType.MONTAGE) {
      manPageLabel = new String[] { "Blendmont", "Midas", "Xfalign", "Xftoxg", "3dmod" };
      manPage = new String[] { "blendmont.html", "midas.html", "xfalign.html",
          "xftoxg.html", "3dmod.html" };
      logFileLabel = new String[] { "Blend", "Preblend", "Xfalign" };
      logFile = new String[] { "blend.log", "preblend.log", "xfalign.log" };
    }
    else {
      manPageLabel = new String[] { "Colornewst", "Midas", "Newstack", "Xfalign",
          "Xftoxg", "3dmod" };
      manPage = new String[] { "colornewst.html", "midas.html", "newstack.html",
          "xfalign.html", "xftoxg.html", "3dmod.html" };
      logFileLabel = new String[] { "Newst", "Xfalign" };
      logFile = new String[] { "newst.log", "xfalign.log" };
    }
    ContextPopup contextPopup = new ContextPopup(pnlRoot, mouseEvent, "Serial Sections",
        ContextPopup.TOMO_GUIDE, manPageLabel, manPage, logFileLabel, logFile, manager,
        axisID);
  }

  private void setTooltips() {
    ViewType viewType = metaData.getViewType();
    String text = "Blends the overlapping edges of the montage images without "
        + "transformations.  Runs blendmont.";
    tabPane.setToolTipTextAt(Tab.INITIAL_BLEND.index,
        TooltipFormatter.INSTANCE.format(text));
    btnPreblend.setToolTipText(text);
    text = "Opens the blended stack.";
    btn3dmodPreblend.setToolTipText(text);
    String text2 = "Opens the raw stack.";
    btn3dmodRawStack.setToolTipText(text2);
    btn3dmodPrealign.setToolTipText(viewType == ViewType.MONTAGE ? text : text2);
    spMidasBinning.setToolTipText(SharedConstants.MIDAS_BINNING_TOOLTIP);
    btnFixEdges.setToolTipText("Optionally use Midas to correct the blending of images "
        + "in the montaged stack.  Runs midas.");
    tabPane.setToolTipTextAt(Tab.ALIGN.index,
        TooltipFormatter.INSTANCE.format("Creates transformations to aligned sections."));
    text = "Applies transformations to the serial sections stack.";
    tabPane.setToolTipTextAt(Tab.MAKE_ALIGNED_STACK.index,
        TooltipFormatter.INSTANCE.format(text));
    btnAlign.setToolTipText(text + "  Runs xftoxg and "
        + (viewType == ViewType.MONTAGE ? "blendmont" : "newst") + ".");
    rbNoOptions.setToolTipText("The " + XftoxgParam.HYBRID_FITS_KEY + " and the "
        + XftoxgParam.NUMBER_TO_FIT_KEY + " options are not used.");
    btn3dmodAlign.setToolTipText("Opens the aligned serial sections stack.");
    ReadOnlyAutodoc autodoc = null;
    try {
      if (viewType == ViewType.MONTAGE) {
        autodoc = AutodocFactory.getInstance(manager, AutodocFactory.BLENDMONT, axisID);
        cbVerySloppyMontage.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
            BlendmontParam.VERY_SLOPPY_MONTAGE_KEY));
        ctfRobustFitCriterion.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
            BlendmontParam.ROBUST_FIT_CRITERION_KEY));
        text = "Converted to " + BlendmontParam.STARTING_AND_ENDING_X_KEY + ":  "
            + EtomoAutodoc.getTooltip(autodoc, BlendmontParam.STARTING_AND_ENDING_X_KEY);
        ltfSizeX.setToolTipText(text);
        ltfShiftX.setToolTipText(text);
        text = "Converted to " + BlendmontParam.STARTING_AND_ENDING_Y_KEY + ":  "
            + EtomoAutodoc.getTooltip(autodoc, BlendmontParam.STARTING_AND_ENDING_Y_KEY);
        ltfSizeY.setToolTipText(text);
        ltfShiftY.setToolTipText(text);
        spBinByFactor.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
            BlendmontParam.BIN_BY_FACTOR_KEY));
        cbFillWithZero.setToolTipText("Sets " + BlendmontParam.FILL_VALUE_KEY
            + " to zero.  "
            + EtomoAutodoc.getTooltip(autodoc, BlendmontParam.FILL_VALUE_KEY));
      }
      else {
        autodoc = AutodocFactory.getInstance(manager, AutodocFactory.NEWSTACK, axisID);
        text = EtomoAutodoc.getTooltip(autodoc, NewstParam.SIZE_TO_OUTPUT_IN_X_AND_Y);
        ltfSizeX.setToolTipText(text);
        ltfSizeY.setToolTipText(text);
        text = "Converted to " + NewstParam.OFFSETS_IN_X_AND_Y_KEY + ":  "
            + EtomoAutodoc.getTooltip(autodoc, NewstParam.OFFSETS_IN_X_AND_Y_KEY);
        ltfShiftX.setToolTipText(text);
        ltfShiftY.setToolTipText(text);
        spBinByFactor.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
            NewstParam.BIN_BY_FACTOR_KEY));
        cbFillWithZero.setToolTipText("Sets " + NewstParam.FILL_VALUE_KEY + " to zero.  "
            + EtomoAutodoc.getTooltip(autodoc, NewstParam.FILL_VALUE_KEY));
      }
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException except) {
      except.printStackTrace();
    }
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.XFTOXG, axisID);
      text = EtomoAutodoc.getTooltip(autodoc, XftoxgParam.HYBRID_FITS_KEY);
      rbHybridFitsTranslations.setToolTipText(text);
      rbHybridFitsTranslationsRotations.setToolTipText(text);
      rbNumberToFitGlobalAlignment.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          XftoxgParam.NUMBER_TO_FIT_KEY));
      cbsReferenceSection.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          XftoxgParam.REFERENCE_SECTION));
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException except) {
      except.printStackTrace();
    }
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
    private static final Tab MAKE_ALIGNED_STACK = new Tab(2, "Make Stack");

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
      if (index == MAKE_ALIGNED_STACK.index) {
        return MAKE_ALIGNED_STACK;
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

    public boolean equals(final int index) {
      return this.index == index;
    }
  }
}
