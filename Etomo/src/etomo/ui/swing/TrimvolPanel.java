package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.TrimvolParam;
import etomo.logic.TrimvolInputFileState;
import etomo.process.ImodManager;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.InvalidEtomoNumberException;
import etomo.type.MetaData;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;

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
 * <p> Revision 1.3  2011/06/21 18:40:03  sueh
 * <p> Bug# 1490 Replaced getTrimvolParams with getParameters.  Placed warnings setup in
 * <p> setStartupWarnings.  Added setParameters(ConstMetaData, dialogExists) since there is no
 * <p> TrimvolParam in metaData anymore.
 * <p>
 * <p> Revision 1.2  2011/02/22 21:42:11  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.40  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.39  2009/12/19 01:20:30  sueh
 * <p> bug# 1294 Factored out the VolumeRangePanel.
 * <p>
 * <p> Revision 3.38  2009/09/17 19:12:06  sueh
 * <p> Removed unnecessary print.
 * <p>
 * <p> Revision 3.37  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.36  2009/06/22 15:34:00  sueh
 * <p> bug# 1224 Added log messages when action listener is added to or
 * <p> removed from the trimvol button..
 * <p>
 * <p> Revision 3.35  2009/06/05 02:19:27  sueh
 * <p> bug# Improved formatting.
 * <p>
 * <p> Revision 3.34  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.33  2009/02/10 22:21:03  sueh
 * <p> bug# 1143 Added warning label.  In setParameters(TrimvolParam) set
 * <p> warning label text and show or hide it.
 * <p>
 * <p> Revision 3.32  2009/01/20 20:33:03  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 3.31  2008/09/30 22:51:46  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 3.30  2008/07/15 21:23:58  sueh
 * <p> bug# 1127 Placing the rubberband button for scaling in the trimvol panel
 * <p> instead of the rubberband panel because it includes Z, which is in a radio
 * <p> button in the trimvol panel.
 * <p>
 * <p> Revision 3.29  2008/05/28 02:52:12  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 3.28  2008/05/13 23:09:14  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 3.27  2008/05/03 00:57:57  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 3.26  2008/02/28 21:19:46  sueh
 * <p> bug# 1085 Added setting Z to setXYMinAndMax.  Implemented
 * <p> RubberbandContainer.
 * <p>
 * <p> Revision 3.25  2007/11/06 20:33:08  sueh
 * <p> bug# 1047 Generalize TripvolPanel.
 * <p>
 * <p> Revision 3.24  2007/08/08 15:08:27  sueh
 * <p> bug# 834 Sharing fields labels.
 * <p>
 * <p> Revision 3.23  2007/03/07 21:16:57  sueh
 * <p> bug# 981 Turned RadioButton into a wrapper rather then a child of JRadioButton,
 * <p> because it is getting more complicated.
 * <p>
 * <p> Revision 3.22  2007/02/09 00:55:11  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.21  2006/10/20 21:48:13  sueh
 * <p> bug# 946  Adding a warning to the reorientation box.
 * <p>
 * <p> Revision 3.20  2006/08/16 22:42:26  sueh
 * <p> bug# 912 Rubberband panel border label is no longer optional.
 * <p>
 * <p> Revision 3.19  2006/08/16 18:52:26  sueh
 * <p> bug# 912 Making the rubberband panel generic.
 * <p>
 * <p> Revision 3.18  2006/08/14 18:34:35  sueh
 * <p> bug#  890 Validating section scale min and max, and fixed scale min and max.
 * <p>
 * <p> Revision 3.17  2006/07/21 19:19:32  sueh
 * <p> bug# 848 Moved dimensions that have to be adjusted for font size from
 * <p> FixedDim to UIParameters.
 * <p>
 * <p> Revision 3.16  2006/06/28 23:29:59  sueh
 * <p> bug# 881 Added pnlScaleRubberband.
 * <p>
 * <p> Revision 3.15  2006/06/27 23:47:59  sueh
 * <p> bug# 879 Placed swapYZ and rotateX into a labeled panel.
 * <p>
 * <p> Revision 3.14  2006/06/27 17:55:37  sueh
 * <p> bug# 897 Simplifying rotate x label.
 * <p>
 * <p> Revision 3.13  2006/06/27 17:49:38  sueh
 * <p> bug# 879 Make the swap yz check box a radio button.  Added a rotate in x radio
 * <p> button and a do not change radio button.
 * <p>
 * <p> Revision 3.12  2006/01/31 21:01:36  sueh
 * <p> bug# 521 Managing the trimvol button in ProcessResultDisplayFactory.
 * <p> Made trimvol a toggle button.
 * <p>
 * <p> Revision 3.11  2006/01/04 00:05:47  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox.  Converted
 * <p> JRadioButton's to RadioButton.
 * <p>
 * <p> Revision 3.10  2005/11/14 22:35:38  sueh
 * <p> bug# 762 Made scaleAction().
 * <p>
 * <p> Revision 3.9  2005/08/12 00:01:38  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Prevent context menu from popping up when button is
 * <p> disabled.  Get rid of duplicate code by running the 3dmods from a private
 * <p> function called run3dmod(String, Run3dmodMenuOptions).  It can be
 * <p> called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and the
 * <p> action function.
 * <p>
 * <p> Revision 3.8  2005/08/10 20:48:56  sueh
 * <p> bug# 711 Moved button sizing to MultiLineButton.  SetSize() sets the
 * <p> standard button size.
 * <p>
 * <p> Revision 3.7  2005/08/09 21:11:46  sueh
 * <p> bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * <p> Changed 3dmod buttons to Run3dmodButton.  No longer inheriting
 * <p> MultiLineButton from JButton.
 * <p>
 * <p> Revision 3.6  2005/04/25 21:41:51  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.5  2005/03/24 17:55:44  sueh
 * <p> bug# 621 Set a preferred text width for fields that where too small.
 * <p>
 * <p> Revision 3.4  2004/05/13 20:13:51  sueh
 * <p> bug# 33 change setXYMinAndMax() so it can ignore non rubberband data
 * <p>
 * <p> Revision 3.3  2004/05/07 19:53:23  sueh
 * <p> bug# 33 getting coordinates info in the right order, getting only
 * <p> the correct kind of data
 * <p>
 * <p> Revision 3.2  2004/05/06 20:25:16  sueh
 * <p> bug# 33 added getCoordinates button, moved fullvol button to the top
 * <p> of the dialog, added setXYMinAndMax() to set field values
 * <p>
 * <p> Revision 3.1  2004/01/30 22:45:34  sueh
 * <p> bug# 356 Changing buttons with html labels to
 * <p> MultiLineButton and MultiLineToggleButton
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.12  2003/10/29 20:49:11  rickg
 * <p> Bug# 308 Tooltips
 * <p>
 * <p> Revision 1.11  2003/10/20 23:25:41  rickg
 * <p> Bug# 253 Added convert to bytes checkbox
 * <p>
 * <p> Revision 1.10  2003/10/16 17:05:10  rickg
 * <p> Bug# 305 Label changes, backup file filter
 * <p>
 * <p> Revision 1.9  2003/09/08 05:48:16  rickg
 * <p> Method name change for opening the complete volume
 * <p>
 * <p> Revision 1.8  2003/04/28 23:25:25  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 1.7  2003/04/17 23:08:38  rickg
 * <p> Initial revision
 * <p>
 * <p> Revision 1.6  2003/04/16 22:18:17  rickg
 * <p> Trimvol in progress
 * <p>
 * <p> Revision 1.5  2003/04/16 00:14:40  rickg
 * <p> Trimvol in progress
 * <p>
 * <p> Revision 1.4  2003/04/14 23:57:44  rickg
 * <p> In progress
 * <p>
 * <p> Revision 1.3  2003/04/14 04:31:31  rickg
 * <p> In progres
 * <p>
 * <p> Revision 1.2  2003/04/10 23:42:51  rickg
 * <p> In progress
 * <p>
 * <p> Revision 1.1  2003/04/09 23:37:20  rickg
 * <p> In progress
 * <p> </p>
 */

public final class TrimvolPanel implements Run3dmodButtonContainer, RubberbandContainer {
  public static final String rcsid = "$Id$";

  private static final String SCALING_ERROR_TITLE = "Scaling Panel Error";
  private static final String FIXED_SCALE_MIN_LABEL = "black: ";
  private static final String FIXED_SCALE_MAX_LABEL = " white: ";
  private static final String SECTION_SCALE_MIN_LABEL = "Z min: ";
  private static final String SECTION_SCALE_MAX_LABEL = " Z max: ";
  static final String SWAP_YZ_LABEL = "Swap Y and Z dimensions";
  static final String REORIENTATION_GROUP_LABEL = "Reorientation:";

  private ApplicationManager applicationManager;

  private EtomoPanel pnlTrimvol = new EtomoPanel();

  private SpacedPanel pnlScale = SpacedPanel.getInstance();
  private JPanel pnlScaleFixed = new JPanel();
  private CheckBox cbConvertToBytes = new CheckBox("Convert to bytes");
  private RadioButton rbScaleFixed = new RadioButton("Scale to match contrast  ");
  private LabeledTextField ltfFixedScaleMin = new LabeledTextField(FieldType.INTEGER,
      FIXED_SCALE_MIN_LABEL);
  private LabeledTextField ltfFixedScaleMax = new LabeledTextField(FieldType.INTEGER,
      FIXED_SCALE_MAX_LABEL);

  private RadioButton rbScaleSection = new RadioButton("Find scaling from sections  ");
  private JPanel pnlScaleSection = new JPanel();
  private LabeledTextField ltfSectionScaleMin = new LabeledTextField(FieldType.INTEGER,
      SECTION_SCALE_MIN_LABEL);
  private LabeledTextField ltfSectionScaleMax = new LabeledTextField(FieldType.INTEGER,
      SECTION_SCALE_MAX_LABEL);

  private final EtomoPanel pnlReorientationChoices = new EtomoPanel();
  private final ButtonGroup bgReorientation = new ButtonGroup();
  private final RadioButton rbNone = new RadioButton("None");
  private final RadioButton rbSwapYZ = new RadioButton(SWAP_YZ_LABEL);
  private final RadioButton rbRotateX = new RadioButton("Rotate around X axis");
  private final JLabel lWarning1 = new JLabel("Warning:");
  private final JLabel lWarning2 = new JLabel("For serial joins, use");
  private final JLabel lWarning3 = new JLabel("the same reorientation");
  private final JLabel lWarning4 = new JLabel("method for each");
  private final JLabel lWarning5 = new JLabel("section.");
  private final JLabel warning = new JLabel();

  private JPanel pnlButton = new JPanel();
  private Run3dmodButton btnImodFull = Run3dmodButton.get3dmodInstance(
      "3dmod Full Volume", this);
  private final Run3dmodButton btnTrimvol;
  private Run3dmodButton btnImodTrim = Run3dmodButton.get3dmodInstance(
      "3dmod Trimmed Volume", this);
  private MultiLineButton btnGetCoordinates = new MultiLineButton(
      "Get XYZ Volume Range From 3dmod");
  private JPanel pnlImodFull = new JPanel();
  private final VolumeRangePanel volumeRangePanel;

  private final ButtonListener buttonActonListener;
  private final RubberbandPanel pnlScaleRubberband;
  private final AxisID axisID;
  private final DialogType dialogType;
  private final boolean lockPanel;

  /**
   * Default constructor
   */
  public TrimvolPanel(final ApplicationManager appMgr, final AxisID axisID,
      final DialogType dialogType, final boolean lockPanel) {
    this.dialogType = dialogType;
    this.axisID = axisID;
    this.lockPanel = lockPanel;
    applicationManager = appMgr;
    volumeRangePanel = VolumeRangePanel.getInstance(lockPanel);
    // panels
    pnlScaleRubberband = RubberbandPanel.getNoButtonInstance(appMgr, this,
        ImodManager.COMBINED_TOMOGRAM_KEY, "Scaling from sub-area:",
        "Get XYZ Sub-Area From 3dmod",
        "Minimum X coordinate on the left side to analyze for contrast range.",
        "Maximum X coordinate on the right side to analyze for contrast range.",
        "The lower Y coordinate to analyze for contrast range.",
        "The upper Y coordinate to analyze for contrast range.", lockPanel);
    btnTrimvol = (Run3dmodButton) appMgr.getProcessResultDisplayFactory(AxisID.ONLY)
        .getTrimVolume();
    btnTrimvol.setContainer(this);
    btnTrimvol.setDeferred3dmodButton(btnImodTrim);

    // init
    btnTrimvol.setEnabled(!lockPanel);

    // Set the button sizes
    btnImodFull.setSize();
    btnTrimvol.setSize();
    btnImodTrim.setSize();
    btnGetCoordinates.setSize();

    // Layout the scale panel
    pnlScaleFixed.setLayout(new BoxLayout(pnlScaleFixed, BoxLayout.X_AXIS));

    pnlScaleFixed.add(rbScaleFixed.getComponent());
    pnlScaleFixed.add(ltfFixedScaleMin.getContainer());
    pnlScaleFixed.add(ltfFixedScaleMax.getContainer());

    pnlScaleSection.setLayout(new BoxLayout(pnlScaleSection, BoxLayout.X_AXIS));
    pnlScaleSection.add(rbScaleSection.getComponent());
    ltfSectionScaleMin.setTextPreferredWidth(UIParameters.INSTANCE.getFourDigitWidth());
    ltfSectionScaleMax.setTextPreferredWidth(UIParameters.INSTANCE.getFourDigitWidth());
    pnlScaleSection.add(ltfSectionScaleMin.getContainer());
    pnlScaleSection.add(ltfSectionScaleMax.getContainer());

    ButtonGroup bgScale = new ButtonGroup();
    bgScale.add(rbScaleFixed.getAbstractButton());
    bgScale.add(rbScaleSection.getAbstractButton());

    pnlScale.setBoxLayout(BoxLayout.Y_AXIS);
    pnlScale.setBorder(new EtchedBorder("Scaling").getBorder());

    cbConvertToBytes.setAlignmentX(Component.RIGHT_ALIGNMENT);
    JPanel pnlConvertToBytes = new JPanel();
    pnlConvertToBytes.setLayout(new BoxLayout(pnlConvertToBytes, BoxLayout.X_AXIS));
    pnlConvertToBytes.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlConvertToBytes.add(cbConvertToBytes);
    pnlConvertToBytes.add(Box.createHorizontalGlue());
    pnlScale.add(pnlConvertToBytes);
    pnlScale.add(pnlScaleFixed);
    pnlScale.add(pnlScaleSection);
    pnlScale.add(pnlScaleRubberband.getComponent());
    pnlScale.add(pnlScaleRubberband.getRubberbandButtonComponent());

    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnTrimvol.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodTrim.getComponent());
    pnlButton.add(Box.createHorizontalGlue());

    pnlTrimvol.setLayout(new BoxLayout(pnlTrimvol, BoxLayout.Y_AXIS));
    pnlTrimvol.setBorder(new BeveledBorder("Volume Trimming").getBorder());
    warning.setForeground(ProcessControlPanel.colorNotStarted);
    warning.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlTrimvol.add(warning);
    pnlImodFull.setLayout(new BoxLayout(pnlImodFull, BoxLayout.X_AXIS));
    pnlImodFull.add(Box.createHorizontalGlue());
    pnlImodFull.add(btnImodFull.getComponent());
    pnlImodFull.add(Box.createHorizontalGlue());
    pnlImodFull.add(btnGetCoordinates.getComponent());
    pnlImodFull.add(Box.createHorizontalGlue());
    pnlTrimvol.add(pnlImodFull);
    pnlTrimvol.add(volumeRangePanel.getComponent());
    pnlTrimvol.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlTrimvol.add(pnlScale.getContainer());
    pnlTrimvol.add(Box.createRigidArea(FixedDim.x0_y10));
    SpacedPanel pnlReorientation = SpacedPanel.getInstance();
    pnlReorientation.setBoxLayout(BoxLayout.X_AXIS);
    pnlReorientationChoices.setLayout(new BoxLayout(pnlReorientationChoices,
        BoxLayout.Y_AXIS));
    pnlReorientationChoices.setBorder(new EtchedBorder(REORIENTATION_GROUP_LABEL)
        .getBorder());
    pnlReorientationChoices.setAlignmentX(Component.RIGHT_ALIGNMENT);
    bgReorientation.add(rbNone.getAbstractButton());
    bgReorientation.add(rbSwapYZ.getAbstractButton());
    bgReorientation.add(rbRotateX.getAbstractButton());
    rbNone.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbSwapYZ.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbRotateX.setAlignmentX(Component.LEFT_ALIGNMENT);
    pnlReorientationChoices.add(rbNone.getComponent());
    pnlReorientationChoices.add(rbSwapYZ.getComponent());
    pnlReorientationChoices.add(rbRotateX.getComponent());
    pnlReorientation.add(pnlReorientationChoices);
    // reorientation warning panel
    JPanel pnlReorientationWarning = new JPanel();
    pnlReorientationWarning.setLayout(new BoxLayout(pnlReorientationWarning,
        BoxLayout.Y_AXIS));
    pnlReorientationWarning.add(lWarning1);
    pnlReorientationWarning.add(lWarning2);
    pnlReorientationWarning.add(lWarning3);
    pnlReorientationWarning.add(lWarning4);
    pnlReorientationWarning.add(lWarning5);
    pnlReorientation.add(pnlReorientationWarning);
    // trimvol panel
    pnlTrimvol.add(pnlReorientation.getContainer());
    pnlTrimvol.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlTrimvol.add(pnlButton);
    pnlTrimvol.add(Box.createRigidArea(FixedDim.x0_y10));

    setToolTipText();

    ScalingListener ScalingListener = new ScalingListener(this);
    rbScaleFixed.addActionListener(ScalingListener);
    rbScaleSection.addActionListener(ScalingListener);
    cbConvertToBytes.addActionListener(ScalingListener);

    buttonActonListener = new ButtonListener(this);
    btnImodFull.addActionListener(buttonActonListener);
    btnTrimvol.addActionListener(buttonActonListener);
    btnImodTrim.addActionListener(buttonActonListener);
    btnGetCoordinates.addActionListener(buttonActonListener);
  }

  /**
   * Return the container of the panel
   * @return
   */
  public Container getContainer() {
    return pnlTrimvol;
  }

  void setParameters(final TrimvolParam param) {
    if (lockPanel) {
      return;
    }
    // volumeRangePanel.setParameters(param);
    if (param.isSwapYZ()) {
      rbSwapYZ.setSelected(true);
    }
    else if (param.isRotateX()) {
      rbRotateX.setSelected(true);
    }
    else {
      rbNone.setSelected(true);
    }

    cbConvertToBytes.setSelected(param.isConvertToBytes());
    if (param.isFixedScaling()) {
      rbScaleFixed.setSelected(true);
    }
    else {
      ltfSectionScaleMin.setText(param.getSectionScaleMin());
      ltfSectionScaleMax.setText(param.getSectionScaleMax());
      rbScaleSection.setSelected(true);
    }
    volumeRangePanel.setParameters(param);
    pnlScaleRubberband.setScaleParameters(param);
    setScaleState();
  }

  /**
   * Set the panel values with the specified parameters
   * @param trimvolParam
   */
  void setParameters(final ConstMetaData metaData, final boolean dialogExists) {
    if (!dialogExists || lockPanel) {
      // TrimvolParam can calculate the initial values, while metaData would have nothing
      // from this panel if the dialog hadn't been created yet.
      return;
    }
    volumeRangePanel.setParameters(metaData);
    if (metaData.isPostTrimvolSwapYZ()) {
      rbSwapYZ.setSelected(true);
    }
    else if (metaData.isPostTrimvolRotateX()) {
      rbRotateX.setSelected(true);
    }
    else {
      rbNone.setSelected(true);
    }

    cbConvertToBytes.setSelected(metaData.isPostTrimvolConvertToBytes());
    if (metaData.isPostTrimvolFixedScaling()) {
      ltfFixedScaleMin.setText(metaData.getPostTrimvolFixedScaleMin());
      ltfFixedScaleMax.setText(metaData.getPostTrimvolFixedScaleMax());
      rbScaleFixed.setSelected(true);
    }
    else {
      ltfSectionScaleMin.setText(metaData.getPostTrimvolSectionScaleMin());
      ltfSectionScaleMax.setText(metaData.getPostTrimvolSectionScaleMax());
      rbScaleSection.setSelected(true);
    }
    setScaleState();
    pnlScaleRubberband.setParameters(metaData);
  }

  void setStartupWarnings(final TrimvolInputFileState inputFileState) {
    // set warning
    if (inputFileState.isNColumnsChanged() || inputFileState.isNRowsChanged()) {
      if (inputFileState.isNSectionsChanged()) {
        warning.setText("Min and max values have been restored to defaults");
      }
      else {
        warning.setText("X,Y values have been restored to defaults");
      }
    }
    else {
      if (inputFileState.isNSectionsChanged()) {
        warning.setText("Z values have been restored to defaults");
      }
      else {
        warning.setVisible(false);
      }
    }
  }

  public void getParameters(final MetaData metaData) {
    if (lockPanel) {
      return;
    }
    volumeRangePanel.getParameters(metaData);
    metaData.setPostTrimvolSwapYZ(rbSwapYZ.isSelected());
    metaData.setPostTrimvolRotateX(rbRotateX.isSelected());
    metaData.setPostTrimvolConvertToBytes(cbConvertToBytes.isSelected());
    metaData.setPostTrimvolFixedScaling(rbScaleFixed.isSelected());
    metaData.setPostTrimvolFixedScaleMin(ltfFixedScaleMin.getText());
    metaData.setPostTrimvolFixedScaleMax(ltfFixedScaleMax.getText());
    metaData.setPostTrimvolSectionScaleMin(ltfSectionScaleMin.getText());
    metaData.setPostTrimvolSectionScaleMax(ltfSectionScaleMax.getText());
    // get the xyParam and set the values in it
    pnlScaleRubberband.getParameters(metaData);
  }

  void getParametersForTrimvol(final MetaData metaData) {
    if (lockPanel) {
      return;
    }
    volumeRangePanel.getParametersForTrimvol(metaData);
    metaData.setPostTrimvolScalingNewStyleZ(ltfSectionScaleMin.getText(),
        ltfSectionScaleMax.getText());
  }

  /**
   * Get the parameter values from the panel 
   * @param trimvolParam
   */
  public boolean getParameters(TrimvolParam trimvolParam, final boolean doValidation) {
    if (lockPanel) {
      return true;
    }
    try {
      if (!volumeRangePanel.getParameters(trimvolParam, doValidation)) {
        return false;
      }
      // Assume volume is flipped and set flipped - this means that Y and Z don't have to
      // be
      // swapped when setting them.
      trimvolParam.setFlippedVolume(true);
      trimvolParam.setSwapYZ(rbSwapYZ.isSelected());
      trimvolParam.setRotateX(rbRotateX.isSelected());

      trimvolParam.setConvertToBytes(cbConvertToBytes.isSelected());
      String errorMessage;
      if (rbScaleFixed.isSelected()) {
        trimvolParam.setFixedScaling(true);

        try {
          errorMessage = trimvolParam.setFixedScaleMin(
              ltfFixedScaleMin.getText(doValidation)).validate(FIXED_SCALE_MIN_LABEL);
          if (errorMessage != null) {
            UIHarness.INSTANCE.openMessageDialog(applicationManager, errorMessage,
                SCALING_ERROR_TITLE, axisID);
            throw new InvalidEtomoNumberException(errorMessage);
          }
          errorMessage = trimvolParam.setFixedScaleMax(
              ltfFixedScaleMax.getText(doValidation)).validate(FIXED_SCALE_MAX_LABEL);
          if (errorMessage != null) {
            UIHarness.INSTANCE.openMessageDialog(applicationManager, errorMessage,
                SCALING_ERROR_TITLE, axisID);
            throw new InvalidEtomoNumberException(errorMessage);
          }
        }
        catch (InvalidEtomoNumberException e) {
          return false;
        }
      }
      else {
        trimvolParam.setFixedScaling(false);
        try {
          errorMessage = trimvolParam.setSectionScaleMin(
              ltfSectionScaleMin.getText(doValidation)).validate(SECTION_SCALE_MIN_LABEL);
          if (errorMessage != null) {
            UIHarness.INSTANCE.openMessageDialog(applicationManager, errorMessage,
                SCALING_ERROR_TITLE, axisID);
            throw new InvalidEtomoNumberException(errorMessage);
          }
          errorMessage = trimvolParam.setSectionScaleMax(
              ltfSectionScaleMax.getText(doValidation)).validate(SECTION_SCALE_MAX_LABEL);
          if (errorMessage != null) {
            UIHarness.INSTANCE.openMessageDialog(applicationManager, errorMessage,
                SCALING_ERROR_TITLE, axisID);
            throw new InvalidEtomoNumberException(errorMessage);
          }
        }
        catch (InvalidEtomoNumberException e) {
          return false;
        }
      }
      // get the xyParam and set the values in it
      if (!pnlScaleRubberband.getScaleParameters(trimvolParam, doValidation)) {
        return false;
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  public void setParameters(ReconScreenState screenState) {
    if (lockPanel) {
      return;
    }
    btnTrimvol.setButtonState(screenState.getButtonState(btnTrimvol.getButtonStateKey()));
  }

  public void setZMin(String zMin) {
    if (rbScaleSection.isSelected()) {
      ltfSectionScaleMin.setText(zMin);
    }
  }

  public void setZMax(String zMax) {
    if (rbScaleSection.isSelected()) {
      ltfSectionScaleMax.setText(zMax);
    }
  }

  /**
   * Enable/disable the appropriate text fields for the scale section
   *
   */
  private void setScaleState() {
    rbScaleFixed.setEnabled(cbConvertToBytes.isSelected());
    rbScaleSection.setEnabled(cbConvertToBytes.isSelected());
    boolean fixedState = cbConvertToBytes.isSelected() && rbScaleFixed.isSelected();
    ltfFixedScaleMin.setEnabled(fixedState);
    ltfFixedScaleMax.setEnabled(fixedState);
    boolean scaleState = cbConvertToBytes.isSelected() && rbScaleSection.isSelected();
    ltfSectionScaleMin.setEnabled(scaleState);
    ltfSectionScaleMax.setEnabled(scaleState);
    pnlScaleRubberband.setEnabled(scaleState);
  }

  /**
   * Call setScaleState when the radio buttons change
   * @param event
   */
  protected void scaleAction(ActionEvent event) {
    setScaleState();
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    buttonAction(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void buttonAction(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command == btnTrimvol.getActionCommand()) {
      applicationManager.trimVolume(btnTrimvol, null, deferred3dmodButton,
          run3dmodMenuOptions, dialogType);
    }
    else if (command == btnGetCoordinates.getActionCommand()) {
      volumeRangePanel.setXYMinAndMax(applicationManager.imodGetRubberbandCoordinates(
          ImodManager.COMBINED_TOMOGRAM_KEY, AxisID.ONLY));
    }
    else if (command == btnImodFull.getActionCommand()) {
      applicationManager.imodCombinedTomogram(run3dmodMenuOptions);
    }
    else if (command == btnImodTrim.getActionCommand()) {
      applicationManager.imodTrimmedVolume(run3dmodMenuOptions, axisID);
    }
  }

  void done() {
    btnTrimvol.removeActionListener(buttonActonListener);
  }

  private void cbConvertToBytesAction(ActionEvent event) {
    boolean state = cbConvertToBytes.isSelected();
    rbScaleFixed.setEnabled(state);
    ltfFixedScaleMax.setEnabled(state);
    ltfFixedScaleMin.setEnabled(state);

    rbScaleSection.setEnabled(state);
    ltfSectionScaleMin.setEnabled(state);
    ltfSectionScaleMax.setEnabled(state);
  }

  /**
   * An inner class to manage the scale controls 
   */
  class ScalingListener implements ActionListener {
    TrimvolPanel listenee;

    ScalingListener(TrimvolPanel TrimvolPanel) {
      listenee = TrimvolPanel;
    }

    public void actionPerformed(ActionEvent event) {
      listenee.scaleAction(event);
    }
  }

  private final class ButtonListener implements ActionListener {
    private final TrimvolPanel listenee;

    private ButtonListener(final TrimvolPanel trimvolPanel) {
      listenee = trimvolPanel;
    }

    public void actionPerformed(final ActionEvent event) {
      listenee.buttonAction(event.getActionCommand(), null, null);
    }
  }

  /**
   * Initialize the tooltip text
   */
  private void setToolTipText() {
    cbConvertToBytes
        .setToolTipText("Scale densities to bytes with extreme densities truncated.");
    rbScaleFixed
        .setToolTipText("Set the scaling to match the contrast in a 3dmod display.");
    ltfFixedScaleMin
        .setToolTipText("Enter the black contrast slider setting (0-254) that gives the desired "
            + "contrast.");
    ltfFixedScaleMax
        .setToolTipText("Enter the white contrast slider setting (1-255) that gives the desired "
            + "contrast.");
    rbScaleSection
        .setToolTipText("Set the scaling based on the range of contrast in a subset of sections and XY volume.  "
            + "Exclude areas with extreme densities that can be truncated (gold "
            + "particles).");
    ltfSectionScaleMin
        .setToolTipText("Minimum Z section of the subset to analyze for contrast range.");
    ltfSectionScaleMax
        .setToolTipText("Maximum Z section of the subset to analyze for contrast range.");
    pnlReorientationChoices.setToolTipText("If the output volume is not reoriented, "
        + "the file will need to be flipped when loaded into 3dmod.");
    rbNone.setToolTipText("Do not change the orientation of the output volume.  "
        + "The file will need to be flipped when loaded into 3dmod.");
    rbSwapYZ
        .setToolTipText("Flip Y and Z in the output volume so that the file does not need to be "
            + "flipped when loaded into 3dmod.");
    rbRotateX
        .setToolTipText("Rotate the output volume by -90 degrees around the X axis, "
            + "by first creating a temporary trimmed volume with newstack then running \"clip rotx\" on this volume to create the final output file.  "
            + "The slices will look the same as with the -yz option but rotating instead of flipping will preserve the handedness of structures.");
    btnImodFull.setToolTipText("View the original, untrimmed volume in 3dmod.");
    btnGetCoordinates
        .setToolTipText("After pressing the 3dmod Full Volume button, press shift-B in the "
            + "ZaP window.  Create a rubberband around the volume range.  Then "
            + "press this button to retrieve X and Y coordinates.");
    btnTrimvol
        .setToolTipText("Trim the original volume with the parameters given above.");
    btnImodTrim.setToolTipText("View the trimmed volume.");
  }
}