package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltxcorrParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.ImodchopcontsParam;
import etomo.comscript.TiltxcorrParam;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.BaseScreenState;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.FileType;
import etomo.type.MetaData;
import etomo.type.PanelId;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TiltAngleSpec;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;

/**
 * <p>Description: Panel to modify the tiltxcorr parameters.</p>
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
 * <p> Revision 1.6  2011/07/19 20:01:14  sueh
 * <p> Bug# 1459 Wrapped checkboxes in a panel and used glue to left justify them.  Prevented spinners
 * <p> which have a value when they are first displayed from going all the way to the right.
 * <p>
 * <p> Revision 1.5  2011/03/02 00:00:12  sueh
 * <p> bug# 1452 Removing image rotation conversion between float and
 * <p> double.  Using string where possible.
 * <p>
 * <p> Revision 1.4  2011/02/24 23:38:47  sueh
 * <p> bug# 1452 in getParameters(TiltxcorrParam) setting RotationAngle.
 * <p>
 * <p> Revision 1.3  2011/02/22 21:40:17  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:21:50  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.  Handling a
 * <p> possibly null btnTiltxcorr.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.5  2010/10/11 20:43:37  sueh
 * <p> bug# 1379 Implemented ContextMenu.  Use own popup for patch
 * <p> tracking.  Otherwise use the parents.
 * <p>
 * <p> Revision 1.4  2010/03/08 21:15:43  sueh
 * <p> bug# 1311 Hooking the imod button to the run button.
 * <p>
 * <p> Revision 1.3  2010/03/05 04:04:56  sueh
 * <p> bug# 1311 Fixed typo.
 * <p>
 * <p> Revision 1.2  2010/03/04 02:52:29  sueh
 * <p> bug# 1311 Improved some labels.  Put some fields into advanced.
 * <p>
 * <p> Revision 1.1  2010/03/03 05:09:57  sueh
 * <p> bug# 1311 Changed CrossCorrelationPanel to TiltxcorrPanel.  Aded Patch
 * <p> Tracking functionality.
 * <p>
 * <p> Revision 3.35  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.34  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.33  2009/06/12 19:48:34  sueh
 * <p> bug# 1221 Factored running correlation, making it independent of the
 * <p> coarse align dialog.
 * <p>
 * <p> Revision 3.32  2009/03/24 20:27:13  sueh
 * <p> bug# 1201 Added angleOffset.
 * <p>
 * <p> Revision 3.31  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.30  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 3.29  2009/01/20 19:51:44  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 3.28  2008/05/28 02:49:46  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 3.27  2008/05/03 00:49:27  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 3.26  2007/09/10 20:42:24  sueh
 * <p> bug# 925 Should only load button states once.  Changed
 * <p> ProcessResultDisplayFactory to load button states immediately, so removing
 * <p> button state load in the dialogs.
 * <p>
 * <p> Revision 3.25  2007/03/21 19:45:28  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 3.24  2007/03/01 01:28:53  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 3.23  2007/02/09 00:48:42  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.22  2006/11/07 22:34:29  sueh
 * <p> bug# 954 Completing tooltips for XMinAndMax and YMinAndMax.
 * <p>
 * <p> Revision 3.21  2006/07/04 18:47:23  sueh
 * <p> bug# 893 Added updateAdvanced(boolean) to change the headers when the
 * <p> advanced button is pressed.
 * <p>
 * <p> Revision 3.20  2006/06/21 15:52:08  sueh
 * <p> bug# 581 Passing axis to ContextPopup, so that imodqtassist can be run.
 * <p>
 * <p> Revision 3.19  2006/03/30 16:48:02  sueh
 * <p> bug# 437 Passing the dialog type to getCrossCorrelateDIsplay().
 * <p>
 * <p> Revision 3.18  2006/03/27 21:01:10  sueh
 * <p> bug# 836 Moved btnCrossCorrelate to CrossCorrelationPanel.  Added a
 * <p> PanelHeader.
 * <p>
 * <p> Revision 3.17  2006/01/12 17:09:40  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 3.16  2006/01/03 23:34:36  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox
 * <p>
 * <p> Revision 3.15  2005/11/14 21:48:29  sueh
 * <p> bug# 762 Made buttonAction() protected.
 * <p>
 * <p> Revision 3.14  2005/08/27 22:35:36  sueh
 * <p> bug# 532 Changed Autodoc.get() to getInstance().
 * <p>
 * <p> Revision 3.13  2005/04/25 20:55:02  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.12  2005/02/11 16:45:29  sueh
 * <p> bug# 600 Getting tooltips using EtomoAutodoc instead of TooltipFormatter.
 * <p>
 * <p> Revision 3.11  2004/12/02 20:39:19  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
 * <p> Revision 3.10  2004/11/19 23:50:37  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.9.4.1  2004/10/11 02:12:43  sueh
 * <p> bug# 520 Passed the manager to the ContextPopup object in order to get
 * <p> the propertyUserDir.
 * <p>
 * <p> Revision 3.9  2004/05/03 18:03:49  sueh
 * <p> bug# 418 standardizing param gets and sets
 * <p>
 * <p> Revision 3.8  2004/04/07 21:03:10  rickg
 * <p> Fixed layout using UIUtilities
 * <p>
 * <p> Revision 3.7  2004/03/13 00:25:00  sueh
 * <p> bug# 412 Right justified checkboxes, changed labels.
 * <p>
 * <p> Revision 3.6  2004/03/12 20:09:33  sueh
 * <p> bug# 412 Added CrossCorrelationActionListener, cbAbsoluteCosineStretch,
 * <p> cbCumulativeCorrelation, cbNoCosineStretch, XMinAndMax, YMinAndMax,
 * <p> ltfTestOutput, updateCrossCorrelationPanel() - enables/disables fields.
 * <p> Removed ltfInputFile, ltfOutputFile.
 * <p>
 * <p> Revision 3.5  2004/03/11 19:43:45  sueh
 * <p> bug# 372 removing FilterSigma2, change text and order of FilterSigma1,
 * <p> FilterRadius2, FilterRadius1
 * <p>
 * <p> Revision 3.4  2004/02/05 04:49:22  rickg
 * <p> Added tiltxcorr border, simplified layout
 * <p>
 * <p> Revision 3.3  2004/01/30 01:30:26  sueh
 * <p> bug# 373 split filter parameters into four fields, changed
 * <p> tiltxcorrParam function calls
 * <p>
 * <p> Revision 3.2  2003/12/31 01:34:44  sueh
 * <p> bug# 372 tooltips moved to autodoc where possible, tooltips
 * <p> coming from autodoc
 * <p>
 * <p> Revision 3.1  2003/12/23 21:33:38  sueh
 * <p> bug# 372 Adding commented out test code.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.7  2003/11/06 22:45:01  sueh
 * <p> cleaning up tasks
 * <p>
 * <p> Revision 2.6  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.5  2003/10/28 23:35:48  rickg
 * <p> Bug# 336 Context menu label capitalization
 * <p>
 * <p> Revision 2.4  2003/10/20 20:08:37  sueh
 * <p> Bus322 corrected labels
 * <p>
 * <p> Revision 2.3  2003/10/14 21:56:34  sueh
 * <p> Bug273 add tooltips
 * <p>
 * <p> Revision 2.2  2003/10/13 23:00:48  sueh
 * <p> removed PieceListFile, rename fields, move field to advanced
 * <p>
 * <p> Revision 2.1  2003/09/09 17:15:02  rickg
 * <p> Changed view list to view range
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.2  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
final class TiltxcorrPanel implements Expandable, TiltXcorrDisplay,
    Run3dmodButtonContainer, ContextMenu {
  public static final String rcsid = "$Id$";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance(true);
  private final JPanel pnlBody = new JPanel();
  private final JPanel pnlAdvanced = new JPanel();
  private final JPanel pnlAdvanced2 = new JPanel();
  private final JPanel pnlXMinAndMax = new JPanel();
  private final JPanel pnlYMinAndMax = new JPanel();

  private final CheckBox cbExcludeCentralPeak = new CheckBox(
      "Exclude central peak due to fixed pattern noise");

  private final LabeledTextField ltfTestOutput = new LabeledTextField(FieldType.STRING,
      "Test output: ");
  private final LabeledTextField ltfFilterSigma1 = new LabeledTextField(
      FieldType.FLOATING_POINT, "Low frequency rolloff sigma: ");
  private final LabeledTextField ltfFilterRadius2 = new LabeledTextField(
      FieldType.FLOATING_POINT, "High frequency cutoff radius: ");
  private final LabeledTextField ltfFilterSigma2 = new LabeledTextField(
      FieldType.FLOATING_POINT, "High frequency rolloff sigma: ");
  private final LabeledTextField ltfTrim = new LabeledTextField(FieldType.INTEGER_PAIR,
      "Pixels to trim (x,y): ");
  private final LabeledTextField ltfXMin = new LabeledTextField(FieldType.INTEGER,
      "X axis min ");
  private final LabeledTextField ltfXMax = new LabeledTextField(FieldType.INTEGER, "Max ");
  private final LabeledTextField ltfYMin = new LabeledTextField(FieldType.INTEGER,
      "Y axis min ");
  private final LabeledTextField ltfYMax = new LabeledTextField(FieldType.INTEGER, "Max ");
  private final LabeledTextField ltfPadPercent = new LabeledTextField(
      FieldType.INTEGER_PAIR, "Pixels to pad (x,y): ");
  private final LabeledTextField ltfTaperPercent = new LabeledTextField(
      FieldType.INTEGER_PAIR, "Pixels to taper (x,y): ");
  private final CheckBox cbCumulativeCorrelation = new CheckBox("Cumulative correlation");
  private final CheckBox cbAbsoluteCosineStretch = new CheckBox("Absolute Cosine Stretch");
  private final CheckBox cbNoCosineStretch = new CheckBox("No Cosine Stretch");
  private final LabeledTextField ltfViewRange = new LabeledTextField(
      FieldType.INTEGER_PAIR, "View range (start,end): ");
  private final LabeledTextField ltfAngleOffset = new LabeledTextField(
      FieldType.FLOATING_POINT, "Tilt angle offset: ");
  private final CrossCorrelationActionListener actionListener = new CrossCorrelationActionListener(
      this);
  private final LabeledTextField ltfSkipViews = new LabeledTextField(
      FieldType.INTEGER_LIST, "Views to skip: ");

  // Patch tracking
  private final LabeledTextField ltfSizeOfPatchesXandY = new LabeledTextField(
      FieldType.INTEGER_PAIR, "Size of patches (X,Y): ");
  private final ButtonGroup bgPatchLayout = new ButtonGroup();
  private final RadioTextField rtfOverlapOfPatchesXandY = RadioTextField.getInstance(
      FieldType.FLOATING_POINT_PAIR, "Fractional overlap of patches (X,Y): ",
      bgPatchLayout);
  private final RadioTextField rtfNumberOfPatchesXandY = RadioTextField.getInstance(
      FieldType.INTEGER_PAIR, "Number of patches (X,Y): ", bgPatchLayout);
  private final Spinner spIterateCorrelations = Spinner.getLabeledInstance(
      "Iterations to increase subpixel accuracy: ",
      TiltxcorrParam.ITERATE_CORRELATIONS_DEFAULT,
      TiltxcorrParam.ITERATE_CORRELATIONS_MIN, TiltxcorrParam.ITERATE_CORRELATIONS_MAX);
  private final LabeledTextField ltfShiftLimitsXandY = new LabeledTextField(
      FieldType.INTEGER_PAIR, "Limits on shifts from correlation (X,Y): ");
  private final CheckTextField ctfLengthOfPiecesMinimumOverlap = CheckTextField
      .getInstance(FieldType.INTEGER, "Break contours into pieces with overlap: ");
  private final CheckBox cbBoundaryModel = new CheckBox("Use boundary model");
  private final Run3dmodButton btn3dmodBoundaryModel = Run3dmodButton.get3dmodInstance(
      "Create Boundary Model", this);
  private final ButtonGroup bgLengthOfPieces = new ButtonGroup();
  private final RadioButton rbLengthOfPiecesDefault = new RadioButton(
      "Use default length", bgLengthOfPieces);
  private RadioTextField rtfLengthOfPieces = RadioTextField.getInstance(
      FieldType.INTEGER, "Use length", bgLengthOfPieces);

  private final AxisID axisID;
  private final DialogType dialogType;
  private final MultiLineButton btnTiltxcorr;
  private final MultiLineButton btnImodchopconts;
  private final Run3dmodButton btn3dmodPatchTracking = Run3dmodButton.get3dmodInstance(
      "Open Tracked Patches", this);
  private final PanelHeader header;
  private final ApplicationManager applicationManager;
  private final PanelId panelId;
  private String skipViews = null;
  private final CheckTextField ctfMagChanges;
  private final boolean magChangesMode;

  final ContextMenu contextMenu;

  private TiltxcorrPanel(final ApplicationManager applicationManager, final AxisID id,
      final DialogType dialogType, final GlobalExpandButton globalAdvancedButton,
      final PanelId panelId, final ContextMenu contextMenu, final boolean magChangesMode) {
    this.dialogType = dialogType;
    axisID = id;
    this.applicationManager = applicationManager;
    this.panelId = panelId;
    this.contextMenu = contextMenu;
    this.magChangesMode = magChangesMode;
    header = PanelHeader.getAdvancedBasicInstance("Tiltxcorr", this, dialogType,
        globalAdvancedButton);

    ProcessResultDisplayFactory factory = applicationManager
        .getProcessResultDisplayFactory(axisID);
    btnTiltxcorr = (MultiLineButton) factory.getTiltxcorr(dialogType);
    btnImodchopconts = (MultiLineButton) factory.getImodchopconts();
    if (panelId == PanelId.PATCH_TRACKING) {
      ctfMagChanges = null;
      ((Run3dmodButton) btnTiltxcorr).setDeferred3dmodButton(btn3dmodPatchTracking);
      ((Run3dmodButton) btnTiltxcorr).setContainer(this);
    }
    else {
      ctfMagChanges = CheckTextField.getInstance(FieldType.INTEGER_LIST,
          "Find mag change at view(s):");
    }
  }

  static TiltxcorrPanel getCrossCorrelationInstance(
      final ApplicationManager applicationManager, final AxisID id,
      final DialogType dialogType, final GlobalExpandButton globalAdvancedButton,
      final ContextMenu contextMenu, final boolean magChangesMode) {
    TiltxcorrPanel instance = new TiltxcorrPanel(applicationManager, id, dialogType,
        globalAdvancedButton, PanelId.CROSS_CORRELATION, contextMenu, magChangesMode);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  static TiltxcorrPanel getPatchTrackingInstance(
      final ApplicationManager applicationManager, final AxisID id,
      final DialogType dialogType, final GlobalExpandButton globalAdvancedButton) {
    TiltxcorrPanel instance = new TiltxcorrPanel(applicationManager, id, dialogType,
        globalAdvancedButton, PanelId.PATCH_TRACKING, null, false);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // initialize
    if (btnTiltxcorr != null) {
      btnTiltxcorr.setSize();
    }
    btnImodchopconts.setSize();
    rbLengthOfPiecesDefault.setSelected(true);
    rbLengthOfPiecesDefault.setText("Use default length ("
        + ImodchopcontsParam.getLengthOfPiecesDefault(applicationManager, axisID,
            FileType.PREALIGNED_STACK) + ")");
    // root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    // Construct the min and max subpanels
    pnlXMinAndMax.setLayout(new BoxLayout(pnlXMinAndMax, BoxLayout.X_AXIS));
    UIUtilities.addWithXSpace(pnlXMinAndMax, ltfXMin.getContainer());
    UIUtilities.addWithXSpace(pnlXMinAndMax, ltfXMax.getContainer());

    pnlYMinAndMax.setLayout(new BoxLayout(pnlYMinAndMax, BoxLayout.X_AXIS));
    UIUtilities.addWithXSpace(pnlYMinAndMax, ltfYMin.getContainer());
    UIUtilities.addWithXSpace(pnlYMinAndMax, ltfYMax.getContainer());
    // advanced panel
    pnlAdvanced.setLayout(new BoxLayout(pnlAdvanced, BoxLayout.Y_AXIS));
    pnlAdvanced2.setLayout(new BoxLayout(pnlAdvanced2, BoxLayout.Y_AXIS));
    if (panelId == PanelId.CROSS_CORRELATION) {
      // Construct the advanced panel
      UIUtilities.addWithYSpace(pnlAdvanced, ltfAngleOffset.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced, ltfFilterSigma1.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced, ltfFilterRadius2.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced, ltfFilterSigma2.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced, ltfTrim.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced, pnlXMinAndMax);
      UIUtilities.addWithYSpace(pnlAdvanced, pnlYMinAndMax);
      UIUtilities.addWithYSpace(pnlAdvanced, ltfPadPercent.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced, ltfTaperPercent.getContainer());

      UIUtilities.addWithYSpace(pnlAdvanced2, cbCumulativeCorrelation);
      UIUtilities.addWithYSpace(pnlAdvanced2, cbAbsoluteCosineStretch);
      UIUtilities.addWithYSpace(pnlAdvanced2, cbNoCosineStretch);
      UIUtilities.addWithYSpace(pnlAdvanced2, cbExcludeCentralPeak);
      UIUtilities.addWithYSpace(pnlAdvanced2, ltfTestOutput.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced2, ltfViewRange.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced2, ltfSkipViews.getContainer());

      pnlBody.setLayout(new BoxLayout(pnlBody, BoxLayout.Y_AXIS));
      pnlBody.add(Box.createRigidArea(FixedDim.x0_y5));
      pnlBody.add(pnlAdvanced);
      if (ctfMagChanges != null) {
        pnlBody.add(ctfMagChanges.getComponent());
      }
      pnlBody.add(pnlAdvanced2);
      pnlBody.add(Box.createRigidArea(FixedDim.x0_y5));
      if (btnTiltxcorr != null) {
        pnlBody.add(btnTiltxcorr.getComponent());
      }
      pnlBody.add(Box.createRigidArea(FixedDim.x0_y5));
      UIUtilities.alignComponentsX(pnlBody, Component.CENTER_ALIGNMENT);
      UIUtilities.alignComponentsX(pnlAdvanced, Component.LEFT_ALIGNMENT);
      UIUtilities.alignComponentsX(pnlAdvanced2, Component.LEFT_ALIGNMENT);

      pnlRoot.setBorder(BorderFactory.createEtchedBorder());
      pnlRoot.add(header);
      pnlRoot.add(pnlBody);
    }
    else if (panelId == PanelId.PATCH_TRACKING) {
      // initialize
      rtfOverlapOfPatchesXandY.setText(TiltxcorrParam.OVERLAP_OF_PATCHES_X_AND_Y_DEFAULT);
      ctfLengthOfPiecesMinimumOverlap.setTextPreferredWidth(30);
      btn3dmodPatchTracking.setSize();
      btn3dmodBoundaryModel.setSize();
      rtfOverlapOfPatchesXandY.setSelected(true);
      ltfTrim.setText(TiltxcorrParam.getBordersInXandYDefault(applicationManager, axisID,
          FileType.PREALIGNED_STACK));
      ltfFilterSigma1.setText(TiltxcorrParam.FILTER_SIGMA_1_DEFAULT);
      ltfFilterRadius2.setText(TiltxcorrParam.FILTER_RADIUS_2_DEFAULT);
      ltfFilterSigma2.setText(TiltxcorrParam.FILTER_SIGMA_2_DEFAULT);
      // local panels
      JPanel pnlPatchLayout = new JPanel();
      JPanel pnlBoundaryModel = new JPanel();
      JPanel pnlButtons = new JPanel();
      JPanel pnlImodchopconts = new JPanel();
      JPanel pnlLengthOfPieces = new JPanel();
      // root panel
      pnlRoot.setBorder(new EtchedBorder("Patch Tracking").getBorder());
      pnlRoot.add(ltfSizeOfPatchesXandY.getContainer());
      pnlRoot.add(pnlPatchLayout);
      pnlRoot.add(pnlBoundaryModel);
      JPanel pnlIterateCorrelations = new JPanel();
      pnlIterateCorrelations.setLayout(new BoxLayout(pnlIterateCorrelations,
          BoxLayout.X_AXIS));
      pnlIterateCorrelations.setAlignmentX(Component.CENTER_ALIGNMENT);
      pnlIterateCorrelations.add(spIterateCorrelations.getContainer());
      pnlIterateCorrelations.add(Box.createHorizontalGlue());
      pnlRoot.add(pnlIterateCorrelations);
      pnlRoot.add(ltfShiftLimitsXandY.getContainer());
      pnlRoot.add(ctfLengthOfPiecesMinimumOverlap.getRootComponent());
      pnlRoot.add(pnlLengthOfPieces);
      pnlRoot.add(ltfAngleOffset);
      pnlRoot.add(pnlAdvanced2);
      pnlRoot.add(ltfTrim);
      pnlRoot.add(pnlXMinAndMax);
      pnlRoot.add(pnlYMinAndMax);
      pnlRoot.add(pnlAdvanced);
      pnlRoot.add(pnlButtons);
      pnlRoot.add(pnlImodchopconts);
      // patch layout panel
      pnlPatchLayout.setLayout(new BoxLayout(pnlPatchLayout, BoxLayout.Y_AXIS));
      pnlPatchLayout.setBorder(new EtchedBorder("Patch Layout").getBorder());
      pnlPatchLayout.add(rtfOverlapOfPatchesXandY.getContainer());
      pnlPatchLayout.add(rtfNumberOfPatchesXandY.getContainer());
      // boundary model panel
      pnlBoundaryModel.setLayout(new BoxLayout(pnlBoundaryModel, BoxLayout.X_AXIS));
      pnlBoundaryModel.add(cbBoundaryModel);
      pnlBoundaryModel.add(Box.createHorizontalGlue());
      pnlBoundaryModel.add(btn3dmodBoundaryModel.getComponent());
      pnlBoundaryModel.add(Box.createHorizontalGlue());
      // LengthOfPieces
      pnlLengthOfPieces.setLayout(new BoxLayout(pnlLengthOfPieces, BoxLayout.X_AXIS));
      pnlLengthOfPieces.add(rbLengthOfPiecesDefault.getComponent());
      pnlLengthOfPieces.add(Box.createRigidArea(FixedDim.x10_y0));
      pnlLengthOfPieces.add(rtfLengthOfPieces.getContainer());
      // advanced 2 panel
      UIUtilities.addWithYSpace(pnlAdvanced2, ltfFilterSigma1.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced2, ltfFilterRadius2.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced2, ltfFilterSigma2.getContainer());
      // advanced panel
      UIUtilities.addWithYSpace(pnlAdvanced, ltfPadPercent.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced, ltfTaperPercent.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced, ltfTestOutput.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced, ltfViewRange.getContainer());
      UIUtilities.addWithYSpace(pnlAdvanced, ltfSkipViews.getContainer());
      // button panel
      pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
      pnlButtons.add(Box.createHorizontalGlue());
      if (btnTiltxcorr != null) {
        pnlButtons.add(btnTiltxcorr.getComponent());
      }
      pnlButtons.add(Box.createHorizontalGlue());
      pnlButtons.add(btn3dmodPatchTracking.getComponent());
      pnlButtons.add(Box.createHorizontalGlue());
      // Imodchopconts
      pnlImodchopconts.setLayout(new BoxLayout(pnlImodchopconts, BoxLayout.X_AXIS));
      pnlImodchopconts.add(Box.createHorizontalGlue());
      pnlImodchopconts.add(btnImodchopconts.getComponent());
      pnlImodchopconts.add(Box.createHorizontalGlue());
      updatePanel();
    }
  }

  private void addListeners() {
    pnlRoot.addMouseListener(new GenericMouseAdapter(this));
    cbCumulativeCorrelation.addActionListener(actionListener);
    cbNoCosineStretch.addActionListener(actionListener);
    if (btnTiltxcorr != null) {
      btnTiltxcorr.addActionListener(actionListener);
    }
    btn3dmodPatchTracking.addActionListener(actionListener);
    cbBoundaryModel.addActionListener(actionListener);
    btn3dmodBoundaryModel.addActionListener(actionListener);
    if (ctfMagChanges != null) {
      ctfMagChanges.addActionListener(actionListener);
    }
    btnImodchopconts.addActionListener(actionListener);
    ctfLengthOfPiecesMinimumOverlap.addActionListener(actionListener);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(final MouseEvent mouseEvent) {
    if (panelId != PanelId.PATCH_TRACKING) {
      if (contextMenu != null) {
        contextMenu.popUpContextMenu(mouseEvent);
      }
      return;
    }
    String[] manPagelabel = { "Tiltxcorr", "Imodchopconts", "3dmod" };
    String[] manPage = { "tiltxcorr.html", "imodchopconts.html", "3dmod.html" };

    String[] logFileLabel = { "Xcorr_pt" };
    String[] logFile = { "xcorr_pt" + axisID.getExtension() + ".log" };
    new ContextPopup(pnlRoot.getContainer(), mouseEvent, "PatchTracking",
        ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel, logFile,
        applicationManager, axisID);
  }

  public PanelId getPanelId() {
    return panelId;
  }

  void done() {
    btnTiltxcorr.removeActionListener(actionListener);
    btnImodchopconts.removeActionListener(actionListener);
  }

  void updateAdvanced(final boolean state) {
    pnlAdvanced.setVisible(state);
    pnlAdvanced2.setVisible(state);
    if (panelId == PanelId.PATCH_TRACKING) {
      ltfAngleOffset.setVisible(state);
      ltfShiftLimitsXandY.setVisible(state);
    }
    // If magChangesMode is true, then the mag changes fields are not advanced fields.
    if (!magChangesMode && ctfMagChanges != null) {
      ctfMagChanges.setVisible(state);
    }
  }

  /**
   * All expansion is done through the header.
   */
  public void expand(final GlobalExpandButton button) {
  }

  public void expand(final ExpandButton button) {
    if (header.equalsOpenClose(button)) {
      pnlBody.setVisible(button.isExpanded());
    }
    else if (header.equalsAdvancedBasic(button)) {
      updateAdvanced(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  Container getPanel() {
    return pnlRoot.getContainer();
  }

  void setParameters(final ImodchopcontsParam param) {
    if (panelId == PanelId.PATCH_TRACKING) {
      ctfLengthOfPiecesMinimumOverlap.setText(param.getMinimumOverlap());
      boolean lengthOfPiecesSet = !param.isLengthOfPiecesNull();
      ctfLengthOfPiecesMinimumOverlap.setSelected(lengthOfPiecesSet);
      if (lengthOfPiecesSet) {
        if (param.isLengthOfPiecesDefault()) {
          rbLengthOfPiecesDefault.setSelected(true);
        }
        else {
          rtfLengthOfPieces.setSelected(true);
          rtfLengthOfPieces.setText(param.getLengthOfPieces());
        }
      }
      updatePanel();
    }
  }

  /**
   * Set the field values for the panel from the ConstTiltxcorrParam object
   */
  void setParameters(final ConstTiltxcorrParam tiltXcorrParams) {
    ltfAngleOffset.setText(tiltXcorrParams.getAngleOffset());
    // Avoid overriding the default
    if (tiltXcorrParams.isBordersInXandYSet()) {
      ltfTrim.setText(tiltXcorrParams.getBordersInXandY());
    }
    ltfXMin.setText(tiltXcorrParams.getXMinString());
    ltfXMax.setText(tiltXcorrParams.getXMaxString());
    ltfYMin.setText(tiltXcorrParams.getYMinString());
    ltfYMax.setText(tiltXcorrParams.getYMaxString());
    ltfPadPercent.setText(tiltXcorrParams.getPadsInXandYString());
    ltfTaperPercent.setText(tiltXcorrParams.getTaperPercentString());
    ltfTestOutput.setText(tiltXcorrParams.getTestOutput());
    ltfViewRange.setText(tiltXcorrParams.getStartingEndingViews());
    ltfSkipViews.setText(tiltXcorrParams.getSkipViews());
    if (tiltXcorrParams.isFilterSigma1Set()) {
      ltfFilterSigma1.setText(tiltXcorrParams.getFilterSigma1String());
    }
    if (tiltXcorrParams.isFilterRadius2Set()) {
      ltfFilterRadius2.setText(tiltXcorrParams.getFilterRadius2String());
    }
    if (tiltXcorrParams.isFilterSigma2Set()) {
      ltfFilterSigma2.setText(tiltXcorrParams.getFilterSigma2String());
    }
    if (panelId == PanelId.CROSS_CORRELATION) {
      cbExcludeCentralPeak.setSelected(tiltXcorrParams.getExcludeCentralPeak());
      cbCumulativeCorrelation.setSelected(tiltXcorrParams.isCumulativeCorrelation());
      cbAbsoluteCosineStretch.setSelected(tiltXcorrParams.isAbsoluteCosineStretch());
      cbNoCosineStretch.setSelected(tiltXcorrParams.isNoCosineStretch());
    }
    else if (panelId == PanelId.PATCH_TRACKING) {
      ltfSizeOfPatchesXandY.setText(tiltXcorrParams.getSizeOfPatchesXandY());
      if (tiltXcorrParams.isOverlapOfPatchesXandYSet()) {
        rtfOverlapOfPatchesXandY.setSelected(true);
        rtfOverlapOfPatchesXandY.setText(tiltXcorrParams.getOverlapOfPatchesXandY());
      }
      if (tiltXcorrParams.isNumberOfPatchesXandYSet()) {
        rtfNumberOfPatchesXandY.setSelected(true);
        rtfNumberOfPatchesXandY.setText(tiltXcorrParams.getNumberOfPatchesXandY());
      }
      spIterateCorrelations.setValue(tiltXcorrParams.getIterateCorrelations());
      ltfShiftLimitsXandY.setText(tiltXcorrParams.getShiftLimitsXandY());
      cbBoundaryModel.setSelected(tiltXcorrParams.isBoundaryModelSet());
    }
    if (ctfMagChanges != null) {
      ctfMagChanges.setSelected(tiltXcorrParams.isSearchMagChanges());
      ctfMagChanges.setText(tiltXcorrParams.getViewsWithMagChanges());
    }
    updatePanel();
  }

  /**
   * Load parameters which can be inactivated before loading from TiltxcorrParam.
   * @param metaData
   */
  void setParameters(ConstMetaData metaData) {
    if (panelId == PanelId.PATCH_TRACKING) {
      // Don't override defaults unless there is a value in meta data
      if (metaData.isTrackOverlapOfPatchesXandYSet(axisID)) {
        rtfOverlapOfPatchesXandY.setText(metaData.getTrackOverlapOfPatchesXandY(axisID));
      }
      rtfNumberOfPatchesXandY.setText(metaData.getTrackNumberOfPatchesXandY(axisID));
      // Backwards compatibility
      if (metaData.isTrackLengthAndOverlapSet(axisID)) {
        ctfLengthOfPiecesMinimumOverlap.setText(metaData.getMinimumOverlap(axisID));
      }
      rtfLengthOfPieces.setText(metaData.getLengthOfPieces(axisID));
    }
  }

  /**
   * Save parameters which can be inactivated to meta data.
   * @param metaData
   */
  void getParameters(MetaData metaData) {
    if (panelId == PanelId.PATCH_TRACKING) {
      metaData.setTrackOverlapOfPatchesXandY(axisID, rtfOverlapOfPatchesXandY.getText());
      metaData.setTrackNumberOfPatchesXandY(axisID, rtfNumberOfPatchesXandY.getText());
      metaData.setLengthOfPieces(axisID, rtfLengthOfPieces.getText());
      // MinimumOverlap does not have to be saved because it can be placed in the
      // comscript even when it is disabled. It is in meta data for backwards
      // compatibility, and is loaded from trackLengthAndOverlap.
    }
  }

  void setParameters(BaseScreenState screenState) {
    // btnCrossCorrelate.setButtonState(screenState
    // .getButtonState(btnCrossCorrelate.getButtonStateKey()));
    header.setButtonStates(screenState);
  }

  void getParameters(BaseScreenState screenState) {
    header.getButtonStates(screenState);
  }

  public boolean getParameters(final ImodchopcontsParam param, final boolean doValidation) {
    try {
      if (panelId == PanelId.PATCH_TRACKING) {
        param.setMinimumOverlap(ctfLengthOfPiecesMinimumOverlap.getText(doValidation));
        if (ctfLengthOfPiecesMinimumOverlap.isSelected()) {
          if (rbLengthOfPiecesDefault.isSelected()) {
            param.setLengthOfPiecesDefault();
          }
          else {
            param.setLengthOfPieces(rtfLengthOfPieces.getText(doValidation));
          }
        }
        else {
          param.resetLengthOfPieces();
        }
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  /**
   * Get the field values from the panel filling in the TiltxcorrParam object
   * @return false if there was a field error
   */
  public boolean getParameters(final TiltxcorrParam tiltXcorrParams,
      final boolean doValidation) throws FortranInputSyntaxException {
    try {
      tiltXcorrParams.setTestOutput(ltfTestOutput.getText(doValidation));
      if (panelId == PanelId.CROSS_CORRELATION) {
        tiltXcorrParams.setExcludeCentralPeak(cbExcludeCentralPeak.isSelected());
      }
      else if (panelId == PanelId.PATCH_TRACKING) {
        String errorMessage = tiltXcorrParams
            .setIterateCorrelations(spIterateCorrelations.getValue());
        if (errorMessage != null) {
          UIHarness.INSTANCE.openMessageDialog(applicationManager,
              spIterateCorrelations.getLabel() + ": " + errorMessage, "Entry Error",
              axisID);
          return false;
        }
        tiltXcorrParams.setInputFile(FileType.PREALIGNED_STACK.getFileName(
            applicationManager, axisID));
        tiltXcorrParams.setOutputFile(FileType.FIDUCIAL_PATCH_TRACKING_MODEL.getFileName(
            applicationManager, axisID));
        if (cbBoundaryModel.isSelected()) {
          tiltXcorrParams.setBoundaryModel(FileType.PATCH_TRACKING_BOUNDARY_MODEL
              .getFileName(applicationManager, axisID));
        }
        else {
          tiltXcorrParams.resetBoundaryModel();
        }
        MetaData metaData = applicationManager.getMetaData();
        TiltAngleSpec tiltAngleSpec = metaData.getTiltAngleSpec(axisID);
        tiltXcorrParams.setTiltAngleSpec(tiltAngleSpec);
        tiltXcorrParams.setRotationAngle(metaData.getImageRotation(axisID).getDouble());
      }
      String currentParam = "unknown";
      try {
        currentParam = ltfAngleOffset.getLabel();
        tiltXcorrParams.setAngleOffset(ltfAngleOffset.getText(doValidation));
        currentParam = ltfTrim.getLabel();
        tiltXcorrParams.setBordersInXandY(ltfTrim.getText(doValidation));
        currentParam = "X" + ltfXMin.getLabel();
        tiltXcorrParams.setXMin(ltfXMin.getText(doValidation));
        currentParam = "X" + ltfXMax.getLabel();
        tiltXcorrParams.setXMax(ltfXMax.getText(doValidation));
        currentParam = "Y" + ltfYMin.getLabel();
        tiltXcorrParams.setYMin(ltfYMin.getText(doValidation));
        currentParam = "Y" + ltfYMax.getLabel();
        tiltXcorrParams.setYMax(ltfYMax.getText(doValidation));
        currentParam = ltfPadPercent.getLabel();
        tiltXcorrParams.setPadsInXandY(ltfPadPercent.getText(doValidation));
        currentParam = ltfTaperPercent.getLabel();
        tiltXcorrParams.setTapersInXandY(ltfTaperPercent.getText(doValidation));
        currentParam = ltfViewRange.getLabel();
        tiltXcorrParams.setStartingEndingViews(ltfViewRange.getText(doValidation));
        currentParam = ltfSkipViews.getLabel();
        tiltXcorrParams.setSkipViews(ltfSkipViews.getText(doValidation));
        currentParam = ltfFilterSigma1.getLabel();
        tiltXcorrParams.setFilterSigma1(ltfFilterSigma1.getText(doValidation));
        currentParam = ltfFilterRadius2.getLabel();
        tiltXcorrParams.setFilterRadius2(ltfFilterRadius2.getText(doValidation));
        currentParam = ltfFilterSigma2.getLabel();
        tiltXcorrParams.setFilterSigma2(ltfFilterSigma2.getText(doValidation));
        if (panelId == PanelId.CROSS_CORRELATION) {
          currentParam = cbCumulativeCorrelation.getText();
          tiltXcorrParams.setCumulativeCorrelation(cbCumulativeCorrelation.isSelected());
          currentParam = cbAbsoluteCosineStretch.getText();
          tiltXcorrParams.setAbsoluteCosineStretch(cbAbsoluteCosineStretch.isSelected());
          currentParam = cbNoCosineStretch.getText();
          tiltXcorrParams.setNoCosineStretch(cbNoCosineStretch.isSelected());
        }
        else if (panelId == PanelId.PATCH_TRACKING) {
          currentParam = ltfSizeOfPatchesXandY.getLabel();
          if (!tiltXcorrParams.setSizeOfPatchesXandY(
              ltfSizeOfPatchesXandY.getText(doValidation),
              ltfSizeOfPatchesXandY.getLabel())) {
            return false;
          }
          currentParam = rtfOverlapOfPatchesXandY.getLabel();
          if (rtfOverlapOfPatchesXandY.isSelected()) {
            tiltXcorrParams.setOverlapOfPatchesXandY(rtfOverlapOfPatchesXandY
                .getText(doValidation));
          }
          else {
            tiltXcorrParams.resetOverlapOfPatchesXandY();
          }
          currentParam = rtfNumberOfPatchesXandY.getLabel();
          if (rtfNumberOfPatchesXandY.isSelected()) {
            tiltXcorrParams.setNumberOfPatchesXandY(rtfNumberOfPatchesXandY
                .getText(doValidation));
          }
          else {
            tiltXcorrParams.resetNumberOfPatchesXandY();
          }
          currentParam = ltfShiftLimitsXandY.getLabel();
          tiltXcorrParams.setShiftLimitsXandY(ltfShiftLimitsXandY.getText(doValidation));
          tiltXcorrParams.setPrealignmentTransformFileDefault();
          tiltXcorrParams.setImagesAreBinned(UIExpertUtilities.INSTANCE.getStackBinning(
              applicationManager, axisID, ".preali"));
        }
        if (ctfMagChanges != null) {
          tiltXcorrParams.setSearchMagChanges(ctfMagChanges.isSelected());
          tiltXcorrParams.setViewsWithMagChanges(ctfMagChanges.getText(doValidation));
        }
      }
      catch (FortranInputSyntaxException except) {
        String message = currentParam + except.getMessage();
        throw new FortranInputSyntaxException(message);
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  void setVisible(final boolean state) {
    pnlRoot.setVisible(state);
  }

  void updatePanel() {
    if (cbCumulativeCorrelation.isSelected() && !cbNoCosineStretch.isSelected()) {
      cbAbsoluteCosineStretch.setEnabled(true);
    }
    else {
      cbAbsoluteCosineStretch.setSelected(false);
      cbAbsoluteCosineStretch.setEnabled(false);
    }
    btn3dmodBoundaryModel.setEnabled(cbBoundaryModel.isSelected());
    if (ctfMagChanges != null) {
      cbCumulativeCorrelation.setEnabled(!ctfMagChanges.isSelected()
          || !ctfMagChanges.isEnabled());
      ctfMagChanges.setEnabled(!cbCumulativeCorrelation.isSelected()
          || !cbCumulativeCorrelation.isEnabled());
    }
    boolean enable = ctfLengthOfPiecesMinimumOverlap.isSelected();
    rbLengthOfPiecesDefault.setEnabled(enable);
    rtfLengthOfPieces.setEnabled(enable);
  }

  private boolean validate() {
    if (panelId == PanelId.PATCH_TRACKING) {
      if (ltfSizeOfPatchesXandY.isEmpty()) {
        UIHarness.INSTANCE.openMessageDialog(applicationManager,
            ltfSizeOfPatchesXandY.getLabel() + " is required.", "Entry Error", axisID);
        return false;
      }
    }
    return true;
  }

  public void action(final Run3dmodButton button, final Run3dmodMenuOptions menuOptions) {
    buttonAction(button.getActionCommand(), button.getDeferred3dmodButton(), menuOptions);
  }

  // Action functions for setup panel buttons
  void buttonAction(final String actionCommand,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    boolean runTiltxcorr = btnTiltxcorr != null
        && actionCommand.equals(btnTiltxcorr.getActionCommand());
    if (runTiltxcorr && panelId == PanelId.CROSS_CORRELATION) {
      applicationManager.preCrossCorrelate(axisID, btnTiltxcorr, null, dialogType, this);
    }
    else if ((runTiltxcorr && panelId == PanelId.PATCH_TRACKING)
        || actionCommand.equals(btnImodchopconts.getActionCommand())) {
      // The validations do not cover imodchopconts fields
      if (runTiltxcorr && !validate()) {
        return;
      }
      applicationManager.tiltxcorr(axisID,
          runTiltxcorr ? btnTiltxcorr : btnImodchopconts, deferred3dmodButton,
          run3dmodMenuOptions, null, dialogType, this, false,
          FileType.PATCH_TRACKING_COMSCRIPT, runTiltxcorr,
          ctfLengthOfPiecesMinimumOverlap.isSelected());
    }
    else if (actionCommand.equals(btn3dmodPatchTracking.getActionCommand())) {
      applicationManager.imodModel(FileType.PREALIGNED_STACK, FileType.FIDUCIAL_MODEL,
          axisID, run3dmodMenuOptions, false);
    }
    else if (actionCommand.equals(btn3dmodBoundaryModel.getActionCommand())) {
      applicationManager.imodModel(FileType.PREALIGNED_STACK,
          FileType.PATCH_TRACKING_BOUNDARY_MODEL, axisID, run3dmodMenuOptions, false);
    }
    else {
      updatePanel();
    }
  }

  private static class CrossCorrelationActionListener implements ActionListener {
    private final TiltxcorrPanel adaptee;

    public CrossCorrelationActionListener(final TiltxcorrPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.buttonAction(event.getActionCommand(), null, null);
    }
  }

  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    String text;
    ReadOnlyAutodoc autodoc = null;

    try {
      autodoc = AutodocFactory.getInstance(applicationManager, AutodocFactory.TILTXCORR,
          axisID);
      // autodoc.print();
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
    ltfTestOutput.setToolTipText(EtomoAutodoc.getTooltip(autodoc, "TestOutput"));
    ltfFilterSigma1.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltxcorrParam.FILTER_SIGMA1_KEY));
    ltfFilterRadius2.setToolTipText(EtomoAutodoc.getTooltip(autodoc, "FilterRadius2"));
    ltfFilterSigma2.setToolTipText(EtomoAutodoc.getTooltip(autodoc, "FilterSigma2"));
    ltfTrim.setToolTipText(EtomoAutodoc.getTooltip(autodoc, "BordersInXandY"));
    if (ctfMagChanges != null) {
      ctfMagChanges.setCheckBoxToolTipText(EtomoAutodoc.getTooltip(autodoc,
          TiltxcorrParam.SEARCH_MAG_CHANGES_KEY));
      ctfMagChanges.setCheckBoxToolTipText(EtomoAutodoc.getTooltip(autodoc,
          TiltxcorrParam.VIEWS_WITH_MAG_CHANGES_KEY));
    }
    text = EtomoAutodoc.getTooltip(autodoc, "XMinAndMax");
    if (text != null) {
      pnlXMinAndMax.setToolTipText(text);
      ltfXMin.setToolTipText(text);
      ltfXMax.setToolTipText(text);
    }
    text = EtomoAutodoc.getTooltip(autodoc, "YMinAndMax");
    if (text != null) {
      pnlYMinAndMax.setToolTipText(text);
      ltfYMin.setToolTipText(text);
      ltfYMax.setToolTipText(text);
    }
    ltfPadPercent.setToolTipText(EtomoAutodoc.getTooltip(autodoc, "PadsInXandY"));
    ltfTaperPercent.setToolTipText(EtomoAutodoc.getTooltip(autodoc, "TapersInXandY"));
    cbCumulativeCorrelation.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        "CumulativeCorrelation"));
    cbAbsoluteCosineStretch.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        "AbsoluteCosineStretch"));
    cbNoCosineStretch.setToolTipText(EtomoAutodoc.getTooltip(autodoc, "NoCosineStretch"));
    ltfViewRange.setToolTipText(EtomoAutodoc.getTooltip(autodoc, "StartingEndingViews"));
    ltfSkipViews.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltxcorrParam.SKIP_VIEWS_KEY));
    cbExcludeCentralPeak.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        "ExcludeCentralPeak"));
    ltfAngleOffset.setToolTipText(EtomoAutodoc.getTooltip(autodoc, "AngleOffset"));
    if (btnTiltxcorr != null) {
      btnTiltxcorr.setToolTipText("Find alignment transformations between successive "
          + "images by cross-correlation.");
    }
    btn3dmodPatchTracking
        .setToolTipText("Open the pre-aligned stack with the patch tracking "
            + "fiducial model.");
    ltfSizeOfPatchesXandY.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltxcorrParam.SIZE_OF_PATCHES_X_AND_Y_KEY));
    rtfOverlapOfPatchesXandY.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltxcorrParam.OVERLAP_OF_PATCHES_X_AND_Y_KEY));
    rtfNumberOfPatchesXandY.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltxcorrParam.NUMBER_OF_PATCHES_X_AND_Y_KEY));
    spIterateCorrelations.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltxcorrParam.ITERATE_CORRELATIONS_KEY));
    ltfShiftLimitsXandY.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltxcorrParam.SHIFT_LIMITS_X_AND_Y_KEY));
    String tooltip = EtomoAutodoc.getTooltip(autodoc, TiltxcorrParam.BOUNDARY_MODEL_KEY);
    cbBoundaryModel.setToolTipText(tooltip);
    btn3dmodBoundaryModel.setToolTipText(tooltip);
    ctfLengthOfPiecesMinimumOverlap.setCheckBoxToolTipText(EtomoAutodoc.getTooltip(
        autodoc, ImodchopcontsParam.LENGTH_OF_PIECES_KEY));
    ctfLengthOfPiecesMinimumOverlap.setFieldToolTipText(EtomoAutodoc.getTooltip(autodoc,
        ImodchopcontsParam.MINIMUM_OVERLAP_KEY));
    rbLengthOfPiecesDefault.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        ImodchopcontsParam.LENGTH_OF_PIECES_KEY));
    rtfLengthOfPieces.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        ImodchopcontsParam.LENGTH_OF_PIECES_KEY));
    btnImodchopconts
        .setToolTipText("Changes the contour pieces without rerunning tiltaxcorr.");
  }
}