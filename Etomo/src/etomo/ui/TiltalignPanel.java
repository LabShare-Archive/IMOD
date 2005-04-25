package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;

import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.TiltalignParam;
import etomo.type.AxisID;
import etomo.type.EtomoAutodoc;
import etomo.util.FidXyz;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

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
 */

public class TiltalignPanel {
  public static final String rcsid = "$Id$";

  private AxisID axisID;

  private int prealignedBinning = 1;
  /*
   //  TODO need recomended default for all sub groups see (align.com)
   private final int defaultTiltAngleType = 5;
   private final int defaultTiltAngleGroupSize = 5;
   private final int defaultMagnificationType = 3;
   private final int defaultDistortionType = 2;
   private final int defaultXstretchType = 3;
   private final int defaultXstretchGroupSize = 7;
   private final int defaultSkewType = 3;
   private final int defaultSkewGroupSize = 11;

   private final int defaultLocalRotationType = 3;
   private final int defaultLocalRotationGroupSize = 6;
   private final int defaultLocalTiltAngleType = 5;
   private final int defaultLocalTiltAngleGroupSize = 6;
   private final int defaultLocalMagnificationType = 3;
   private final int defaultLocalMagnificationGroupSize = 7;
   private final int defaultLocalDistortionType = 2;
   private final int defaultLocalXstretchType = 3;
   private final int defaultLocalXstretchGroupSize = 7;
   private final int defaultLocalSkewType = 3;
   private final int defaultLocalSkewGroupSize = 11;*/

  private JTabbedPane tabPane = new JTabbedPane();

  //  General pane
  private JPanel pnlGeneral = new JPanel();

  private LabeledTextField ltfResidualThreshold = new LabeledTextField(
      "Threshold for residual report: ");

  private JRadioButton rbResidAllViews = new JRadioButton("All views");
  private JRadioButton rbResidNeighboring = new JRadioButton(
      "Neighboring views");
  private ButtonGroup bgResidualThreshold = new ButtonGroup();
  private JPanel pnlResidualThreshold = new JPanel();

  private JRadioButton rbSingleFiducialSurface = new JRadioButton(
      "Assume fiducials on 1 surface for analysis");
  private JRadioButton rbDualFiducialSurfaces = new JRadioButton(
      "Assume fiducials on 2 surfaces for analysis");
  private ButtonGroup bgFiducialSurfaces = new ButtonGroup();
  private JPanel pnlFiducialSurfaces = new JPanel();

  private LabeledTextField ltfExcludeList = new LabeledTextField(
      "List of views to exclude: ");
  private LabeledTextField ltfSeparateViewGroups = new LabeledTextField(
      "Separate view groups: ");

  private JPanel pnlVolumeParameters = new JPanel();
  private LabeledTextField ltfTiltAngleOffset = new LabeledTextField(
      "Total tilt angle offset: ");
  private LabeledTextField ltfTiltAxisZShift = new LabeledTextField(
      "Tilt axis z shift: ");

  private JPanel pnlMinimizationParams = new JPanel();
  private LabeledTextField ltfMetroFactor = new LabeledTextField(
      "Metro factor: ");
  private LabeledTextField ltfCycleLimit = new LabeledTextField("Cycle limit: ");

  private JPanel pnlLocalParameters = new JPanel();
  private JCheckBox cbLocalAlignments = new JCheckBox("Enable local alignments");
  private LabeledTextField ltfNLocalPatches = new LabeledTextField(
      "Number of local patches (x,y): ");
  private LabeledTextField ltfMinLocalPatchSize = new LabeledTextField(
      "Min. local patch size or overlap factor (x,y): ");
  private LabeledTextField ltfMinLocalFiducials = new LabeledTextField(
      "Min. # of fiducials (total, each surface): ");

  //  Global variables pane
  private JPanel pnlGlobalVariable = new JPanel();

  //  Tilt angle pane
  private JRadioButton rbTiltAngleFixed = new JRadioButton("Fixed tilt angles");
  private JRadioButton rbTiltAngleAll = new JRadioButton(
      "Solve for all except minimum tilt");
  private JRadioButton rbTiltAngleAutomap = new JRadioButton(
      "Group tilt angles ");
  private ButtonGroup bgTiltAngleSolution = new ButtonGroup();
  private JPanel pnlTiltAngleSolution = new JPanel();

  private LabeledTextField ltfTiltAngleGroupSize = new LabeledTextField(
      "Group size: ");
  private LabeledTextField ltfTiltAngleNonDefaultGroups = new LabeledTextField(
      "Non-default grouping: ");

  //  Magnfication pane
  private JRadioButton rbMagnificationFixed = new JRadioButton(
      "Fixed magnification at 1.0");
  private JRadioButton rbMagnificationAll = new JRadioButton(
      "Solve for all magnifications");
  private JRadioButton rbMagnificationAutomap = new JRadioButton(
      "Group magnifications");
  private ButtonGroup bgMagnificationSolution = new ButtonGroup();
  private JPanel pnlMagnificationSolution = new JPanel();

  private LabeledTextField ltfMagnificationReferenceView = new LabeledTextField(
      "Reference view: ");
  private LabeledTextField ltfMagnificationGroupSize = new LabeledTextField(
      "Group size: ");
  private LabeledTextField ltfMagnificationNonDefaultGroups = new LabeledTextField(
      "Non-default grouping: ");

  //  Compression pane
  /*  private JRadioButton rbCompressionAll =
   new JRadioButton("Solve for all Compressions");
   private JRadioButton rbCompressionAutomapLinear =
   new JRadioButton("Group compression (first order fit)");
   private JRadioButton rbCompressionAutomapFixed =
   new JRadioButton("Group compression (zeroth order fit)");
   private ButtonGroup bgCompressionSolution = new ButtonGroup();
   private JPanel pnlCompressionSolution = new JPanel();
   
   private LabeledTextField ltfCompressionReferenceView =
   new LabeledTextField("Compression reference view: ");
   private LabeledTextField ltfCompressionGroupSize =
   new LabeledTextField("Group size: ");
   private LabeledTextField ltfCompressionAdditionalGroups =
   new LabeledTextField("Additional group list: ");
   */
  // GlobalDistortion pane
  private JPanel pnlDistortionSolution = new JPanel();
  private JRadioButton rbDistortionDisabled = new JRadioButton("Disabled");
  private JRadioButton rbDistortionFullSolution = new JRadioButton(
      "Full solution");
  private JRadioButton rbDistortionSkew = new JRadioButton("Skew only");
  private ButtonGroup bgDistortionSolution = new ButtonGroup();

  private LabeledTextField ltfXstretchGroupSize = new LabeledTextField(
      "X stretch group size: ");
  private LabeledTextField ltfXstretchNonDefaultGroups = new LabeledTextField(
      "X stretch non-default grouping: ");

  private LabeledTextField ltfSkewGroupSize = new LabeledTextField(
      "Skew group size: ");
  private LabeledTextField ltfSkewNonDefaultGroups = new LabeledTextField(
      "Skew non-default grouping: ");

  //  Local variables pane
  private JPanel pnlLocalSolution = new JPanel();

  //  Local rotation pane
  private JPanel pnlLocalRotationSolution = new JPanel();
  private JCheckBox cbLocalRotation = new JCheckBox("Enable");

  private LabeledTextField ltfLocalRotationGroupSize = new LabeledTextField(
      "Group size: ");
  private LabeledTextField ltfLocalRotationNonDefaultGroups = new LabeledTextField(
      "Non-default grouping: ");

  //  Local tilt angle pane
  private JPanel pnlLocalTiltAngleSolution = new JPanel();
  private JCheckBox cbLocalTiltAngle = new JCheckBox("Enable");

  private LabeledTextField ltfLocalTiltAngleGroupSize = new LabeledTextField(
      "Group size: ");
  private LabeledTextField ltfLocalTiltAngleNonDefaultGroups = new LabeledTextField(
      "Non-default grouping: ");

  // Local magnfication pane
  private JPanel pnlLocalMagnificationSolution = new JPanel();
  private JCheckBox cbLocalMagnification = new JCheckBox("Enable");

  private LabeledTextField ltfLocalMagnificationGroupSize = new LabeledTextField(
      "Group size: ");
  private LabeledTextField ltfLocalMagnificationNonDefaultGroups = new LabeledTextField(
      "Non-default grouping: ");

  //  Local distortion pane
  private JPanel pnlLocalDistortionSolution = new JPanel();
  private JRadioButton rbLocalDistortionDisabled = new JRadioButton("Disabled");
  private JRadioButton rbLocalDistortionFullSolution = new JRadioButton(
      "Full solution");
  private JRadioButton rbLocalDistortionSkew = new JRadioButton("Skew only");
  private ButtonGroup bgLocalDistortionSolution = new ButtonGroup();

  private LabeledTextField ltfLocalXstretchGroupSize = new LabeledTextField(
      "X stretch group size: ");
  private LabeledTextField ltfLocalXstretchNonDefaultGroups = new LabeledTextField(
      "X stretch non-default grouping: ");

  private LabeledTextField ltfLocalSkewGroupSize = new LabeledTextField(
      "Skew group size: ");
  private LabeledTextField ltfLocalSkewNonDefaultGroups = new LabeledTextField(
      "Skew non-default grouping: ");

  //  Rotation pane
  private JRadioButton rbRotationNone = new JRadioButton("No rotation");
  private JRadioButton rbRotationAll = new JRadioButton(
      "Solve for all rotations");
  private JRadioButton rbRotationAutomap = new JRadioButton("Group rotations");
  private ButtonGroup bgRotationSolution = new ButtonGroup();
  private JPanel pnlRotationSolution = new JPanel();
  private LabeledTextField ltfRotationGroupSize = new LabeledTextField(
      "Group size: ");
  private LabeledTextField ltfRotationNonDefaultGroups = new LabeledTextField(
      "Non-default grouping: ");
  private JCheckBox cbProjectionStretch = new JCheckBox(
      "Solve for single stretch during projection");

  private FidXyz fidXyz = null;
  private MRCHeader rawstackHeader = null;
  private MRCHeader prealiHeader = null;

  /**
   * Constructor
   * @param axis
   */
  public TiltalignPanel(AxisID axis, FidXyz fidXyz, MRCHeader prealiHeader,
      MRCHeader rawstackHeader) {
    axisID = axis;
    this.fidXyz = fidXyz;
    this.prealiHeader = prealiHeader;
    this.rawstackHeader = rawstackHeader;

    tabPane.setBorder(new EtchedBorder("Tiltalign Parameters").getBorder());
    //  Create the tabs
    createGeneralTab();
    createGlobalSolutionTab();
    createLocalSolutionTab();
    setToolTipText();
  }

  /**
   * Set the values of the panel using a constant tiltalign parameter
   * object
   */
  public void setParameters(ConstTiltalignParam params) {
    try {
      fidXyz.read();
      rawstackHeader.read();
      prealiHeader.read();
    }
    catch (IOException except) {
      except.printStackTrace();
      return;
    }
    catch (InvalidParameterException except) {
      except.printStackTrace();
      return;
    }
    //  General panel parameters

    if (params.getSurfacesToAnalyze().getInteger() == 2) {
      rbDualFiducialSurfaces.setSelected(true);
    }
    else {
      rbSingleFiducialSurface.setSelected(true);
    }

    ltfResidualThreshold.setText(Math.abs(params.getResidualReportCriterion()
        .getDouble()));
    if (params.getResidualReportCriterion().getDouble() < 0) {
      rbResidNeighboring.setSelected(true);
    }
    else {
      rbResidAllViews.setSelected(true);
    }

    if (params.isExcludeListAvailable()) {
      ltfExcludeList.setEnabled(true);
      ltfExcludeList.setText(params.getExcludeList());
    }
    else {
      ltfExcludeList.setEnabled(false);
    }

    ltfSeparateViewGroups.setText(params.getSeparateGroup());
    ltfTiltAngleOffset.setText(params.getAngleOffset().toString());

    if (fidXyz.exists() && fidXyz.isEmpty()) {
      //if file exists but is empty, assume that tilt align failed and use
      //pre align instead
      //multiply by the binning previously used by pre align
      ltfTiltAxisZShift.setText(params.getAxisZShift().getDouble()
          * Math.round(prealiHeader.getXPixelSpacing()
              / rawstackHeader.getXPixelSpacing()));
    }
    else if (!fidXyz.exists() || !fidXyz.isPixelSizeSet()) {
      //if fidXyz.pixelSize could not be read from fid.xyz, then binning must
      //have been 1 the last time align.com was run.
      ltfTiltAxisZShift.setText(params.getAxisZShift().toString());
    }
    else {
      //multiply by the binning previously used by align.com
      ltfTiltAxisZShift.setText(params.getAxisZShift().getDouble()
          * Math.round(fidXyz.getPixelSize()
              / rawstackHeader.getXPixelSpacing()));
    }

    ltfMetroFactor.setText(params.getMetroFactor().toString());
    ltfCycleLimit.setText(params.getMaximumCycles().toString());

    cbLocalAlignments.setSelected(params.getLocalAlignments().is());
    ltfNLocalPatches.setText(params.getNumberOfLocalPatchesXandY());
    ltfMinLocalPatchSize.setText(params.getMinSizeOrOverlapXandY());
    ltfMinLocalFiducials.setText(params.getMinFidsTotalAndEachSurface());

    //  Tilt angle solution parameters
    int solutionType = params.getTiltOption().getInteger();
    if (solutionType == 0) {
      rbTiltAngleFixed.setSelected(true);
    }
    if (solutionType == 2) {
      rbTiltAngleAll.setSelected(true);
    }
    if (solutionType == 5) {
      rbTiltAngleAutomap.setSelected(true);
    }
    ltfTiltAngleGroupSize.setText(params.getTiltDefaultGrouping().toString());
    ltfTiltAngleNonDefaultGroups.setText(params.getTiltNondefaultGroup());

    //  Magnification solution parameters
    //  TODO what to do if the magnification type is not one of the cases
    //  below
    ltfMagnificationReferenceView.setText(params.getMagReferenceView()
        .toString());
    solutionType = params.getMagOption().getInteger();
    if (solutionType == 0) {
      rbMagnificationFixed.setSelected(true);
    }
    if (solutionType == 1) {
      rbMagnificationAll.setSelected(true);
    }
    if (solutionType == TiltalignParam.AUTOMAPPED_OPTION) {
      rbMagnificationAutomap.setSelected(true);
    }
    ltfMagnificationGroupSize
        .setText(params.getMagDefaultGrouping().toString());
    ltfMagnificationNonDefaultGroups.setText(params.getMagNondefaultGroup());

    //  Rotation solution parameters
    solutionType = params.getMagOption().getInteger();
    if (solutionType == 0) {
      rbRotationNone.setSelected(true);
    }
    if (solutionType == 1) {
      rbRotationAll.setSelected(true);
    }
    if (solutionType == TiltalignParam.AUTOMAPPED_OPTION) {
      rbRotationAutomap.setSelected(true);
    }
    ltfRotationGroupSize.setText(params.getRotDefaultGrouping().toString());
    ltfRotationNonDefaultGroups.setText(params.getRotNondefaultGroup());

    //  Compression solution parameters
    /*
     ltfCompressionReferenceView.setText(
     params.getCompressionSolutionReferenceView());
     solutionType = params.getCompressionSolutionType();
     if (solutionType == 1) {
     rbCompressionAll.setSelected(true);
     }
     if (solutionType == 3) {
     rbCompressionAutomapLinear.setSelected(true);
     }
     if (solutionType == 4) {
     rbCompressionAutomapFixed.setSelected(true);
     }
     
     if (solutionType > 2) {
     ltfCompressionGroupSize.setText(params.getCompressionSolutionGroupSize());
     ltfCompressionAdditionalGroups.setText(
     params.getCompressionSolutionAdditionalGroups());
     }
     */
    //  Global distortion solution type
    int xStretchSolutionType = params.getXStretchOption().getInteger();
    int skewSolutionType = params.getSkewOption().getInteger();
    if (xStretchSolutionType == 0 && skewSolutionType == 0) {
      rbDistortionDisabled.setSelected(true);
    }
    else if (xStretchSolutionType == 3 && skewSolutionType == 3) {
      rbDistortionFullSolution.setSelected(true);
    }
    else {
      this.rbDistortionSkew.setSelected(true);
    }
    ltfXstretchGroupSize
        .setText(params.getXStretchDefaultGrouping().toString());
    ltfXstretchNonDefaultGroups.setText(params.getXStretchNondefaultGroup());
    //   skew solution parameters
    ltfSkewGroupSize.setText(params.getSkewDefaultGrouping().toString());
    ltfSkewNonDefaultGroups.setText(params.getSkewNondefaultGroup());

    cbProjectionStretch.setSelected(params.getProjectionStretch().is());

    // Local rotation solution parameters
    // NOTE this is brittle since we are mapping a numeric value to a boolean
    // at David's request
    solutionType = params.getLocalRotOption().getInteger();
    if (solutionType == 0) {
      cbLocalRotation.setSelected(false);
    }
    else {
      cbLocalRotation.setSelected(true);
    }
    ltfLocalRotationGroupSize.setText(params.getLocalRotDefaultGrouping()
        .toString());
    ltfLocalRotationNonDefaultGroups.setText(params
        .getLocalRotNondefaultGroup());

    // Local tilt angle solution parameters
    solutionType = params.getLocalTiltOption().getInteger();
    if (solutionType == 0) {
      cbLocalTiltAngle.setSelected(false);
    }
    else {
      cbLocalTiltAngle.setSelected(true);
    }
    ltfLocalTiltAngleGroupSize.setText(params.getLocalTiltDefaultGrouping()
        .toString());
    ltfLocalTiltAngleNonDefaultGroups.setText(params
        .getLocalTiltNondefaultGroup());

    //  Local magnification solution parameters
    solutionType = params.getLocalMagOption().getInteger();
    if (solutionType == 0) {
      cbLocalMagnification.setSelected(false);
    }
    else {
      cbLocalMagnification.setSelected(true);
    }
    ltfLocalMagnificationGroupSize.setText(params.getLocalMagDefaultGrouping()
        .toString());
    ltfLocalMagnificationNonDefaultGroups.setText(params
        .getLocalMagNondefaultGroup());

    //  Local distortion solution type
    xStretchSolutionType = params.getLocalXStretchOption().getInteger();
    skewSolutionType = params.getLocalSkewOption().getInteger();
    if (xStretchSolutionType == 0 && skewSolutionType == 0) {
      rbLocalDistortionDisabled.setSelected(true);
    }
    else if (xStretchSolutionType == 3 && skewSolutionType == 3) {
      rbLocalDistortionFullSolution.setSelected(true);
    }
    else {
      rbLocalDistortionSkew.setSelected(true);
    }
    ltfLocalXstretchGroupSize.setText(params.getLocalXStretchDefaultGrouping()
        .toString());
    ltfLocalXstretchNonDefaultGroups.setText(params
        .getLocalXStretchNondefaultGroup());
    //  Local skew solution parameters
    ltfLocalSkewGroupSize.setText(params.getLocalSkewDefaultGrouping()
        .toString());
    ltfLocalSkewNonDefaultGroups.setText(params.getLocalSkewNondefaultGroup());

    //  Set the UI to match the data
    enableFields();
  }

  /**
   * Get the values from the panel by updating tiltalign parameter
   * object.  Currently this makes the assumption that the argument
   * contains valid parameters and that only the known parameters will
   * be changed.
   */
  public void getParameters(TiltalignParam params)
      throws FortranInputSyntaxException {
    try {
      prealiHeader.read();
      //raw stack won't change and doesn't really have to be read again
      rawstackHeader.read();
    }
    catch (IOException except) {
      except.printStackTrace();
      return;
    }
    catch (InvalidParameterException except) {
      except.printStackTrace();
      return;
    }
    String badParameter = "";
    try {
      if (rbDualFiducialSurfaces.isSelected()) {
        params.setSurfacesToAnalyze(2);
      }
      else {
        params.setSurfacesToAnalyze(1);
      }

      badParameter = ltfResidualThreshold.getLabel();
      double resid = Double.parseDouble(ltfResidualThreshold.getText());
      if (rbResidNeighboring.isSelected()) {
        resid *= -1;
      }
      params.setResidualReportCriterion(resid);

      //  Currently only supports Exclude list or blank entries
      badParameter = ltfExcludeList.getLabel();
      if (ltfExcludeList.isEnabled()) {
        params.setExcludeList(ltfExcludeList.getText());
      }
      badParameter = ltfSeparateViewGroups.getLabel();
      params.setSeparateGroup(ltfSeparateViewGroups.getText());

      badParameter = ltfTiltAngleOffset.getLabel();
      params.setAngleOffset(ltfTiltAngleOffset.getText());

      badParameter = ltfTiltAxisZShift.getLabel();

      //divide by the binning used to create the .preali file
      params.setAxisZShift(Double.parseDouble(ltfTiltAxisZShift.getText())
          / Math.round(prealiHeader.getXPixelSpacing()
              / rawstackHeader.getXPixelSpacing()));

      badParameter = ltfMetroFactor.getLabel();
      params.setMetroFactor(ltfMetroFactor.getText());

      badParameter = ltfCycleLimit.getLabel();
      params.setMaximumCycles(ltfCycleLimit.getText());

      badParameter = cbLocalAlignments.getText();
      params.setLocalAlignments(cbLocalAlignments.isSelected());

      badParameter = ltfNLocalPatches.getLabel();
      params.setNumberOfLocalPatchesXandY(ltfNLocalPatches.getText());

      badParameter = ltfMinLocalPatchSize.getLabel();
      params.setMinSizeOrOverlapXandY(ltfMinLocalPatchSize.getText());

      badParameter = ltfMinLocalFiducials.getLabel();
      params.setMinFidsTotalAndEachSurface(ltfMinLocalFiducials.getText());

      // Tilt angle pane
      int type = 0;
      if (rbTiltAngleFixed.isSelected())
        type = 0;
      if (rbTiltAngleAll.isSelected())
        type = 2;
      if (rbTiltAngleAutomap.isSelected())
        type = 5;
      params.setTiltOption(type);
      badParameter = ltfTiltAngleGroupSize.getLabel();
      params.setTiltDefaultGrouping(ltfTiltAngleGroupSize.getText());

      badParameter = ltfTiltAngleNonDefaultGroups.getLabel();
      params.setTiltNondefaultGroup(ltfTiltAngleNonDefaultGroups.getText());

      // Magnification pane
      badParameter = ltfMagnificationReferenceView.getLabel();
      params.setMagReferenceView(ltfMagnificationReferenceView.getText());

      if (rbMagnificationFixed.isSelected())
        type = 0;
      if (rbMagnificationAll.isSelected())
        type = 1;
      if (rbMagnificationAutomap.isSelected())
        type = TiltalignParam.AUTOMAPPED_OPTION;
      params.setMagOption(type);

      badParameter = ltfMagnificationGroupSize.getLabel();
      params.setMagDefaultGrouping(ltfMagnificationGroupSize.getText());

      badParameter = ltfMagnificationNonDefaultGroups.getLabel();
      params.setMagNondefaultGroup(ltfMagnificationNonDefaultGroups.getText());

      // Rotation pane
      if (rbRotationNone.isSelected())
        type = 0;
      if (rbRotationAll.isSelected())
        type = 1;
      if (rbRotationAutomap.isSelected())
        type = TiltalignParam.AUTOMAPPED_OPTION;
      params.setRotOption(type);
      badParameter = ltfRotationGroupSize.getLabel();
      params.setRotDefaultGrouping(ltfRotationGroupSize.getText());
      badParameter = ltfRotationNonDefaultGroups.getLabel();
      params.setRotNondefaultGroup(ltfRotationNonDefaultGroups.getText());

      // Compression pane
      /*      badParameter = ltfCompressionReferenceView.getLabel();
       params.setCompressionReferenceView(ltfCompressionReferenceView.getText());
       
       if (rbCompressionAll.isSelected())
       type = 1;
       if (rbCompressionAutomapLinear.isSelected())
       type = 3;
       if (rbCompressionAutomapFixed.isSelected())
       type = 4;
       params.setCompressionType(type);
       
       if (type > 2) {
       badParameter = ltfCompressionGroupSize.getLabel();
       params.setCompressionSolutionGroupSize(
       ltfCompressionGroupSize.getText());
       
       badParameter = ltfCompressionAdditionalGroups.getLabel();
       params.setCompressionSolutionAdditionalGroups(
       ltfCompressionAdditionalGroups.getText());
       }
       */

      // Distortion pane
      type = 0;
      //  Set the necessary types for distortion xstretch and skew
      if (rbDistortionDisabled.isSelected()) {
        params.setSkewOption(TiltalignParam.FIXED_OPTION);
        params.setXStretchOption(TiltalignParam.FIXED_OPTION);
      }
      else {
        params.setSkewOption(TiltalignParam.AUTOMAPPED_OPTION);
        if (rbDistortionFullSolution.isSelected()) {
          params.setXStretchOption(TiltalignParam.AUTOMAPPED_OPTION);
        }
        else {
          params.setXStretchOption(TiltalignParam.FIXED_OPTION);
        }
      }

      badParameter = ltfSkewGroupSize.getLabel();
      params.setSkewDefaultGrouping(ltfSkewGroupSize.getText());

      badParameter = ltfSkewNonDefaultGroups.getLabel();
      params.setSkewNondefaultGroup(ltfSkewNonDefaultGroups.getText());

      badParameter = ltfXstretchGroupSize.getLabel();
      params.setXStretchDefaultGrouping(ltfXstretchGroupSize.getText());

      badParameter = ltfXstretchNonDefaultGroups.getLabel();
      params.setXStretchNondefaultGroup(ltfXstretchNonDefaultGroups.getText());

      badParameter = cbProjectionStretch.getText();
      params.setProjectionStretch(cbProjectionStretch.isSelected());

      //  Get the local alignment parameters
      // Rotation pane
      // NOTE this only works if 0 and 5 are valid local tilt angle codes
      type = 0;
      if (cbLocalRotation.isSelected())
        type = params.getLocalRotOption().getDisplayInteger();
      params.setLocalRotOption(type);
      badParameter = ltfLocalRotationGroupSize.getLabel();
      params.setLocalRotDefaultGrouping(ltfLocalRotationGroupSize.getText());

      badParameter = ltfLocalRotationNonDefaultGroups.getLabel();
      params.setLocalRotNondefaultGroup(ltfLocalRotationNonDefaultGroups
          .getText());

      // Tilt angle pane
      type = 0;
      if (cbLocalTiltAngle.isSelected())
        type = params.getLocalTiltOption().getDisplayInteger();
      params.setLocalTiltOption(type);
      badParameter = ltfLocalTiltAngleGroupSize.getLabel();
      params.setLocalTiltDefaultGrouping(ltfLocalTiltAngleGroupSize.getText());

      badParameter = ltfLocalTiltAngleNonDefaultGroups.getLabel();
      params.setLocalTiltNondefaultGroup(ltfLocalTiltAngleNonDefaultGroups
          .getText());

      // Local magnification pane
      if (cbLocalMagnification.isSelected()) {
        params
            .setLocalMagOption(params.getLocalMagOption().getDisplayInteger());
      }
      else {
        params.setLocalMagOption(0);
      }

      badParameter = ltfLocalMagnificationGroupSize.getLabel();
      params.setLocalMagDefaultGrouping(ltfLocalMagnificationGroupSize
          .getText());

      badParameter = ltfLocalMagnificationNonDefaultGroups.getLabel();
      params.setLocalMagNondefaultGroup(ltfLocalMagnificationNonDefaultGroups
          .getText());

      // Distortion pane
      type = 0;
      if (rbLocalDistortionDisabled.isSelected()) {
        params.setLocalSkewOption(TiltalignParam.FIXED_OPTION);
        params.setLocalXStretchOption(TiltalignParam.FIXED_OPTION);
      }
      else {
        params.setLocalSkewOption(TiltalignParam.AUTOMAPPED_OPTION);
        if (rbLocalDistortionFullSolution.isSelected()) {
          params.setLocalXStretchOption(TiltalignParam.AUTOMAPPED_OPTION);
        }
        else {
          params.setLocalXStretchOption(TiltalignParam.FIXED_OPTION);
        }
      }
      badParameter = ltfLocalSkewGroupSize.getLabel();
      params.setLocalSkewDefaultGrouping(ltfLocalSkewGroupSize.getText());

      badParameter = ltfLocalSkewNonDefaultGroups.getLabel();
      params
          .setLocalSkewNondefaultGroup(ltfLocalSkewNonDefaultGroups.getText());

      badParameter = ltfLocalXstretchGroupSize.getLabel();
      params.setLocalXStretchDefaultGrouping(ltfLocalXstretchGroupSize
          .getText());

      badParameter = ltfLocalXstretchNonDefaultGroups.getLabel();
      params.setLocalXStretchNondefaultGroup(ltfLocalXstretchNonDefaultGroups
          .getText());
      //params needs to have other values set before it can set OutputZFactorFile
      params.setOutputZFactorFile();
    }
    catch (FortranInputSyntaxException except) {
      String message = badParameter + " " + except.getMessage();
      throw new FortranInputSyntaxException(message);
    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
  }

  /**
   * Make the panel visible
   * @param state
   */
  void setVisible(boolean state) {
    pnlGeneral.setVisible(state);
  }

  /**
   *
   */
  void setLargestTab() {
    //tabPane.setSelectedComponent(pnlGlobalVariable);
    tabPane.setSelectedComponent(pnlLocalSolution);
  }

  void setFirstTab() {
    tabPane.setSelectedComponent(pnlGeneral);
  }

  void setAdvanced(boolean state) {

    //    ltfMetroFactor.setVisible(state);
    //    ltfCycleLimit.setVisible(state);
    pnlMinimizationParams.setVisible(state);
    ltfMagnificationReferenceView.setVisible(state);
    ltfRotationNonDefaultGroups.setVisible(state);
    ltfTiltAngleNonDefaultGroups.setVisible(state);
    ltfMagnificationNonDefaultGroups.setVisible(state);
    ltfXstretchNonDefaultGroups.setVisible(state);
    ltfSkewNonDefaultGroups.setVisible(state);
    cbProjectionStretch.setVisible(state);
    ltfLocalRotationNonDefaultGroups.setVisible(state);
    ltfLocalTiltAngleNonDefaultGroups.setVisible(state);
    ltfLocalMagnificationNonDefaultGroups.setVisible(state);
    ltfLocalXstretchNonDefaultGroups.setVisible(state);
    ltfLocalSkewNonDefaultGroups.setVisible(state);
    ltfMinLocalPatchSize.setVisible(state);
  }

  //  Local alignment state
  void enableLocalAlignmentFields() {
    boolean state = cbLocalAlignments.isSelected();
    ltfNLocalPatches.setEnabled(state);
    ltfMinLocalPatchSize.setEnabled(state);
    ltfMinLocalFiducials.setEnabled(state);
    tabPane.setEnabledAt(tabPane.indexOfComponent(pnlLocalSolution), state);
  }

  /**
   * Signal each pane to update its enabled/disabled state.
   */
  public void enableFields() {
    //  update all of the enable/disable states
    enableLocalAlignmentFields();

    enableRotationSolutionFields();
    enableTiltAngleSolutionFields();
    enableMagnificationSolutionFields();
    enableDistortionSolutionFields();

    enableLocalRotationSolutionFields();
    enableLocalTiltAngleSolutionFields();
    enableLocalMagnificationSolutionFields();
    enableLocalDistortionSolutionFields();
  }

  /**
   * Update the enabled/disabled state of the specified solution panel.
   */
  void enableTiltAngleSolutionFields() {
    boolean state = rbTiltAngleAutomap.isSelected();
    ltfTiltAngleGroupSize.setEnabled(state);
    ltfTiltAngleNonDefaultGroups.setEnabled(state);
  }

  void enableMagnificationSolutionFields() {
    boolean state = rbMagnificationAutomap.isSelected();
    ltfMagnificationGroupSize.setEnabled(state);
    ltfMagnificationNonDefaultGroups.setEnabled(state);
  }

  void enableRotationSolutionFields() {
    boolean state = rbRotationAutomap.isSelected();
    ltfRotationGroupSize.setEnabled(state);
    ltfRotationNonDefaultGroups.setEnabled(state);
  }

  /*
   void updateCompressionSolutionPanel() {
   boolean state =
   rbCompressionAutomapLinear.isSelected()
   || rbCompressionAutomapFixed.isSelected();
   ltfCompressionGroupSize.setEnabled(state);
   ltfCompressionAdditionalGroups.setEnabled(state);
   }
   */

  void setDistortionSolutionState() {
    if (rbDistortionDisabled.isSelected()) {
      rbLocalDistortionDisabled.setSelected(true);
    }
    else {
      rbTiltAngleAutomap.setSelected(true);
      enableTiltAngleSolutionFields();
      if (rbDistortionFullSolution.isSelected()) {
        rbLocalDistortionFullSolution.setSelected(true);
      }
      else if (rbDistortionSkew.isSelected()) {
        rbLocalDistortionSkew.setSelected(true);
      }
    }
    enableLocalDistortionSolutionFields();
    enableDistortionSolutionFields();
  }

  void enableDistortionSolutionFields() {
    boolean xStretchState = rbDistortionFullSolution.isSelected();
    ltfXstretchGroupSize.setEnabled(xStretchState);
    ltfXstretchNonDefaultGroups.setEnabled(xStretchState);
    boolean skewState = rbDistortionFullSolution.isSelected()
        || rbDistortionSkew.isSelected();
    ltfSkewGroupSize.setEnabled(skewState);
    ltfSkewNonDefaultGroups.setEnabled(skewState);
  }

  void enableLocalRotationSolutionFields() {
    boolean state = cbLocalRotation.isSelected();
    ltfLocalRotationGroupSize.setEnabled(state);
    ltfLocalRotationNonDefaultGroups.setEnabled(state);
  }

  void enableLocalTiltAngleSolutionFields() {
    boolean state = cbLocalTiltAngle.isSelected();
    ltfLocalTiltAngleGroupSize.setEnabled(state);
    ltfLocalTiltAngleNonDefaultGroups.setEnabled(state);
  }

  void enableLocalMagnificationSolutionFields() {
    boolean state = cbLocalMagnification.isSelected();
    ltfLocalMagnificationGroupSize.setEnabled(state);
    ltfLocalMagnificationNonDefaultGroups.setEnabled(state);
  }

  void enableLocalDistortionSolutionFields() {
    boolean xStretchState = rbLocalDistortionFullSolution.isSelected();
    ltfLocalXstretchGroupSize.setEnabled(xStretchState);
    ltfLocalXstretchNonDefaultGroups.setEnabled(xStretchState);
    boolean skewState = rbLocalDistortionSkew.isSelected()
        || rbLocalDistortionFullSolution.isSelected();
    ltfLocalSkewGroupSize.setEnabled(skewState);
    ltfLocalSkewNonDefaultGroups.setEnabled(skewState);
  }

  Container getContainer() {
    return tabPane;
  }

  /**
   * 
   * @param panel
   * @param group
   * @param items
   * @param listener
   */
  private void createRadioBox(JPanel panel, ButtonGroup group,
      JRadioButton[] items, ActionListener listener) {
    createRadioBox(panel, group, items, listener, 245);
  }

  /**
   * 
   * @param panel
   * @param group
   * @param items
   * @param listener
   * @param width
   */
  private void createRadioBox(JPanel panel, ButtonGroup group,
      JRadioButton[] items, ActionListener listener, int width) {
    int radioButtonHeight = 18;
    Dimension radioButtonItemSize = new Dimension(width, radioButtonHeight);

    // Add the items to the group and to the panel
    for (int i = 0; i < items.length; i++) {
      group.add(items[i]);
      panel.add(items[i]);
      items[i].addActionListener(listener);
      items[i].setPreferredSize(radioButtonItemSize);
    }
  }

  /**
   * Layout the general parameters tab
   */
  private void createGeneralTab() {

    pnlGeneral.setLayout(new BoxLayout(pnlGeneral, BoxLayout.Y_AXIS));
    pnlGeneral.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlGeneral.add(ltfExcludeList.getContainer());
    pnlGeneral.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlGeneral.add(ltfSeparateViewGroups.getContainer());
    pnlGeneral.add(Box.createRigidArea(FixedDim.x0_y10));

    //Residual reporting
    pnlResidualThreshold.setLayout(new BoxLayout(pnlResidualThreshold,
        BoxLayout.Y_AXIS));
    pnlResidualThreshold.setBorder(new EtchedBorder("Residual Reporting")
        .getBorder());
    //top panel
    SpacedPanel topResidualPanel = new SpacedPanel(FixedDim.x5_y0);
    topResidualPanel.setLayout(new BoxLayout(topResidualPanel.getContainer(),
        BoxLayout.X_AXIS));
    ltfResidualThreshold.setColumns(10);
    topResidualPanel.add(ltfResidualThreshold);
    topResidualPanel.add(new JLabel("s.d."));
    pnlResidualThreshold.add(topResidualPanel.getContainer());
    //bottom panel
    SpacedPanel bottomResidualPanel = new SpacedPanel(FixedDim.x10_y0);
    bottomResidualPanel.setLayout(new BoxLayout(bottomResidualPanel
        .getContainer(), BoxLayout.X_AXIS));
    bottomResidualPanel.setComponentAlignmentX(Container.RIGHT_ALIGNMENT);
    bottomResidualPanel.add(new JLabel("Relative to"));
    //create radio button group
    JRadioButton[] items = new JRadioButton[2];
    items[0] = rbResidAllViews;
    items[1] = rbResidNeighboring;
    JPanel pnlRBResidual = new JPanel();
    pnlRBResidual.setLayout(new BoxLayout(pnlRBResidual, BoxLayout.Y_AXIS));
    ResidualRadioListener residualRadioListener = new ResidualRadioListener(
        this);
    createRadioBox(pnlRBResidual, bgResidualThreshold, items,
        residualRadioListener, 300);
    bottomResidualPanel.add(pnlRBResidual);
    pnlResidualThreshold.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlResidualThreshold.add(bottomResidualPanel.getContainer());

    pnlGeneral.add(pnlResidualThreshold);
    pnlGeneral.add(Box.createRigidArea(FixedDim.x0_y10));

    pnlFiducialSurfaces.setLayout(new BoxLayout(pnlFiducialSurfaces,
        BoxLayout.X_AXIS));
    pnlFiducialSurfaces
        .setBorder(new EtchedBorder("Analysis of Surface Angles").getBorder());

    //  Need an extra panel to make border extend the appropriate width
    JPanel pnlRBFiducual = new JPanel();
    pnlRBFiducual.setLayout(new BoxLayout(pnlRBFiducual, BoxLayout.Y_AXIS));
    items = new JRadioButton[2];
    items[0] = rbSingleFiducialSurface;
    items[1] = rbDualFiducialSurfaces;
    FiducialRadioListener fiducialRadioListener = new FiducialRadioListener(
        this);
    createRadioBox(pnlRBFiducual, bgFiducialSurfaces, items,
        fiducialRadioListener, 300);

    pnlFiducialSurfaces.add(pnlRBFiducual);
    pnlFiducialSurfaces.add(Box.createHorizontalGlue());
    pnlGeneral.add(pnlFiducialSurfaces);
    pnlGeneral.add(Box.createRigidArea(FixedDim.x0_y10));

    pnlVolumeParameters.setLayout(new BoxLayout(pnlVolumeParameters,
        BoxLayout.Y_AXIS));
    pnlVolumeParameters
        .setBorder(new EtchedBorder("Volume Position Parameters").getBorder());
    pnlVolumeParameters.add(ltfTiltAngleOffset.getContainer());
    pnlVolumeParameters.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlVolumeParameters.add(ltfTiltAxisZShift.getContainer());
    pnlVolumeParameters.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlGeneral.add(pnlVolumeParameters);
    pnlGeneral.add(Box.createRigidArea(FixedDim.x0_y10));

    pnlMinimizationParams.setLayout(new BoxLayout(pnlMinimizationParams,
        BoxLayout.Y_AXIS));
    pnlMinimizationParams.setBorder(new EtchedBorder("Minimization Parameters")
        .getBorder());
    pnlMinimizationParams.add(ltfMetroFactor.getContainer());
    pnlMinimizationParams.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMinimizationParams.add(ltfCycleLimit.getContainer());
    pnlMinimizationParams.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlGeneral.add(pnlMinimizationParams);
    pnlGeneral.add(Box.createRigidArea(FixedDim.x0_y10));

    //local alignment
    ltfMinLocalFiducials.setTextPreferredWidth(FixedDim.integerPairWidth);
    createVariablePanel(pnlLocalParameters, cbLocalAlignments,
        ltfNLocalPatches, ltfMinLocalPatchSize, ltfMinLocalFiducials,
        "Local Alignment Parameters");
    LocalAlignmentsListener localAlignmentsListener = new LocalAlignmentsListener(
        this);
    cbLocalAlignments.addActionListener(localAlignmentsListener);

    pnlGeneral.add(pnlLocalParameters);
    pnlGeneral.add(Box.createVerticalGlue());

    tabPane.addTab("General", pnlGeneral);
  }

  /**
   * Layout the global estimate tab
   */
  private void createGlobalSolutionTab() {
    pnlGlobalVariable.setLayout(new BoxLayout(pnlGlobalVariable,
        BoxLayout.Y_AXIS));

    //  Layout the global rotation variable parameters
    JPanel pnlRBRotation = new JPanel();
    pnlRBRotation.setLayout(new BoxLayout(pnlRBRotation, BoxLayout.Y_AXIS));
    JRadioButton[] items = new JRadioButton[3];
    items[0] = rbRotationNone;
    items[1] = rbRotationAll;
    items[2] = rbRotationAutomap;
    RotationRadioListener rotationRadioListener = new RotationRadioListener(
        this);
    createRadioBox(pnlRBRotation, bgRotationSolution, items,
        rotationRadioListener);
    createVariablePanel(pnlRotationSolution, pnlRBRotation,
        ltfRotationGroupSize, ltfRotationNonDefaultGroups,
        "Rotation Solution Type");

    //  Layout the global tilt angle estimate pane
    JPanel pnlRBTiltAngle = new JPanel();
    pnlRBTiltAngle.setLayout(new BoxLayout(pnlRBTiltAngle, BoxLayout.Y_AXIS));
    items = new JRadioButton[3];
    items[0] = rbTiltAngleFixed;
    items[1] = rbTiltAngleAll;
    items[2] = rbTiltAngleAutomap;
    TiltAngleRadioListener tiltAngleRadioListener = new TiltAngleRadioListener(
        this);
    createRadioBox(pnlRBTiltAngle, bgTiltAngleSolution, items,
        tiltAngleRadioListener);
    createVariablePanel(pnlTiltAngleSolution, pnlRBTiltAngle,
        ltfTiltAngleGroupSize, ltfTiltAngleNonDefaultGroups,
        "Tilt Angle Solution Type");

    //  Layout the global magnification variable parameters
    JPanel pnlRBMagnification = new JPanel();
    pnlRBMagnification.setLayout(new BoxLayout(pnlRBMagnification,
        BoxLayout.Y_AXIS));
    items = new JRadioButton[3];
    items[0] = rbMagnificationFixed;
    items[1] = rbMagnificationAll;
    items[2] = rbMagnificationAutomap;
    MagnificationRadioListener magnificationRadioListener = new MagnificationRadioListener(
        this);
    createRadioBox(pnlRBMagnification, bgMagnificationSolution, items,
        magnificationRadioListener);
    createVariablePanel(pnlMagnificationSolution, pnlRBMagnification,
        ltfMagnificationReferenceView, ltfMagnificationGroupSize,
        ltfMagnificationNonDefaultGroups, null, "Magnification Solution Type");

    // Layout the global distortion pane

    //Create radio box
    items = new JRadioButton[3];
    items[0] = rbDistortionDisabled;
    items[1] = rbDistortionFullSolution;
    items[2] = rbDistortionSkew;
    JPanel pnlRBDistortion = new JPanel();
    pnlRBDistortion.setLayout(new BoxLayout(pnlRBDistortion, BoxLayout.Y_AXIS));

    DistortionRadioListener distortionRadioListener = new DistortionRadioListener(
        this);
    createRadioBox(pnlRBDistortion, bgDistortionSolution, items,
        distortionRadioListener);
    ltfXstretchNonDefaultGroups
        .setTextPreferredWidth(FixedDim.integerTripletWidth);
    createVariablePanel(pnlDistortionSolution, pnlRBDistortion,
        ltfXstretchGroupSize, ltfXstretchNonDefaultGroups, ltfSkewGroupSize,
        ltfSkewNonDefaultGroups, "Distortion Solution Type");

    //  Add the individual panes to the tab
    pnlGlobalVariable.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlGlobalVariable.add(pnlRotationSolution);
    pnlGlobalVariable.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlGlobalVariable.add(pnlTiltAngleSolution);
    pnlGlobalVariable.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlGlobalVariable.add(Box.createVerticalGlue());
    pnlGlobalVariable.add(pnlMagnificationSolution);
    pnlGlobalVariable.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlGlobalVariable.add(Box.createVerticalGlue());
    pnlGlobalVariable.add(pnlDistortionSolution);
    pnlGlobalVariable.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlGlobalVariable.add(Box.createVerticalGlue());
    cbProjectionStretch.setAlignmentX(Component.RIGHT_ALIGNMENT);
    pnlGlobalVariable.add(cbProjectionStretch);

    tabPane.addTab("Global Variables", pnlGlobalVariable);

  }

  /*  private void createCompressionTab() {
   //  Compression solution
   //      ltfCompressionReferenceView.setMaximumSize(dimLTF);
   pnlCompressionSolution.add(ltfCompressionReferenceView.getContainer());
   JRadioButton[] items = new JRadioButton[3];
   items[0] = rbCompressionAll;
   items[1] = rbCompressionAutomapLinear;
   items[2] = rbCompressionAutomapFixed;
   CompressionRadioListener compressionRadioListener =
   new CompressionRadioListener(this);
   createRadioBox(
   pnlCompressionSolution,
   bgCompressionSolution,
   items,
   "Compression solution type",
   compressionRadioListener);
   
   pnlCompressionSolution.add(ltfCompressionGroupSize.getContainer());
   pnlCompressionSolution.add(ltfCompressionAdditionalGroups.getContainer());
   pnlCompressionSolution.add(Box.createRigidArea(FixedDim.x0_y5));
   pnlCompressionSolution.add(Box.createVerticalGlue());
   
   }
   */

  private void createLocalSolutionTab() {
    //  Construct the local solution panel
    pnlLocalSolution
        .setLayout(new BoxLayout(pnlLocalSolution, BoxLayout.Y_AXIS));
    //pnlLocalSolution.setPreferredSize(new Dimension(400, 350));

    //  Construct the rotation solution objects
    createVariablePanel(pnlLocalRotationSolution, cbLocalRotation,
        ltfLocalRotationGroupSize, ltfLocalRotationNonDefaultGroups,
        "Local Rotation Solution Type");
    LocalRotationCheckListener localRotationCheckListener = new LocalRotationCheckListener(
        this);
    cbLocalRotation.addActionListener(localRotationCheckListener);

    //  Construct the tilt angle solution objects
    createVariablePanel(pnlLocalTiltAngleSolution, cbLocalTiltAngle,
        ltfLocalTiltAngleGroupSize, ltfLocalTiltAngleNonDefaultGroups,
        "Local Tilt Angle Solution Type");

    LocalTiltAngleCheckListener localTiltAngleCheckListener = new LocalTiltAngleCheckListener(
        this);
    cbLocalTiltAngle.addActionListener(localTiltAngleCheckListener);

    //  Construct the local magnification pane
    createVariablePanel(pnlLocalMagnificationSolution, cbLocalMagnification,
        ltfLocalMagnificationGroupSize, ltfLocalMagnificationNonDefaultGroups,
        "Local Magnification Solution Type");

    LocalMagnificationCheckListener localMagnificationCheckListener = new LocalMagnificationCheckListener(
        this);
    cbLocalMagnification.addActionListener(localMagnificationCheckListener);

    //  Construction the local distortion pane

    //Create radio box
    JRadioButton[] items = new JRadioButton[3];
    items[0] = rbLocalDistortionDisabled;
    items[1] = rbLocalDistortionFullSolution;
    items[2] = rbLocalDistortionSkew;
    JPanel pnlRBLocalDistortion = new JPanel();
    pnlRBLocalDistortion.setLayout(new BoxLayout(pnlRBLocalDistortion,
        BoxLayout.Y_AXIS));

    LocalDistortionRadioListener localDistortionRadioListener = new LocalDistortionRadioListener(
        this);
    createRadioBox(pnlRBLocalDistortion, bgLocalDistortionSolution, items,
        localDistortionRadioListener);
    ltfLocalXstretchNonDefaultGroups
        .setTextPreferredWidth(FixedDim.integerTripletWidth);

    createVariablePanel(pnlLocalDistortionSolution, pnlRBLocalDistortion,
        ltfLocalXstretchGroupSize, ltfLocalXstretchNonDefaultGroups,
        ltfLocalSkewGroupSize, ltfLocalSkewNonDefaultGroups,
        "Local Distortion Solution Type");

    pnlLocalSolution.add(Box.createVerticalGlue());
    pnlLocalSolution.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlLocalSolution.add(pnlLocalRotationSolution);

    pnlLocalSolution.add(Box.createVerticalGlue());
    pnlLocalSolution.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlLocalSolution.add(pnlLocalTiltAngleSolution);

    pnlLocalSolution.add(Box.createVerticalGlue());
    pnlLocalSolution.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlLocalSolution.add(pnlLocalMagnificationSolution);

    pnlLocalSolution.add(Box.createVerticalGlue());
    pnlLocalSolution.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlLocalSolution.add(pnlLocalDistortionSolution);

    tabPane.addTab("Local Variables", pnlLocalSolution);
  }

  private void createVariablePanel(JPanel panel, JCheckBox checkBox,
      LabeledTextField groupSize, LabeledTextField additionalGroups,
      String title) {
    createVariablePanel(panel, checkBox, groupSize, additionalGroups, null,
        title);
  }

  private void createVariablePanel(JPanel panel, JCheckBox checkBox,
      LabeledTextField field1, LabeledTextField field2,
      LabeledTextField field3, String title) {
    JPanel buttonPanel = new JPanel();
    buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
    buttonPanel.add(checkBox);
    createVariablePanel(panel, buttonPanel, field1, field2, field3, null, title);
  }

  /*
   * create a variable panel with an internal panel (can contain a radio button
   * group
   */
  private void createVariablePanel(JPanel panel, JPanel buttonPanel,
      LabeledTextField groupSize, LabeledTextField additionalGroups,
      String title) {
    createVariablePanel(panel, buttonPanel, groupSize, additionalGroups, null,
        null, title);
  }

  private void createVariablePanel(JPanel panel, JPanel buttonPanel,
      LabeledTextField field1, LabeledTextField field2,
      LabeledTextField field3, LabeledTextField field4, String title) {
    panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
    panel.add(Box.createRigidArea(FixedDim.x5_y0));
    panel.add(buttonPanel);
    panel.add(Box.createRigidArea(FixedDim.x40_y0));
    SpacedPanel fieldPanel = new SpacedPanel(FixedDim.x0_y5);
    fieldPanel.setLayout(new BoxLayout(fieldPanel.getContainer(),
        BoxLayout.Y_AXIS));
    fieldPanel.add(field1);
    if (field2 != null) {
      fieldPanel.add(field2);
    }
    if (field3 != null) {
      fieldPanel.add(field3);
    }
    if (field4 != null) {
      fieldPanel.add(field4);
    }
    panel.add(fieldPanel.getContainer());
    panel.setBorder(new EtchedBorder(title).getBorder());
  }

  class ResidualRadioListener implements ActionListener {
    TiltalignPanel panel;

    ResidualRadioListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
    }
  }

  class FiducialRadioListener implements ActionListener {
    TiltalignPanel panel;

    FiducialRadioListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
    }
  }

  class TiltAngleRadioListener implements ActionListener {
    TiltalignPanel panel;

    TiltAngleRadioListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      panel.enableTiltAngleSolutionFields();
    }
  }

  class MagnificationRadioListener implements ActionListener {
    TiltalignPanel panel;

    MagnificationRadioListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      panel.enableMagnificationSolutionFields();
    }
  }

  class RotationRadioListener implements ActionListener {
    TiltalignPanel panel;

    RotationRadioListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      panel.enableRotationSolutionFields();
    }
  }

  class DistortionRadioListener implements ActionListener {
    TiltalignPanel panel;

    DistortionRadioListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      panel.setDistortionSolutionState();
    }
  }

  class LocalAlignmentsListener implements ActionListener {
    TiltalignPanel panel;

    LocalAlignmentsListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      panel.enableLocalAlignmentFields();
    }
  }

  class LocalRotationCheckListener implements ActionListener {
    TiltalignPanel panel;

    LocalRotationCheckListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      panel.enableLocalRotationSolutionFields();
    }
  }

  class LocalTiltAngleCheckListener implements ActionListener {
    TiltalignPanel panel;

    LocalTiltAngleCheckListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      panel.enableLocalTiltAngleSolutionFields();
    }
  }

  class LocalMagnificationCheckListener implements ActionListener {
    TiltalignPanel panel;

    LocalMagnificationCheckListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      panel.enableLocalMagnificationSolutionFields();
    }
  }

  class LocalDistortionRadioListener implements ActionListener {
    TiltalignPanel panel;

    LocalDistortionRadioListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      enableLocalDistortionSolutionFields();
    }
  }

  /*
   class CompressionRadioListener implements ActionListener {
   TiltalignPanel panel;
   
   CompressionRadioListener(TiltalignPanel adaptee) {
   panel = adaptee;
   }
   public void actionPerformed(ActionEvent event) {
   panel.updateCompressionSolutionPanel();
   }
   }
   */

  /**
   * Initialize the tooltip text for the axis panel objects
   */
private void setToolTipText() {
    String text;
    Section section;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    Autodoc autodoc = null;
    try {
      autodoc = Autodoc.get(Autodoc.TILTALIGN, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    // General tab
    ltfExcludeList.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.EXCLUDE_LIST_KEY)).format());
    ltfSeparateViewGroups.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.SEPARATE_GROUP_KEY)).format());
    
    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME, TiltalignParam.RESIDUAL_REPORT_CRITERION_KEY);
    if (section  != null ) {
      ltfResidualThreshold.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section)).format());
      rbResidAllViews.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section, "all")).format());
      rbResidNeighboring.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section, "neighboring")).format());
    }
    
    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME, TiltalignParam.SURFACES_TO_ANALYZE_KEY);
    if (section != null) {
      rbSingleFiducialSurface.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section, "1")).format());
      rbDualFiducialSurfaces.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section, "2")).format());
    }

    ltfTiltAngleOffset.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.ANGLE_OFFSET_KEY)).format());
    ltfTiltAxisZShift.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.AXIS_Z_SHIFT_KEY)).format());
    ltfMetroFactor.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.METRO_FACTOR_KEY)).format());
    ltfCycleLimit.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.MAXIMUM_CYCLES_KEY)).format());
    cbLocalAlignments.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_ALIGNMENTS_KEY)).format());
    ltfNLocalPatches.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.NUMBER_OF_LOCAL_PATCHES_X_AND_Y_KEY)).format());
    ltfMinLocalPatchSize.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.MIN_SIZE_OR_OVERLAP_X_AND_Y_KEY)).format());
    ltfMinLocalFiducials.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.MIN_FIDS_TOTAL_AND_EACH_SURFACE_KEY)).format());

    //  Global variables
    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME, TiltalignParam.TILT_OPTION_KEY);
    if (section != null) {
      rbTiltAngleFixed.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section, TiltalignParam.FIXED_OPTION)).format());
      rbTiltAngleAll.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section, TiltalignParam.TILT_ALL_OPTION)).format());
      rbTiltAngleAutomap.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section, TiltalignParam.TILT_AUTOMAPPED_OPTION)).format());
    }
    
    ltfTiltAngleGroupSize.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.TILT_DEFAULT_GROUPING_KEY)).format());
    ltfTiltAngleNonDefaultGroups.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.TILT_NONDEFAULT_GROUP_KEY)).format());

    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME, TiltalignParam.MAG_OPTION_KEY);
    if (section != null) {
      rbMagnificationFixed.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section, TiltalignParam.FIXED_OPTION)).format());
      rbMagnificationAll.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section, TiltalignParam.ALL_OPTION)).format());
      rbMagnificationAutomap.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section, TiltalignParam.AUTOMAPPED_OPTION)).format());
    }

    ltfMagnificationReferenceView.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, TiltalignParam.MAG_REFERENCE_VIEW_KEY))
        .format());
    ltfMagnificationGroupSize.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.MAG_DEFAULT_GROUPING_KEY)).format());
    ltfMagnificationNonDefaultGroups.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.MAG_NONDEFAULT_GROUP_KEY)).format());
    
    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME, TiltalignParam.ROT_OPTION_KEY);
    if (section != null) {
      rbRotationNone.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section, TiltalignParam.NONE_OPTION)).format());
      rbRotationAll.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section, TiltalignParam.ALL_OPTION)).format());
      rbRotationAutomap.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(section, TiltalignParam.AUTOMAPPED_OPTION)).format());
    }

    ltfRotationGroupSize.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.ROT_DEFAULT_GROUPING_KEY)).format());
    ltfRotationNonDefaultGroups.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.ROT_NONDEFAULT_GROUP_KEY)).format());

    text = "Do not solve for distortions in the plane of section.";
    rbDistortionDisabled
        .setToolTipText(tooltipFormatter.setText(text).format());

    text = "Solve for X-stretch and skew in the plane of section.";
    rbDistortionFullSolution.setToolTipText(tooltipFormatter.setText(text)
        .format());

    rbDistortionSkew.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.SKEW_OPTION_KEY)).format());
    ltfXstretchGroupSize.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.X_STRETCH_DEFAULT_GROUPING_KEY)).format());
    ltfXstretchNonDefaultGroups.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.X_STRETCH_NONDEFAULT_GROUP_KEY)).format());
    ltfSkewGroupSize.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.SKEW_DEFAULT_GROUPING_KEY)).format());
    ltfSkewNonDefaultGroups.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.SKEW_NONDEFAULT_GROUP_KEY)).format());
    cbProjectionStretch.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.PROJECTION_STRETCH_KEY)).format());

    //local variables
    cbLocalRotation.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_ROT_OPTION_KEY)).format());
    ltfLocalRotationGroupSize.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_ROT_DEFAULT_GROUPING_KEY)).format());
    ltfLocalRotationNonDefaultGroups.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_ROT_NONDEFAULT_GROUP_KEY)).format());
    cbLocalTiltAngle.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_TILT_OPTION_KEY)).format());
    ltfLocalTiltAngleGroupSize.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_TILT_DEFAULT_GROUPING_KEY)).format());
    ltfLocalTiltAngleNonDefaultGroups.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_TILT_NONDEFAULT_GROUP_KEY)).format());
    cbLocalMagnification.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_MAG_OPTION_KEY)).format());
    ltfLocalMagnificationGroupSize.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_MAG_DEFAULT_GROUPING_KEY)).format());
    ltfLocalMagnificationNonDefaultGroups.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_MAG_NONDEFAULT_GROUP_KEY)).format());
    
    text = "Do not solve for local distortions in the plane of section.";
    rbLocalDistortionDisabled.setToolTipText(tooltipFormatter.setText(text)
        .format());

    text = "Solve for local X-stretch and skew in the plane of section.";
    rbLocalDistortionFullSolution.setToolTipText(tooltipFormatter.setText(text)
        .format());

    rbLocalDistortionSkew.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_SKEW_OPTION_KEY)).format());
    ltfLocalXstretchGroupSize.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_X_STRETCH_DEFAULT_GROUPING_KEY)).format());
    ltfLocalXstretchNonDefaultGroups.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_X_STRETCH_NONDEFAULT_GROUP_KEY)).format());
    ltfLocalSkewGroupSize.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_SKEW_DEFAULT_GROUPING_KEY)).format());
    ltfLocalSkewNonDefaultGroups.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, TiltalignParam.LOCAL_SKEW_NONDEFAULT_GROUP_KEY)).format());
  }
  /**
   * @return Returns the currentPrealignedBinning.
   */
  public int getPrealignedBinning() {
    return prealignedBinning;
  }

  /**
   * @param currentPrealignedBinning The currentPrealignedBinning to set.
   */
  public void setPrealignedBinning(int binning) {
    prealignedBinning = binning;
  }
}

/**
 * <p> $Log$
 * <p> Revision 3.22  2005/02/24 00:52:35  sueh
 * <p> bug# 600 Fixed a bug that was saving a value to the wrong parameter.
 * <p>
 * <p> Revision 3.21  2005/02/21 23:05:04  sueh
 * <p> bug# 600 Making ConstTiltalignParam parameter name statics public.
 * <p>
 * <p> Revision 3.20  2005/02/18 01:29:32  sueh
 * <p> bug# 600 Using parameter names from TiltalignParam.
 * <p>
 * <p> Revision 3.19  2005/02/15 22:22:22  sueh
 * <p> bug# 600 Convert tooltips to autodoc.
 * <p>
 * <p> Revision 3.18  2005/02/11 16:46:12  sueh
 * <p> bug# 600 Getting tooltips from autodoc.
 * <p>
 * <p> Revision 3.17  2005/01/26 00:06:49  sueh
 * <p> Converted ConstEtomoNumber.resetValue to displayValue.
 * <p>
 * <p> Revision 3.16  2005/01/11 01:01:05  sueh
 * <p> bug# 567 Varying the widths of the radio button panels.
 * <p>
 * <p> Revision 3.15  2005/01/06 18:19:45  sueh
 * <p> bug# 567 In getParameters(), get z factor file.
 * <p>
 * <p> Revision 3.14  2005/01/05 20:12:17  sueh
 * <p> bug# 567 Made ProjectionStretch advanced only.
 * <p>
 * <p> Revision 3.13  2005/01/05 20:07:20  sueh
 * <p> bug# 567 Added ProjectionStretch checkbox to the global tab.
 * <p>
 * <p> Revision 3.12  2005/01/05 00:08:31  sueh
 * <p> bug# 567 Set the preferred width of fields that are too small
 * <p>
 * <p> Revision 3.11  2004/12/30 19:35:14  sueh
 * <p> bug# 567 in createGeneralTab(): capitalized radio box title.
 * <p>
 * <p> Revision 3.10  2004/12/30 19:33:53  sueh
 * <p> bug# 567 In createGeneralTab() Maved the items in the Resdual Reporting
 * <p> box to make two rows.
 * <p>
 * <p> Revision 3.9  2004/12/30 18:48:34  sueh
 * <p> bug# 567 In createVariablePanel(): Reduce the space between the
 * <p> checkbox and the text fields
 * <p>
 * <p> Revision 3.8  2004/12/30 18:01:54  sueh
 * <p> bug# 567 Fixed a bug in setParameters(ConstTiltalignParam):
 * <p> localRotationNonDefaultGroups was being set incorrectly.
 * <p>
 * <p> Revision 3.7  2004/12/29 23:50:23  sueh
 * <p> bug# 567 Put checkbox or radio buttons to the left on the fields in global and
 * <p> local tabs and in the local alignment panel on the general tab.
 * <p>
 * <p> Revision 3.6  2004/12/29 01:54:46  sueh
 * <p> bug# 567 Adding rotation solution panel.  Putting global and local rotation
 * <p> solution next to tilt angle solution panel.  Putting global and local
 * <p> xstretch and skew side-by-side.
 * <p>
 * <p> Revision 3.5  2004/12/29 00:16:18  sueh
 * <p> bug# 567 Converted distortion checkbox and local distortion checkbox to
 * <p> radio button groups.  Moved default values to ConstTiltalignParam.
 * <p> Changed update...Panel() functions to enable...Fields().  Changed
 * <p> selectGlobalDistortion to setDistortionSolutionState().  No longer testing
 * <p> type values before loading from the param.  Adapt to new tiltalignParam.
 * <p>
 * <p> Revision 3.4  2004/08/31 21:51:19  sueh
 * <p> bug# 545 SetParameters: handle the situation where fid.xyz exists but is empty.
 * <p> Substitute the pixel spacing from pre-aligned stack.
 * <p>
 * <p> Revision 3.3  2004/07/02 17:42:18  sueh
 * <p> bug#461 removing prints
 * <p>
 * <p> Revision 3.2  2004/07/02 00:40:07  sueh
 * <p> bug# 461 adding a connection to fid.xyz, preali header, and rawstack
 * <p> header to the constructor.  Info from these files is required
 * <p> to calculate z shift.  Correcting the calculation of binning
 * <p> when caculating z shift.
 * <p>
 * <p> Revision 3.1  2004/06/21 17:16:37  rickg
 * <p> Bug #461 z shift is scaled by the prealigned binning
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.10  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.9  2003/10/28 20:59:33  rickg
 * <p> Bug# 280 Tooltips
 * <p>
 * <p> Revision 2.8  2003/10/22 22:53:37  rickg
 * <p> Bug# 279 Removed "linear"
 * <p>
 * <p> Revision 2.7  2003/10/20 20:08:37  sueh
 * <p> Bus322 corrected labels
 * <p>
 * <p> Revision 2.6  2003/10/14 20:30:25  rickg
 * <p> Bug#279  Label layout and name changes
 * <p>
 * <p> Revision 2.5  2003/10/09 23:21:21  rickg
 * <p> Bug#279  Label layout and name changes
 * <p>
 * <p> Revision 2.4  2003/05/27 08:50:28  rickg
 * <p> Context menu handled by parent window
 * <p>
 * <p> Revision 2.3  2003/05/23 22:03:06  rickg
 * <p> Changed default tilt angle group size to 5
 * <p>
 * <p> Revision 2.2  2003/03/20 17:43:52  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.23  2003/01/15 00:11:42  rickg
 * <p> Fixed handling of xstretch and skew types (both global and
 * <p> local) when the original align.com does not have those
 * <p> parameters set and the user requests them.
 * <p>
 * <p> Revision 1.22.2.2  2003/01/24 19:04:54  rickg
 * <p> Merged changes from main branch
 * <p>
 * <p> Revision 1.22.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.22  2003/01/06 05:57:29  rickg
 * <p> Quick fix for residual threshold value text field size.  Needs to be more
 * <p> robust
 * <p>
 * <p> Revision 1.21  2003/01/04 00:24:29  rickg
 * <p> Wrap tab pane with a border to ID it as a tiltalign
 * <p> panel.
 * <p>
 * <p> Revision 1.20  2002/12/31 23:13:47  rickg
 * <p> Layout simplification
 * <p>
 * <p> Revision 1.19  2002/12/24 01:08:08  rickg
 * <p> Moved min local patch size to advanced
 * <p>
 * <p> Revision 1.18  2002/12/20 01:08:19  rickg
 * <p> Spelling correction
 * <p>
 * <p> Revision 1.17  2002/12/18 19:15:31  rickg
 * <p> Added advanced capability for metro factor and
 * <p> cycle limit.
 * <p> Ordered handling of panel updates
 * <p>
 * <p> Revision 1.16  2002/12/18 00:53:00  rickg
 * <p> Update in progress
 * <p>
 * <p> Revision 1.15  2002/12/17 01:02:08  rickg
 * <p> Additional groups are now advanced
 * <p>
 * <p> Revision 1.14  2002/12/10 21:33:10  rickg
 * <p> Add residual threshold control
 * <p>
 * <p> Revision 1.13  2002/12/06 01:02:58  rickg
 * <p> Redesign in progress
 * <p>
 * <p> Revision 1.12  2002/12/05 01:21:31  rickg
 * <p> Redesign in progress
 * <p>
 * <p> Revision 1.11  2002/12/04 04:42:13  rickg
 * <p> Redesign in progress
 * <p>
 * <p> Revision 1.10  2002/12/04 01:19:10  rickg
 * <p> Redesign in progress
 * <p>
 * <p> Revision 1.9  2002/12/03 05:41:13  rickg
 * <p> redesign in progress
 * <p>
 * <p> Revision 1.8  2002/12/03 05:22:29  rickg
 * <p> added getLocalRotationSolutionGroupSize
 * <p>
 * <p> Revision 1.7  2002/12/03 00:53:20  rickg
 * <p> Redesign in progress
 * <p>
 * <p> Revision 1.6  2002/11/25 15:59:37  rickg
 * <p> Removed local skew radio buttons
 * <p>
 * <p> Revision 1.5  2002/11/22 00:56:52  rickg
 * <p> removal of non-used fields in progress
 * <p>
 * <p> Revision 1.4  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.3  2002/10/17 23:40:32  rickg
 * <p> Added methods to set titl/mag and distortion defaults.
 * <p>
 * <p> Revision 1.2  2002/10/16 23:20:36  rickg
 * <p> Reformat
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
