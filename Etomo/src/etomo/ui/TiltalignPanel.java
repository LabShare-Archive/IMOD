package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

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
import etomo.comscript.StringList;
import etomo.comscript.TiltalignParam;
import etomo.type.AxisID;

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

public class TiltalignPanel {
  public static final String rcsid =
    "$Id$";

  private AxisID axisID;

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
  private final int defaultLocalSkewGroupSize = 11;

  private JTabbedPane tabPane = new JTabbedPane();

  //  General pane
  private JPanel pnlGeneral = new JPanel();

  private LabeledTextField ltfResidualThreshold =
    new LabeledTextField("Threshold for residual report: ");

  private JRadioButton rbResidAllViews = new JRadioButton("All views");
  private JRadioButton rbResidNeighboring =
    new JRadioButton("Neighboring views");
  private ButtonGroup bgResidualThreshold = new ButtonGroup();
  private JPanel pnlResidualThreshold = new JPanel();

  private JRadioButton rbSingleFiducialSurface =
    new JRadioButton("Assume fiducials on 1 surface for analysis");
  private JRadioButton rbDualFiducialSurfaces =
    new JRadioButton("Assume fiducials on 2 surfaces for analysis");
  private ButtonGroup bgFiducialSurfaces = new ButtonGroup();
  private JPanel pnlFiducialSurfaces = new JPanel();

  private LabeledTextField ltfExcludeList =
    new LabeledTextField("List of views to exclude: ");
  private LabeledTextField ltfSeparateViewGroups =
    new LabeledTextField("Separate view groups: ");

  private JPanel pnlVolumeParameters = new JPanel();
  private LabeledTextField ltfTiltAngleOffset =
    new LabeledTextField("Total tilt angle offset: ");
  private LabeledTextField ltfTiltAxisZShift =
    new LabeledTextField("Tilt axis z shift: ");

  private JPanel pnlMinimizationParams = new JPanel();
  private LabeledTextField ltfMetroFactor =
    new LabeledTextField("Metro factor: ");
  private LabeledTextField ltfCycleLimit =
    new LabeledTextField("Cycle limit: ");

  private JPanel pnlLocalParameters = new JPanel();
  private JCheckBox chkLocalAlignments =
    new JCheckBox("Enable local alignments");
  private LabeledTextField ltfNLocalPatches =
    new LabeledTextField("Number of local patches (x,y): ");
  private LabeledTextField ltfMinLocalPatchSize =
    new LabeledTextField("Min. local patch size or overlap factor (x,y): ");
  private LabeledTextField ltfMinLocalFiducials =
    new LabeledTextField("Min. number of fiducials (total, each surface): ");

  //  Global variables pane
  private JPanel pnlGlobalVariable = new JPanel();

  //  Tilt angle pane
  private JRadioButton rbTiltAngleFixed = new JRadioButton("Fixed tilt angles");
  private JRadioButton rbTiltAngleAll =
    new JRadioButton("Solve for all except minimum tilt");
  private JRadioButton rbTiltAngleAutomap =
    new JRadioButton("Group tilt angles ");
  private ButtonGroup bgTiltAngleSolution = new ButtonGroup();
  private JPanel pnlTiltAngleSolution = new JPanel();

  private LabeledTextField ltfTiltAngleGroupSize =
    new LabeledTextField("Group size: ");
  private LabeledTextField ltfTiltAngleAdditionalGroups =
    new LabeledTextField("Non-default grouping: ");

  //  Magnfication pane
  private JRadioButton rbMagnificationFixed =
    new JRadioButton("Fixed magnification at 1.0");
  private JRadioButton rbMagnificationAll =
    new JRadioButton("Solve for all magnifications");
  private JRadioButton rbMagnificationAutomap =
    new JRadioButton("Group magnifications");
  private ButtonGroup bgMagnificationSolution = new ButtonGroup();
  private JPanel pnlMagnificationSolution = new JPanel();

  private LabeledTextField ltfMagnificationReferenceView =
    new LabeledTextField("Reference view: ");
  private LabeledTextField ltfMagnificationGroupSize =
    new LabeledTextField("Group size: ");
  private LabeledTextField ltfMagnificationAdditionalGroups =
    new LabeledTextField("Non-default grouping: ");

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
  private JCheckBox chkDistortion = new JCheckBox("Enable");

  private LabeledTextField ltfXstretchGroupSize =
    new LabeledTextField("X stretch group size: ");
  private LabeledTextField ltfXstretchAdditionalGroups =
    new LabeledTextField("X stretch non-default grouping: ");

  private LabeledTextField ltfSkewGroupSize =
    new LabeledTextField("Skew group size: ");
  private LabeledTextField ltfSkewAdditionalGroups =
    new LabeledTextField("Skew non-default grouping: ");

  //  Local variables pane
  private JPanel pnlLocalSolution = new JPanel();

  //  Local tilt angle pane
  private JPanel pnlLocalRotationSolution = new JPanel();
  private JCheckBox chkLocalRotation = new JCheckBox("Enable");

  private LabeledTextField ltfLocalRotationGroupSize =
    new LabeledTextField("Group size: ");
  private LabeledTextField ltfLocalRotationAdditionalGroups =
    new LabeledTextField("Non-default grouping: ");

  //  Local tilt angle pane
  private JPanel pnlLocalTiltAngleSolution = new JPanel();
  private JCheckBox chkLocalTiltAngle = new JCheckBox("Enable");

  private LabeledTextField ltfLocalTiltAngleGroupSize =
    new LabeledTextField("Group size: ");
  private LabeledTextField ltfLocalTiltAngleAdditionalGroups =
    new LabeledTextField("Non-default grouping: ");

  // Local magnfication pane
  private JPanel pnlLocalMagnificationSolution = new JPanel();
  private JCheckBox chkLocalMagnification = new JCheckBox("Enable");

  private LabeledTextField ltfLocalMagnificationGroupSize =
    new LabeledTextField("Group size: ");
  private LabeledTextField ltfLocalMagnificationAdditionalGroups =
    new LabeledTextField("Non-default grouping: ");

  //  Local distortion pane
  private JPanel pnlLocalDistortionSolution = new JPanel();
  private JCheckBox chkLocalDistortion = new JCheckBox("Enable");

  private LabeledTextField ltfLocalXstretchGroupSize =
    new LabeledTextField("X stretch group size: ");
  private LabeledTextField ltfLocalXstretchAdditionalGroups =
    new LabeledTextField("X stretch non-default grouping: ");

  private LabeledTextField ltfLocalSkewGroupSize =
    new LabeledTextField("Skew group size: ");
  private LabeledTextField ltfLocalSkewAdditionalGroups =
    new LabeledTextField("Skew non-default grouping: ");

  /**
   * Constructor
   * @param axis
   */
  public TiltalignPanel(AxisID axis) {
    axisID = axis;

    tabPane.setBorder(new EtchedBorder("Tiltalign Parameters").getBorder());
    //  Create the tabs
    createGeneralTab();
    createGlobalSolutionTab();
    createLocalSolutionTab();

  }

  /**
   * Set the values of the panel using a constant tiltalign parameter
   * object
   */
  public void setParameters(ConstTiltalignParam params) {

    //  General panel parameters

    if (params.getNSurfaceAnalysis() == 2) {
      rbDualFiducialSurfaces.setSelected(true);
    }
    else {
      rbSingleFiducialSurface.setSelected(true);
    }

    ltfResidualThreshold.setText(Math.abs(params.getResidualThreshold()));
    if (params.getResidualThreshold() < 0) {
      rbResidNeighboring.setSelected(true);
    }
    else {
      rbResidAllViews.setSelected(true);
    }

    int excludeType = params.getIncludeExcludeType();
    if (excludeType == 0 || excludeType == 3) {
      ltfExcludeList.setEnabled(true);
      ltfExcludeList.setText(params.getIncludeExcludeList());
    }
    else {
      ltfExcludeList.setEnabled(false);
    }

    ltfSeparateViewGroups.setText(params.getSeparateViewGroups());
    ltfTiltAngleOffset.setText(params.getTiltAngleOffset());
    ltfTiltAxisZShift.setText(params.getTiltAxisZShift());
    ltfMetroFactor.setText(params.getMetroFactor());
    ltfCycleLimit.setText(params.getCycleLimit());

    chkLocalAlignments.setSelected(params.getLocalAlignments());
    ltfNLocalPatches.setText(params.getNLocalPatches());
    ltfMinLocalPatchSize.setText(params.getMinLocalPatchSize());
    ltfMinLocalFiducials.setText(params.getMinLocalFiducials());

    //  Tilt angle solution parameters
    int solutionType = params.getTiltAngleSolutionType();
    if (solutionType == 0) {
      rbTiltAngleFixed.setSelected(true);
    }
    if (solutionType == 2) {
      rbTiltAngleAll.setSelected(true);
    }
    if (solutionType == 5) {
      rbTiltAngleAutomap.setSelected(true);
    }
    if (solutionType == 5) {
      ltfTiltAngleGroupSize.setText(params.getTiltAngleSolutionGroupSize());
      ltfTiltAngleAdditionalGroups.setText(
        params.getTiltAngleSolutionAdditionalGroups());
    }

    //  Magnification solution parameters
    //  TODO what to do if the magnification type is not one of the cases
    //  below
    ltfMagnificationReferenceView.setText(
      params.getMagnificationSolutionReferenceView());
    solutionType = params.getMagnificationSolutionType();
    if (solutionType == 0) {
      rbMagnificationFixed.setSelected(true);
    }
    if (solutionType == 1) {
      rbMagnificationAll.setSelected(true);
    }
    if (solutionType == defaultMagnificationType) {
      rbMagnificationAutomap.setSelected(true);
    }

    if (solutionType > 2) {
      ltfMagnificationGroupSize.setText(
        params.getMagnificationSolutionGroupSize());
      ltfMagnificationAdditionalGroups.setText(
        params.getMagnificationSolutionAdditionalGroups());
    }

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
    solutionType = params.getDistortionSolutionType();
    if (solutionType == 0) {
      chkDistortion.setSelected(false);
    }
    else {
      chkDistortion.setSelected(true);

      ltfXstretchGroupSize.setText(params.getXstretchSolutionGroupSize());
      ltfXstretchAdditionalGroups.setText(
        params.getXstretchSolutionAdditionalGroups());

      //   skew solution parameters
      ltfSkewGroupSize.setText(params.getSkewSolutionGroupSize());
      ltfSkewAdditionalGroups.setText(params.getSkewSolutionAdditionalGroups());
    }

    // Local rotation solution parameters
    // NOTE this is brittle since we are mapping a numeric value to a boolean
    // at David's request
    solutionType = params.getLocalRotationSolutionType();
    if (solutionType == 0) {
      chkLocalRotation.setSelected(false);
    }
    else {
      chkLocalRotation.setSelected(true);
      ltfLocalRotationGroupSize.setText(
        params.getLocalRotationSolutionGroupSize());
      ltfLocalTiltAngleAdditionalGroups.setText(
        params.getLocalTiltAdditionalGroups());
    }

    // Local tilt angle solution parameters
    solutionType = params.getLocalTiltSolutionType();
    if (solutionType == 0) {
      chkLocalTiltAngle.setSelected(false);
    }
    else {
      chkLocalTiltAngle.setSelected(true);
      ltfLocalTiltAngleGroupSize.setText(
        params.getLocalTiltSolutionGroupSize());
      ltfLocalTiltAngleAdditionalGroups.setText(
        params.getLocalTiltAdditionalGroups());
    }

    //  Local magnification solution parameters
    solutionType = params.getLocalMagnificationSolutionType();
    if (solutionType == 0) {
      chkLocalMagnification.setSelected(false);
    }
    else {
      chkLocalMagnification.setSelected(true);
      ltfLocalMagnificationGroupSize.setText(
        params.getLocalMagnificationSolutionGroupSize());
      ltfLocalMagnificationAdditionalGroups.setText(
        params.getLocalMagnificationSolutionAdditionalGroups());
    }

    //  Local distortion solution type
    solutionType = params.getLocalDistortionSolutionType();
    if (solutionType == 0) {
      chkLocalDistortion.setSelected(false);
    }
    else {
      chkLocalDistortion.setSelected(true);

      ltfLocalXstretchGroupSize.setText(
        params.getLocalXstretchSolutionGroupSize());
      ltfLocalXstretchAdditionalGroups.setText(
        params.getLocalXstretchSolutionAdditionalGroups());

      //  Local skew solution parameters
      ltfLocalSkewGroupSize.setText(params.getLocalSkewSolutionGroupSize());
      ltfLocalSkewAdditionalGroups.setText(
        params.getLocalSkewSolutionAdditionalGroups());
    }

    //  Set the UI to match the data
    updateEnabled();
  }

  /**
   * Get the values from the panel by updating tiltalign parameter
   * object.  Currently this makes the assumption that the argument
   * contains valid parameters and that only the known parameters will
   * be changed.
   */
  public void getParameters(TiltalignParam params)
    throws FortranInputSyntaxException {
    String badParameter = "";
    try {
      if (rbDualFiducialSurfaces.isSelected()) {
        params.setNSurfaceAnalysis(2);
      }
      else {
        params.setNSurfaceAnalysis(1);
      }

      badParameter = ltfResidualThreshold.getLabel();
      double resid = Double.parseDouble(ltfResidualThreshold.getText());
      if (rbResidNeighboring.isSelected()) {
        resid *= -1;
      }
      params.setResidualThreshold(resid);

      //  Currently only supports Exclude list or blank entries
      badParameter = ltfExcludeList.getLabel();
      if (ltfExcludeList.isEnabled()) {
        StringList temp = new StringList(0);
        temp.parseString(ltfExcludeList.getText());
        if (temp.getNElements() > 0) {
          params.setIncludeExcludeType(3);
        }
        else {
          params.setIncludeExcludeType(0);
        }
        params.setIncludeExcludeList(temp.toString());

      }
      badParameter = ltfSeparateViewGroups.getLabel();
      params.setSeparateViewGroups(ltfSeparateViewGroups.getText());

      badParameter = ltfTiltAngleOffset.getLabel();
      params.setTiltAngleOffset(ltfTiltAngleOffset.getText());

      badParameter = ltfTiltAxisZShift.getLabel();
      params.setTiltAxisZShift(ltfTiltAxisZShift.getText());

      badParameter = ltfMetroFactor.getLabel();
      params.setMetroFactor(ltfMetroFactor.getText());

      badParameter = ltfCycleLimit.getLabel();
      params.setCycleLimit(ltfCycleLimit.getText());

      badParameter = chkLocalAlignments.getText();
      params.setLocalAlignments(chkLocalAlignments.isSelected());

      badParameter = ltfNLocalPatches.getLabel();
      params.setNLocalPatches(ltfNLocalPatches.getText());

      badParameter = ltfMinLocalPatchSize.getLabel();
      params.setMinLocalPatchSize(ltfMinLocalPatchSize.getText());

      badParameter = ltfMinLocalFiducials.getLabel();
      params.setMinLocalFiducials(ltfMinLocalFiducials.getText());

      // Tilt angle pane
      int type = 0;
      if (rbTiltAngleFixed.isSelected())
        type = 0;
      if (rbTiltAngleAll.isSelected())
        type = 2;
      if (rbTiltAngleAutomap.isSelected())
        type = 5;
      params.setTiltAngleSolutionType(type);
      if (type > 2) {
        badParameter = ltfTiltAngleGroupSize.getLabel();
        params.setTiltAngleSolutionGroupSize(ltfTiltAngleGroupSize.getText());

        badParameter = ltfTiltAngleAdditionalGroups.getLabel();
        params.setTiltAngleSolutionAdditionalGroups(
          ltfTiltAngleAdditionalGroups.getText());
      }

      // Magnification pane
      badParameter = ltfMagnificationReferenceView.getLabel();
      params.setMagnificationReferenceView(
        ltfMagnificationReferenceView.getText());

      if (rbMagnificationFixed.isSelected())
        type = 0;
      if (rbMagnificationAll.isSelected())
        type = 1;
      if (rbMagnificationAutomap.isSelected())
        type = defaultMagnificationType;
      params.setMagnificationType(type);

      if (type > 2) {
        badParameter = ltfMagnificationGroupSize.getLabel();
        params.setMagnificationSolutionGroupSize(
          ltfMagnificationGroupSize.getText());

        badParameter = ltfMagnificationAdditionalGroups.getLabel();
        params.setMagnificationSolutionAdditionalGroups(
          ltfMagnificationAdditionalGroups.getText());
      }

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
      if (chkDistortion.isSelected()) {
        //  Set the necessary types for distortion xstretch and skew
        params.setDistortionSolutionType(defaultDistortionType);
        params.setXstretchType(defaultXstretchType);
        params.setSkewType(defaultSkewType);

        badParameter = ltfXstretchGroupSize.getLabel();
        params.setXstretchSolutionGroupSize(ltfXstretchGroupSize.getText());

        badParameter = ltfXstretchAdditionalGroups.getLabel();
        params.setXstretchSolutionAdditionalGroups(
          ltfXstretchAdditionalGroups.getText());

        badParameter = ltfSkewGroupSize.getLabel();
        params.setSkewSolutionGroupSize(ltfSkewGroupSize.getText());

        badParameter = ltfSkewAdditionalGroups.getLabel();
        params.setSkewSolutionAdditionalGroups(
          ltfSkewAdditionalGroups.getText());
      }
      else {
        params.setDistortionSolutionType(0);
      }

      //  Get the local alignment parameters
      // Rotation pane
      // NOTE this only works if 0 and 5 are valid local tilt angle codes
      type = 0;
      if (chkLocalRotation.isSelected())
        type = defaultLocalRotationType;
      params.setLocalRotationSolutionType(type);

      if (type == defaultLocalRotationType) {
        badParameter = ltfLocalRotationGroupSize.getLabel();
        params.setLocalRotationSolutionGroupSize(
          ltfLocalRotationGroupSize.getText());

        badParameter = ltfLocalRotationAdditionalGroups.getLabel();
        params.setLocalRotationSolutionAdditionalGroups(
          ltfLocalRotationAdditionalGroups.getText());
      }

      // Tilt angle pane
      type = 0;
      if (chkLocalTiltAngle.isSelected())
        type = defaultLocalTiltAngleType;
      params.setLocalTiltSolutionType(type);

      if (type == defaultLocalTiltAngleType) {
        badParameter = ltfLocalTiltAngleGroupSize.getLabel();
        params.setLocalTiltSolutionGroupSize(
          ltfLocalTiltAngleGroupSize.getText());

        badParameter = ltfLocalTiltAngleAdditionalGroups.getLabel();
        params.setLocalTiltSolutionAdditionalGroups(
          ltfLocalTiltAngleAdditionalGroups.getText());
      }

      // Local magnification pane
      if (chkLocalMagnification.isSelected()) {
        params.setLocalMagnificationType(defaultLocalMagnificationType);
        badParameter = ltfLocalMagnificationGroupSize.getLabel();
        params.setLocalMagnificationSolutionGroupSize(
          ltfLocalMagnificationGroupSize.getText());

        badParameter = ltfLocalMagnificationAdditionalGroups.getLabel();
        params.setLocalMagnificationSolutionAdditionalGroups(
          ltfLocalMagnificationAdditionalGroups.getText());

      }
      else {
        params.setLocalMagnificationType(0);
      }

      // Distortion pane
      type = 0;
      if (chkLocalDistortion.isSelected()) {
        params.setLocalDistortionSolutionType(defaultLocalDistortionType);
        params.setLocalXstretchType(defaultLocalXstretchType);
        params.setLocalSkewType(defaultLocalXstretchType);

        badParameter = ltfLocalXstretchGroupSize.getLabel();
        params.setLocalXstretchSolutionGroupSize(
          ltfLocalXstretchGroupSize.getText());

        badParameter = ltfLocalXstretchAdditionalGroups.getLabel();
        params.setLocalXstretchSolutionAdditionalGroups(
          ltfLocalXstretchAdditionalGroups.getText());

        badParameter = ltfLocalSkewGroupSize.getLabel();
        params.setLocalSkewSolutionGroupSize(ltfLocalSkewGroupSize.getText());

        badParameter = ltfLocalSkewAdditionalGroups.getLabel();
        params.setLocalSkewSolutionAdditionalGroups(
          ltfLocalSkewAdditionalGroups.getText());
      }
      else {
        params.setLocalDistortionSolutionType(0);
      }
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
    ltfTiltAngleAdditionalGroups.setVisible(state);
    ltfMagnificationAdditionalGroups.setVisible(state);
    ltfXstretchAdditionalGroups.setVisible(state);
    ltfSkewAdditionalGroups.setVisible(state);
    ltfLocalRotationAdditionalGroups.setVisible(state);
    ltfLocalTiltAngleAdditionalGroups.setVisible(state);
    ltfLocalMagnificationAdditionalGroups.setVisible(state);
    ltfLocalXstretchAdditionalGroups.setVisible(state);
    ltfLocalSkewAdditionalGroups.setVisible(state);
    ltfMinLocalPatchSize.setVisible(state);
  }

  void selectGlobalDistortion() {
    if (chkDistortion.isSelected()) {
      chkLocalDistortion.setSelected(true);
      setDistortionDefaults();
      setLocalDistortionDefaults();
    }
    else {
      chkLocalDistortion.setSelected(false);
      setTiltAndMagnificationDefaults();
    }
    updateEnabled();
  }

  void selectLocalDistortion() {
    if (chkLocalDistortion.isSelected()) {
      setLocalDistortionDefaults();
    }
    updateEnabled();
  }

  /**
   * Set the UI parameters to the defaults for a tilt/mag solution.
   */
  void setTiltAndMagnificationDefaults() {
    rbTiltAngleAll.setSelected(true);
    chkDistortion.setSelected(false);
    chkLocalDistortion.setSelected(false);
  }

  /**
   * Set the UI parameters to the default for a distortion solution.  If the
   * group size and additional group lists do not contain any text set them
   * to the defaults.
   */
  void setDistortionDefaults() {
    rbTiltAngleAutomap.setSelected(true);
    if (ltfTiltAngleGroupSize.getText().matches("^\\s*$")) {
      ltfTiltAngleGroupSize.setText(defaultTiltAngleGroupSize);
    }

    chkDistortion.setSelected(true);
    chkLocalDistortion.setSelected(true);

    // If any of the size fields are empty fill them in with the defaults
    // This will happen if someone starts with a com file with distortion
    // disabled and then enables distortion
    if (ltfXstretchGroupSize.getText().matches("^\\s*$")) {
      ltfXstretchGroupSize.setText(defaultXstretchGroupSize);
    }
    if (ltfSkewGroupSize.getText().matches("^\\s*$")) {
      ltfSkewGroupSize.setText(defaultSkewGroupSize);
    }
  }

  void setLocalDistortionDefaults() {
    // If any of the size fields are empty fill them in with the defaults
    // This will happen if someone starts with a com file with distortion
    // disabled and then enables distortion
    if (ltfLocalXstretchGroupSize.getText().matches("^\\s*$")) {
      ltfLocalXstretchGroupSize.setText(defaultLocalXstretchGroupSize);
    }
    if (ltfLocalSkewGroupSize.getText().matches("^\\s*$")) {
      ltfLocalSkewGroupSize.setText(defaultLocalSkewGroupSize);
    }
  }

  // Residual solution panel, nothing much to do.  This is here so that
  // this section matches the other's pattern
  void updateResidualSolutionPanel() {

  }

  // Fiducial solution radio buttons, nothing much to do.  This is here so that
  // this section matches the other's pattern
  void updateFiducialSolutionPanel() {

  }

  //  Local alignment state
  void updateLocalAlignmentState() {
    boolean state = chkLocalAlignments.isSelected();
    ltfNLocalPatches.setEnabled(state);
    ltfMinLocalPatchSize.setEnabled(state);
    ltfMinLocalFiducials.setEnabled(state);
    tabPane.setEnabledAt(tabPane.indexOfComponent(pnlLocalSolution), state);
  }

  /**
   * Signal each pane to update its enabled/disabled state.
   */
  public void updateEnabled() {
    //  update all of the enable/disable states
    updateLocalAlignmentState();

    updateTiltAngleSolutionPanel();
    updateMagnificationSolutionPanel();
    //    updateCompressionSolutionPanel();
    updateDistortionSolutionPanel();

    updateLocalRotationSolutionPanel();
    updateLocalTiltAngleSolutionPanel();
    updateLocalMagnificationSolutionPanel();
    updateLocalDistortionSolutionPanel();
  }

  /**
   * Update the enabled/disabled state of the specified solution panel.
   */
  void updateTiltAngleSolutionPanel() {
    boolean state = rbTiltAngleAutomap.isSelected();
    ltfTiltAngleGroupSize.setEnabled(state);
    ltfTiltAngleAdditionalGroups.setEnabled(state);
  }

  void updateMagnificationSolutionPanel() {
    boolean state = rbMagnificationAutomap.isSelected();
    ltfMagnificationGroupSize.setEnabled(state);
    ltfMagnificationAdditionalGroups.setEnabled(state);
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

  void updateDistortionSolutionPanel() {
    //  Xstretch and skew panel state
    boolean state = chkDistortion.isSelected();
    ltfXstretchGroupSize.setEnabled(state);
    ltfXstretchAdditionalGroups.setEnabled(state);
    ltfSkewGroupSize.setEnabled(state);
    ltfSkewAdditionalGroups.setEnabled(state);
  }

  void updateLocalRotationSolutionPanel() {
    boolean state = chkLocalRotation.isSelected();
    ltfLocalRotationGroupSize.setEnabled(state);
    ltfLocalRotationAdditionalGroups.setEnabled(state);
  }

  void updateLocalTiltAngleSolutionPanel() {
    boolean state = chkLocalTiltAngle.isSelected();
    ltfLocalTiltAngleGroupSize.setEnabled(state);
    ltfLocalTiltAngleAdditionalGroups.setEnabled(state);
  }

  void updateLocalMagnificationSolutionPanel() {
    boolean state = chkLocalMagnification.isSelected();
    ltfLocalMagnificationGroupSize.setEnabled(state);
    ltfLocalMagnificationAdditionalGroups.setEnabled(state);
  }

  void updateLocalDistortionSolutionPanel() {
    boolean state = chkLocalDistortion.isSelected();
    ltfLocalXstretchGroupSize.setEnabled(state);
    ltfLocalXstretchAdditionalGroups.setEnabled(state);
    ltfLocalSkewGroupSize.setEnabled(state);
    ltfLocalSkewAdditionalGroups.setEnabled(state);
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
  private void createRadioBox(
    JPanel panel,
    ButtonGroup group,
    JRadioButton[] items,
    ActionListener listener) {
    int width = 300;
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

    pnlResidualThreshold.setLayout(
      new BoxLayout(pnlResidualThreshold, BoxLayout.X_AXIS));
    pnlResidualThreshold.setBorder(
      new EtchedBorder("Residual Reporting").getBorder());
    ltfResidualThreshold.setColumns(10);
    pnlResidualThreshold.add(ltfResidualThreshold.getContainer());
    pnlResidualThreshold.add(new JLabel(" s.d. relative to   "));
    JRadioButton[] items = new JRadioButton[2];
    items[0] = rbResidAllViews;
    items[1] = rbResidNeighboring;
    JPanel pnlRBResidual = new JPanel();
    pnlRBResidual.setLayout(new BoxLayout(pnlRBResidual, BoxLayout.Y_AXIS));

    ResidualRadioListener residualRadioListener =
      new ResidualRadioListener(this);
    createRadioBox(
      pnlRBResidual,
      bgResidualThreshold,
      items,
      residualRadioListener);
    pnlResidualThreshold.add(pnlRBResidual);

    pnlGeneral.add(pnlResidualThreshold);
    pnlGeneral.add(Box.createRigidArea(FixedDim.x0_y10));

    pnlFiducialSurfaces.setLayout(
      new BoxLayout(pnlFiducialSurfaces, BoxLayout.X_AXIS));
    pnlFiducialSurfaces.setBorder(
      new EtchedBorder("Analysis of Surface Angles").getBorder());

    //  Need an extra panel to make border extend the appropriate width
    JPanel pnlRBFiducual = new JPanel();
    pnlRBFiducual.setLayout(new BoxLayout(pnlRBFiducual, BoxLayout.Y_AXIS));
    items = new JRadioButton[2];
    items[0] = rbSingleFiducialSurface;
    items[1] = rbDualFiducialSurfaces;
    FiducialRadioListener fiducialRadioListener =
      new FiducialRadioListener(this);
    createRadioBox(
      pnlRBFiducual,
      bgFiducialSurfaces,
      items,
      fiducialRadioListener);

    pnlFiducialSurfaces.add(pnlRBFiducual);
    pnlFiducialSurfaces.add(Box.createHorizontalGlue());
    pnlGeneral.add(pnlFiducialSurfaces);
    pnlGeneral.add(Box.createRigidArea(FixedDim.x0_y10));

    pnlVolumeParameters.setLayout(
      new BoxLayout(pnlVolumeParameters, BoxLayout.Y_AXIS));
    pnlVolumeParameters.setBorder(
      new EtchedBorder("Volume Position Parameters").getBorder());
    pnlVolumeParameters.add(ltfTiltAngleOffset.getContainer());
    pnlVolumeParameters.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlVolumeParameters.add(ltfTiltAxisZShift.getContainer());
    pnlVolumeParameters.add(Box.createRigidArea(FixedDim.x0_y5));
		pnlGeneral.add(pnlVolumeParameters);
		pnlGeneral.add(Box.createRigidArea(FixedDim.x0_y10));
		
    pnlMinimizationParams.setLayout(
      new BoxLayout(pnlMinimizationParams, BoxLayout.Y_AXIS));
    pnlMinimizationParams.setBorder(
      new EtchedBorder("Minimization Parameters").getBorder());
    pnlMinimizationParams.add(ltfMetroFactor.getContainer());
    pnlMinimizationParams.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMinimizationParams.add(ltfCycleLimit.getContainer());
    pnlMinimizationParams.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlGeneral.add(pnlMinimizationParams);
    pnlGeneral.add(Box.createRigidArea(FixedDim.x0_y10));

    pnlLocalParameters.setLayout(
      new BoxLayout(pnlLocalParameters, BoxLayout.Y_AXIS));
		pnlLocalParameters.setBorder(
      new EtchedBorder("Local Alignment Parameters").getBorder());
    pnlLocalParameters.add(chkLocalAlignments);
    chkLocalAlignments.setAlignmentX(Container.RIGHT_ALIGNMENT);
    pnlLocalParameters.add(Box.createRigidArea(FixedDim.x0_y5));
    LocalAlignmentsListener localAlignmentsListener =
      new LocalAlignmentsListener(this);
    chkLocalAlignments.addActionListener(localAlignmentsListener);

    pnlLocalParameters.add(ltfNLocalPatches.getContainer());
    pnlLocalParameters.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlLocalParameters.add(ltfMinLocalPatchSize.getContainer());
    pnlLocalParameters.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlLocalParameters.add(ltfMinLocalFiducials.getContainer());
    pnlGeneral.add(pnlLocalParameters);
    pnlGeneral.add(Box.createVerticalGlue());

    tabPane.addTab("General", pnlGeneral);
  }

  /**
   * Layout the global estimate tab
   */
  private void createGlobalSolutionTab() {
    pnlGlobalVariable.setLayout(
      new BoxLayout(pnlGlobalVariable, BoxLayout.Y_AXIS));

    //  Layout the global tilt angle estimate pane
    pnlTiltAngleSolution.setLayout(
      new BoxLayout(pnlTiltAngleSolution, BoxLayout.Y_AXIS));

    JRadioButton[] items = new JRadioButton[3];
    items[0] = rbTiltAngleFixed;
    items[1] = rbTiltAngleAll;
    items[2] = rbTiltAngleAutomap;
    TiltAngleRadioListener tiltAngleRadioListener =
      new TiltAngleRadioListener(this);
    createRadioBox(
      pnlTiltAngleSolution,
      bgTiltAngleSolution,
      items,
      tiltAngleRadioListener);
    pnlTiltAngleSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTiltAngleSolution.add(ltfTiltAngleGroupSize.getContainer());
    pnlTiltAngleSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTiltAngleSolution.add(ltfTiltAngleAdditionalGroups.getContainer());

    pnlTiltAngleSolution.setBorder(
      new EtchedBorder("Tilt Angle Solution Type").getBorder());

    //  Layout the global magnification variable parameters
    pnlMagnificationSolution.setLayout(
      new BoxLayout(pnlMagnificationSolution, BoxLayout.Y_AXIS));
    items = new JRadioButton[3];
    items[0] = rbMagnificationFixed;
    items[1] = rbMagnificationAll;
    items[2] = rbMagnificationAutomap;
    MagnificationRadioListener magnificationRadioListener =
      new MagnificationRadioListener(this);
    createRadioBox(
      pnlMagnificationSolution,
      bgMagnificationSolution,
      items,
      magnificationRadioListener);

    pnlMagnificationSolution.add(ltfMagnificationReferenceView.getContainer());
    pnlMagnificationSolution.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlMagnificationSolution.add(ltfMagnificationGroupSize.getContainer());
    pnlMagnificationSolution.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlMagnificationSolution.add(
      ltfMagnificationAdditionalGroups.getContainer());
    pnlMagnificationSolution.setBorder(
      new EtchedBorder("Magnification Solution Type").getBorder());

    // Layout the global distortion pane
    createVariablePanel(
      pnlDistortionSolution,
      chkDistortion,
      ltfXstretchGroupSize,
      ltfXstretchAdditionalGroups,
      "Distortion Solution Type");

    pnlDistortionSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlDistortionSolution.add(ltfSkewGroupSize.getContainer());

    pnlDistortionSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlDistortionSolution.add(ltfSkewAdditionalGroups.getContainer());

    DistortionCheckListener DistortionCheckListener =
      new DistortionCheckListener(this);
    chkDistortion.addActionListener(DistortionCheckListener);

    //  Add the individual panes to the tab
    pnlGlobalVariable.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlGlobalVariable.add(pnlTiltAngleSolution);
    pnlGlobalVariable.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlGlobalVariable.add(Box.createVerticalGlue());
    pnlGlobalVariable.add(pnlMagnificationSolution);
    pnlGlobalVariable.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlGlobalVariable.add(Box.createVerticalGlue());
    pnlGlobalVariable.add(pnlDistortionSolution);

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
    pnlLocalSolution.setLayout(
      new BoxLayout(pnlLocalSolution, BoxLayout.Y_AXIS));
    //pnlLocalSolution.setPreferredSize(new Dimension(400, 350));

    //  Construct the rotation solution objects
    createVariablePanel(
      pnlLocalRotationSolution,
      chkLocalRotation,
      ltfLocalRotationGroupSize,
      ltfLocalRotationAdditionalGroups,
      "Local Rotation Solution Type");
    LocalRotationCheckListener localRotationCheckListener =
      new LocalRotationCheckListener(this);
    chkLocalRotation.addActionListener(localRotationCheckListener);

    //  Construct the tilt angle solution objects
    createVariablePanel(
      pnlLocalTiltAngleSolution,
      chkLocalTiltAngle,
      ltfLocalTiltAngleGroupSize,
      ltfLocalTiltAngleAdditionalGroups,
      "Local Tilt Angle Aolution Type");

    LocalTiltAngleCheckListener localTiltAngleCheckListener =
      new LocalTiltAngleCheckListener(this);
    chkLocalTiltAngle.addActionListener(localTiltAngleCheckListener);

    //  Construct the local magnification pane
    createVariablePanel(
      pnlLocalMagnificationSolution,
      chkLocalMagnification,
      ltfLocalMagnificationGroupSize,
      ltfLocalMagnificationAdditionalGroups,
      "Local Magnification Solution Type");

    LocalMagnificationCheckListener localMagnificationCheckListener =
      new LocalMagnificationCheckListener(this);
    chkLocalMagnification.addActionListener(localMagnificationCheckListener);

    //  Construction the local distortion pane
    createVariablePanel(
      pnlLocalDistortionSolution,
      chkLocalDistortion,
      ltfLocalXstretchGroupSize,
      ltfLocalXstretchAdditionalGroups,
      "Local Distortion Dolution Type");

		pnlLocalDistortionSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlLocalDistortionSolution.add(ltfLocalSkewGroupSize.getContainer());
    pnlLocalDistortionSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlLocalDistortionSolution.add(ltfLocalSkewAdditionalGroups.getContainer());

    LocalDistortionCheckListener localDistortionCheckListener =
      new LocalDistortionCheckListener(this);
    chkLocalDistortion.addActionListener(localDistortionCheckListener);

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

  private void createVariablePanel(
    JPanel panel,
    JCheckBox checkBox,
    LabeledTextField groupSize,
    LabeledTextField additionalGroups,
    String title) {

    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

    //  Add the check box
    //    checkBox.setAlignmentX(Component.LEFT_ALIGNMENT);
    panel.add(checkBox);

    //  Add the group size labeled text field
    //    groupSize.setAlignmentX(Component.LEFT_ALIGNMENT);
    panel.add(groupSize.getContainer());

    //  Add the additional groups labeled text field
    panel.add(Box.createRigidArea(FixedDim.x0_y5));
    //    additionalGroups.setAlignmentX(Component.LEFT_ALIGNMENT);
    panel.add(additionalGroups.getContainer());

    panel.setBorder(new EtchedBorder(title).getBorder());
  }
  class ResidualRadioListener implements ActionListener {
    TiltalignPanel panel;

    ResidualRadioListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }
    public void actionPerformed(ActionEvent event) {
      panel.updateResidualSolutionPanel();
    }
  }

  class FiducialRadioListener implements ActionListener {
    TiltalignPanel panel;

    FiducialRadioListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }
    public void actionPerformed(ActionEvent event) {
      panel.updateFiducialSolutionPanel();
    }
  }

  class TiltAngleRadioListener implements ActionListener {
    TiltalignPanel panel;

    TiltAngleRadioListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }
    public void actionPerformed(ActionEvent event) {
      panel.updateTiltAngleSolutionPanel();
    }
  }

  class MagnificationRadioListener implements ActionListener {
    TiltalignPanel panel;

    MagnificationRadioListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }
    public void actionPerformed(ActionEvent event) {
      panel.updateMagnificationSolutionPanel();
    }
  }

  class DistortionCheckListener implements ActionListener {
    TiltalignPanel panel;

    DistortionCheckListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }
    public void actionPerformed(ActionEvent event) {
      panel.selectGlobalDistortion();
    }
  }

  class LocalAlignmentsListener implements ActionListener {
    TiltalignPanel panel;

    LocalAlignmentsListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }
    public void actionPerformed(ActionEvent event) {
      panel.updateLocalAlignmentState();
    }
  }

  class LocalRotationCheckListener implements ActionListener {
    TiltalignPanel panel;

    LocalRotationCheckListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }
    public void actionPerformed(ActionEvent event) {
      panel.updateLocalRotationSolutionPanel();
    }
  }

  class LocalTiltAngleCheckListener implements ActionListener {
    TiltalignPanel panel;

    LocalTiltAngleCheckListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }
    public void actionPerformed(ActionEvent event) {
      panel.updateLocalTiltAngleSolutionPanel();
    }
  }

  class LocalMagnificationCheckListener implements ActionListener {
    TiltalignPanel panel;

    LocalMagnificationCheckListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }
    public void actionPerformed(ActionEvent event) {
      panel.updateLocalMagnificationSolutionPanel();
    }
  }

  class LocalDistortionCheckListener implements ActionListener {
    TiltalignPanel panel;

    LocalDistortionCheckListener(TiltalignPanel adaptee) {
      panel = adaptee;
    }
    public void actionPerformed(ActionEvent event) {
      panel.selectLocalDistortion();
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

}
