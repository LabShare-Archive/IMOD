package etomo.ui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.TitledBorder;

import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.TiltalignParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.StringList;

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
public class TiltalignPanel implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private String logSuffix;

  //  FIXME need recomended default for all sub groups see (align.com)
  private final int defaultTiltAngleType = 5;
  private final int defaultTiltAngleGroupSize = 10;
  private final int defaultMagnificationType = 3;
  private final int defaultDistortionType = 2;
  private final int defaultXStretchGroupSize = 7;
  private final int defaultSkewGroupSize = 11;
  private final int defaultLocalRotationType = 3;
  private final int defaultLocalRotationGroupSize = 6;
  private final int defaultLocalTiltAngleType = 5;
  private final int defaultLocalTiltAngleGroupSize = 6;
  private final int defaultLocalMagnificationType = 3;
  private final int defaultLocalMagnificationGroupSize = 7;
  private final int defaultLocalDistortionType = 2;
  private final int defaultLocalXStretchGroupSize = 7;
  private final int defaultLocalSkewGroupSize = 11;

  // FIXME: these should be gotten from the app some how
  private final Color highlight = new Color(248, 254, 255);
  private final Color shadow = new Color(121, 124, 136);

  private JTabbedPane tabPane = new JTabbedPane();

  //  General pane
  private JPanel panelGeneral = new JPanel();

  private LabeledTextField ltfNSurfaceAnalysis =
    new LabeledTextField("Number of fiducial surfaces: ");
  private LabeledTextField ltfExcludeList =
    new LabeledTextField("Exclude list: ");
  private LabeledTextField ltfAdditionalViewGroups =
    new LabeledTextField("Additional view groups: ");
  private LabeledTextField ltfTiltAngleOffset =
    new LabeledTextField("Tilt angle offset: ");
  private LabeledTextField ltfTiltAxisZShift =
    new LabeledTextField("Tilt axis z shift: ");

  private JCheckBox chkLocalAlignments =
    new JCheckBox("Enable local alignments");
  private LabeledTextField ltfNLocalPatches =
    new LabeledTextField("# of local patches (x,y): ");
  private LabeledTextField ltfMinLocalPatchSize =
    new LabeledTextField("Min. local patch size (x,y): ");
  private LabeledTextField ltfMinLocalFiducials =
    new LabeledTextField("Min.# of fiducials (total, each surface): ");

  //  Global variables pane
  private JPanel panelGlobalVariable = new JPanel();

  //  Tilt angle pane
  private JRadioButton rbTiltAngleFixed = new JRadioButton("Fixed tilt angles");
  private JRadioButton rbTiltAngleAll =
    new JRadioButton("Solve for all except minimum tilt");
  private JRadioButton rbTiltAngleAutomap =
    new JRadioButton("Group tilt angles (linear)");
  private ButtonGroup bgTiltAngleSolution = new ButtonGroup();
  private JPanel panelTiltAngleSolution = new JPanel();

  private LabeledTextField ltfTiltAngleGroupSize =
    new LabeledTextField("Tilt angle group size: ");
  private LabeledTextField ltfTiltAngleAdditionalGroups =
    new LabeledTextField("Tilt angle additional group list: ");

  //  Magnfication pane
  private JRadioButton rbMagnificationFixed =
    new JRadioButton("Fixed magnification at 1.0");
  private JRadioButton rbMagnificationAll =
    new JRadioButton("Solve for all magnifications");
  private JRadioButton rbMagnificationAutomap =
    new JRadioButton("Group magnification");
  private ButtonGroup bgMagnificationSolution = new ButtonGroup();
  private JPanel panelMagnificationSolution = new JPanel();

  private LabeledTextField ltfMagnificationReferenceView =
    new LabeledTextField("Magnification reference view: ");
  private LabeledTextField ltfMagnificationGroupSize =
    new LabeledTextField("Magnification group size: ");
  private LabeledTextField ltfMagnificationAdditionalGroups =
    new LabeledTextField("Magnification additional group list: ");

  //  Compression pane
  private JRadioButton rbCompressionAll =
    new JRadioButton("Solve for all Compressions");
  private JRadioButton rbCompressionAutomapLinear =
    new JRadioButton("Group compression (first order fit)");
  private JRadioButton rbCompressionAutomapFixed =
    new JRadioButton("Group compression (zeroth order fit)");
  private ButtonGroup bgCompressionSolution = new ButtonGroup();
  private JPanel panelCompressionSolution = new JPanel();

  private LabeledTextField ltfCompressionReferenceView =
    new LabeledTextField("Compression reference view: ");
  private LabeledTextField ltfCompressionGroupSize =
    new LabeledTextField("Compression group size: ");
  private LabeledTextField ltfCompressionAdditionalGroups =
    new LabeledTextField("Compression additional group list: ");

  // GlobalDistortion pane
  private JPanel panelDistortionSolution = new JPanel();
  private JCheckBox chkDistortion = new JCheckBox(" distortion");

  private LabeledTextField ltfXstretchGroupSize =
    new LabeledTextField("X-stretch group size: ");
  private LabeledTextField ltfXstretchAdditionalGroups =
    new LabeledTextField("X-stretch additional group list: ");

  private LabeledTextField ltfSkewGroupSize =
    new LabeledTextField("Skew group size: ");
  private LabeledTextField ltfSkewAdditionalGroups =
    new LabeledTextField("Skew additional group list: ");

  //  Local variables pane
  private JPanel panelLocalSolution = new JPanel();

  //  Local tilt angle pane
  private JPanel panelLocalRotationSolution = new JPanel();
  private JCheckBox chkLocalRotation = new JCheckBox("Local rotation");

  private LabeledTextField ltfLocalRotationGroupSize =
    new LabeledTextField("Rotation group size: ");
  private LabeledTextField ltfLocalRotationAdditionalGroups =
    new LabeledTextField("Rotation additional group list: ");

  //  Local tilt angle pane
  private JPanel panelLocalTiltAngleSolution = new JPanel();
  private JCheckBox chkLocalTiltAngle = new JCheckBox("Local tilt angle");

  private LabeledTextField ltfLocalTiltAngleGroupSize =
    new LabeledTextField("Tilt angle group size: ");
  private LabeledTextField ltfLocalTiltAngleAdditionalGroups =
    new LabeledTextField("Tilt angle additional group list: ");

  // Local magnfication pane
  private JPanel panelLocalMagnificationSolution = new JPanel();
  private JCheckBox chkLocalMagnification =
    new JCheckBox("Local magnification");

  private LabeledTextField ltfLocalMagnificationGroupSize =
    new LabeledTextField("Magnification group size: ");
  private LabeledTextField ltfLocalMagnificationAdditionalGroups =
    new LabeledTextField("Magnification additional group list: ");

  //  Local distortion pane
  private JPanel panelLocalDistortionSolution = new JPanel();
  private JCheckBox chkLocalDistortion = new JCheckBox("Local distortion");

  private LabeledTextField ltfLocalXstretchGroupSize =
    new LabeledTextField("X-stretch group size: ");
  private LabeledTextField ltfLocalXstretchAdditionalGroups =
    new LabeledTextField("X-stretch additional group list: ");

  private LabeledTextField ltfLocalSkewGroupSize =
    new LabeledTextField("Skew group size: ");
  private LabeledTextField ltfLocalSkewAdditionalGroups =
    new LabeledTextField("Skew additional group list: ");

  private Dimension dimLTF = new Dimension(1000, 30);

  public TiltalignPanel(String suffix) {
    logSuffix = suffix;

    //  Create the tabs
    createGeneralTab();
    createGlobalSolutionTab();
    createLocalSolutionTab();

    //
    //  Mouse adapter for context menu
    //
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    panelGeneral.addMouseListener(mouseAdapter);

  }

  /**
   * Set the values of the panel using a constant tiltalign parameter
   * object
   */
  public void setParameters(ConstTiltalignParam params) {

    //  General panel parameters
    ltfNSurfaceAnalysis.setText(params.getNSurfaceAnalysis());
    int excludeType = params.getIncludeExcludeType();
    if (excludeType == 0 || excludeType == 3) {
      ltfExcludeList.setEnabled(true);
      ltfExcludeList.setText(params.getIncludeExcludeList());
    }
    else {
      ltfExcludeList.setEnabled(false);
    }

    ltfAdditionalViewGroups.setText(params.getAdditionalViewGroups());
    ltfTiltAngleOffset.setText(params.getTiltAngleOffset());
    ltfTiltAxisZShift.setText(params.getTiltAxisZShift());
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
    //  FIXME: what to do if the magnification type is not one of the cases
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
    // FIXME: this is brittle since we are mapping a numeric value to a boolean
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

  public void updateEnabled() {
    //  update all of the enable/disable states
    updateTiltAngleSolutionPanel();
    updateMagnificationSolutionPanel();
    updateCompressionSolutionPanel();
    updateDistortionSolutionPanel();
    updateLocalAlignmentState();
    updateLocalMagnificationSolutionPanel();
    updateLocalDistortionSolutionPanel();
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
      badParameter = ltfNSurfaceAnalysis.getLabel();
      params.setNSurfaceAnalysis(ltfNSurfaceAnalysis.getText());

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
      badParameter = ltfAdditionalViewGroups.getLabel();
      params.setAdditionalViewGroups(ltfAdditionalViewGroups.getText());

      badParameter = ltfTiltAngleOffset.getLabel();
      params.setTiltAngleOffset(ltfTiltAngleOffset.getText());

      badParameter = ltfTiltAxisZShift.getLabel();
      params.setTiltAxisZShift(ltfTiltAxisZShift.getText());

      /*      badParameter = ltfTiltAxisXShift.getLabel();
            params.setTiltAxisXShift(ltfTiltAxisXShift.getText());
      */
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
      badParameter = ltfCompressionReferenceView.getLabel();
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
      // Distortion pane
      type = 0;
      if (chkDistortion.isSelected()) {
        params.setDistortionSolutionType(defaultDistortionType);
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
      //  FIXME this only works if 0 and 5 are valid local tilt angle codes
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

  void setVisible(boolean state) {
    panelGeneral.setVisible(state);
  }

  void setAdvanced(boolean state) {
    ltfMagnificationReferenceView.setVisible(state);
  }

  void setTiltAndMagnificationDefaults() {
    rbTiltAngleAll.setSelected(true);
    chkDistortion.setSelected(false);
    chkLocalDistortion.setSelected(false);
    updateEnabled();
  }

  void setDistortionDefaults() {
    rbTiltAngleAutomap.setSelected(true);
    ltfTiltAngleGroupSize.setText(defaultTiltAngleGroupSize);

    chkDistortion.setSelected(true);
    chkLocalDistortion.setSelected(true);
    //  FIXME: what to do about these values, are they always available
    ltfXstretchGroupSize.setText(defaultXStretchGroupSize);
    ltfSkewGroupSize.setText(defaultSkewGroupSize);
    ltfLocalXstretchGroupSize.setText(defaultLocalXStretchGroupSize);
    ltfLocalSkewGroupSize.setText(defaultLocalSkewGroupSize);

    updateEnabled();
  }

  void setLargestTab() {
    tabPane.setSelectedComponent(panelGlobalVariable);
    //tabPane.setSelectedComponent(panelLocalSolution);
  }

  void setFirstTab() {
    tabPane.setSelectedComponent(panelGeneral);
  }

  Container getContainer() {
    return tabPane;
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "tiltalign" };
    String[] manPage = { "tiltalign.html" };
    String[] logFileLabel = { "align" };
    String[] logFile = new String[1];
    logFile[0] = "align" + logSuffix + ".log";
    ContextPopup contextPopup =
      new ContextPopup(
        panelGeneral,
        mouseEvent,
        "FINAL ALIGNMENT",
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
  }

  //  Local alignment state
  void updateLocalAlignmentState() {
    boolean state = chkLocalAlignments.isSelected();
    ltfNLocalPatches.setEnabled(state);
    ltfMinLocalPatchSize.setEnabled(state);
    ltfMinLocalFiducials.setEnabled(state);
    tabPane.setEnabledAt(tabPane.indexOfComponent(panelLocalSolution), state);
  }

  //  Soluition panel update methods
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

  void updateCompressionSolutionPanel() {
    boolean state =
      rbCompressionAutomapLinear.isSelected()
        || rbCompressionAutomapFixed.isSelected();
    ltfCompressionGroupSize.setEnabled(state);
    ltfCompressionAdditionalGroups.setEnabled(state);
  }

  void updateDistortionSolutionPanel() {

    //  Xstretch panel state
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

    panelGeneral.setLayout(new BoxLayout(panelGeneral, BoxLayout.Y_AXIS));

    ltfNSurfaceAnalysis.setMaximumSize(dimLTF);
    panelGeneral.add(ltfNSurfaceAnalysis.getContainer());
    panelGeneral.add(Box.createRigidArea(FixedDim.x0_y5));

    ltfExcludeList.setMaximumSize(dimLTF);
    panelGeneral.add(ltfExcludeList.getContainer());
    panelGeneral.add(Box.createRigidArea(FixedDim.x0_y5));

    ltfAdditionalViewGroups.setMaximumSize(dimLTF);
    panelGeneral.add(ltfAdditionalViewGroups.getContainer());
    panelGeneral.add(Box.createRigidArea(FixedDim.x0_y5));

    ltfTiltAngleOffset.setMaximumSize(dimLTF);
    panelGeneral.add(ltfTiltAngleOffset.getContainer());

    ltfTiltAxisZShift.setMaximumSize(dimLTF);
    panelGeneral.add(ltfTiltAxisZShift.getContainer());
    panelGeneral.add(Box.createRigidArea(FixedDim.x0_y5));

    ltfTiltAxisZShift.setMaximumSize(dimLTF);
    panelGeneral.add(ltfTiltAxisZShift.getContainer());
    panelGeneral.add(Box.createRigidArea(FixedDim.x0_y5));

    panelGeneral.add(chkLocalAlignments);
    chkLocalAlignments.setAlignmentX(1.0F);
    panelGeneral.add(Box.createRigidArea(FixedDim.x0_y5));
    LocalAlignmentsListener localAlignmentsListener =
      new LocalAlignmentsListener(this);
    chkLocalAlignments.addActionListener(localAlignmentsListener);

    ltfNLocalPatches.setMaximumSize(dimLTF);
    panelGeneral.add(ltfNLocalPatches.getContainer());
    panelGeneral.add(Box.createRigidArea(FixedDim.x0_y5));

    ltfMinLocalPatchSize.setMaximumSize(dimLTF);
    panelGeneral.add(ltfMinLocalPatchSize.getContainer());
    panelGeneral.add(Box.createRigidArea(FixedDim.x0_y5));

    ltfMinLocalFiducials.setMaximumSize(dimLTF);
    panelGeneral.add(ltfMinLocalFiducials.getContainer());

    panelGeneral.add(Box.createVerticalGlue());

    tabPane.addTab("General", panelGeneral);
  }

  /**
   * Layout the global estimate tab
   */
  private void createGlobalSolutionTab() {
    panelGlobalVariable.setLayout(
      new BoxLayout(panelGlobalVariable, BoxLayout.Y_AXIS));

    //  Layout the global tilt angle estimate pane
    panelTiltAngleSolution.setLayout(
      new BoxLayout(panelTiltAngleSolution, BoxLayout.Y_AXIS));

    JRadioButton[] items = new JRadioButton[3];
    items[0] = rbTiltAngleFixed;
    items[1] = rbTiltAngleAll;
    items[2] = rbTiltAngleAutomap;
    TiltAngleRadioListener tiltAngleRadioListener =
      new TiltAngleRadioListener(this);
    createRadioBox(
      panelTiltAngleSolution,
      bgTiltAngleSolution,
      items,
      tiltAngleRadioListener);

    ltfTiltAngleGroupSize.setMaximumSize(dimLTF);
    panelTiltAngleSolution.add(ltfTiltAngleGroupSize.getContainer());
    panelTiltAngleSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    ltfTiltAngleAdditionalGroups.setMaximumSize(dimLTF);
    panelTiltAngleSolution.add(ltfTiltAngleAdditionalGroups.getContainer());

    panelTiltAngleSolution
      .setBorder(
        new TitledBorder(
          BorderFactory.createEtchedBorder(highlight, shadow),
          "Tilt angle solution type"));

    //  Layout the global magnification variable parameters
    panelMagnificationSolution.setLayout(
      new BoxLayout(panelMagnificationSolution, BoxLayout.Y_AXIS));
    items = new JRadioButton[3];
    items[0] = rbMagnificationFixed;
    items[1] = rbMagnificationAll;
    items[2] = rbMagnificationAutomap;
    MagnificationRadioListener magnificationRadioListener =
      new MagnificationRadioListener(this);
    createRadioBox(
      panelMagnificationSolution,
      bgMagnificationSolution,
      items,
      magnificationRadioListener);

    ltfMagnificationReferenceView.setMaximumSize(dimLTF);
    panelMagnificationSolution.add(
      ltfMagnificationReferenceView.getContainer());

    ltfMagnificationGroupSize.setMaximumSize(dimLTF);
    panelMagnificationSolution.add(ltfMagnificationGroupSize.getContainer());
    panelMagnificationSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    ltfMagnificationAdditionalGroups.setMaximumSize(dimLTF);
    panelMagnificationSolution.add(
      ltfMagnificationAdditionalGroups.getContainer());
    panelMagnificationSolution
      .setBorder(
        new TitledBorder(
          BorderFactory.createEtchedBorder(highlight, shadow),
          "Magnification solution type"));
          
    // Layout the global distortion pane
    createVariablePanel(
      panelDistortionSolution,
      chkDistortion,
      dimLTF,
      ltfXstretchGroupSize,
      ltfXstretchAdditionalGroups);

    ltfSkewGroupSize.setMaximumSize(dimLTF);
    panelDistortionSolution.add(ltfSkewGroupSize.getContainer());
    ltfSkewAdditionalGroups.setMaximumSize(dimLTF);
    panelDistortionSolution.add(ltfSkewAdditionalGroups.getContainer());

    DistortionCheckListener DistortionCheckListener =
      new DistortionCheckListener(this);
    chkDistortion.addActionListener(DistortionCheckListener);

    //  Add the individual panes to the tab
    panelGlobalVariable.add(panelTiltAngleSolution);
    panelGlobalVariable.add(Box.createVerticalGlue());
    panelGlobalVariable.add(panelMagnificationSolution);
    panelGlobalVariable.add(Box.createVerticalGlue());
    panelGlobalVariable.add(panelDistortionSolution);

    tabPane.addTab("Global variables", panelGlobalVariable);

  }

  /*  private void createCompressionTab() {
      //  Compression solution
      ltfCompressionReferenceView.setMaximumSize(dimLTF);
      panelCompressionSolution.add(ltfCompressionReferenceView.getContainer());
      JRadioButton[] items = new JRadioButton[3];
      items[0] = rbCompressionAll;
      items[1] = rbCompressionAutomapLinear;
      items[2] = rbCompressionAutomapFixed;
      CompressionRadioListener compressionRadioListener =
        new CompressionRadioListener(this);
      createRadioBox(
        panelCompressionSolution,
        bgCompressionSolution,
        items,
        "Compression solution type",
        compressionRadioListener);
  
      ltfCompressionGroupSize.setMaximumSize(dimLTF);
      panelCompressionSolution.add(ltfCompressionGroupSize.getContainer());
      ltfCompressionAdditionalGroups.setMaximumSize(dimLTF);
      panelCompressionSolution.add(ltfCompressionAdditionalGroups.getContainer());
      panelCompressionSolution.add(Box.createRigidArea(FixedDim.x0_y5));
      panelCompressionSolution.add(Box.createVerticalGlue());
  
    }
  */

  private void createLocalSolutionTab() {
    //  Construct the local solution panel
    panelLocalSolution.setLayout(
      new BoxLayout(panelLocalSolution, BoxLayout.Y_AXIS));
    //panelLocalSolution.setPreferredSize(new Dimension(400, 350));

    //  Construct the rotation solution objects
    createVariablePanel(
      panelLocalRotationSolution,
      chkLocalRotation,
      dimLTF,
      ltfLocalRotationGroupSize,
      ltfLocalRotationAdditionalGroups);
    LocalRotationCheckListener localRotationCheckListener =
      new LocalRotationCheckListener(this);
    chkLocalRotation.addActionListener(localRotationCheckListener);

    //  Construct the tilt angle solution objects
    createVariablePanel(
      panelLocalTiltAngleSolution,
      chkLocalTiltAngle,
      dimLTF,
      ltfLocalTiltAngleGroupSize,
      ltfLocalTiltAngleAdditionalGroups);

    LocalTiltAngleCheckListener localTiltAngleCheckListener =
      new LocalTiltAngleCheckListener(this);
    chkLocalTiltAngle.addActionListener(localTiltAngleCheckListener);

    //  Construct the local magnification pane
    createVariablePanel(
      panelLocalMagnificationSolution,
      chkLocalMagnification,
      dimLTF,
      ltfLocalMagnificationGroupSize,
      ltfLocalMagnificationAdditionalGroups);

    LocalMagnificationCheckListener localMagnificationCheckListener =
      new LocalMagnificationCheckListener(this);
    chkLocalMagnification.addActionListener(localMagnificationCheckListener);

    //  Construction the local distortion pane
    createVariablePanel(
      panelLocalDistortionSolution,
      chkLocalDistortion,
      dimLTF,
      ltfLocalXstretchGroupSize,
      ltfLocalXstretchAdditionalGroups);

    ltfLocalSkewGroupSize.setMaximumSize(dimLTF);
    panelLocalDistortionSolution.add(ltfLocalSkewGroupSize.getContainer());
    ltfLocalSkewAdditionalGroups.setMaximumSize(dimLTF);
    panelLocalDistortionSolution.add(
      ltfLocalSkewAdditionalGroups.getContainer());

    LocalDistortionCheckListener localDistortionCheckListener =
      new LocalDistortionCheckListener(this);
    chkLocalDistortion.addActionListener(localDistortionCheckListener);

    panelLocalSolution.add(panelLocalRotationSolution);
    panelLocalSolution.add(Box.createVerticalGlue());
    panelLocalSolution.add(panelLocalTiltAngleSolution);
    panelLocalSolution.add(Box.createVerticalGlue());
    panelLocalSolution.add(panelLocalMagnificationSolution);
    panelLocalSolution.add(Box.createVerticalGlue());
    panelLocalSolution.add(panelLocalDistortionSolution);

    tabPane.addTab("Local Variables", panelLocalSolution);
  }

  private void createVariablePanel(
    JPanel panel,
    JCheckBox checkBox,
    Dimension dimLTF,
    LabeledTextField groupSize,
    LabeledTextField additionalGroups) {

    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

    //  Add the check box
    panel.add(checkBox);

    //  Add the group size labeled text field
    groupSize.setMaximumSize(dimLTF);
    panel.add(groupSize.getContainer());

    //  Add the additional groups labeled text field
    additionalGroups.setMaximumSize(dimLTF);
    panel.add(additionalGroups.getContainer());

    panel.add(Box.createRigidArea(FixedDim.x0_y5));

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
    panel.updateDistortionSolutionPanel();
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
    panel.updateLocalDistortionSolutionPanel();
  }
}

class CompressionRadioListener implements ActionListener {
  TiltalignPanel panel;

  CompressionRadioListener(TiltalignPanel adaptee) {
    panel = adaptee;
  }
  public void actionPerformed(ActionEvent event) {
    panel.updateCompressionSolutionPanel();
  }
}
