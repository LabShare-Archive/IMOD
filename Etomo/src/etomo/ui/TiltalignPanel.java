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

  private JTabbedPane tabPane = new JTabbedPane();

  //  General pane
  private JPanel panelGeneral = new JPanel();
  private JPanel panelAdvanced = new JPanel();

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
  private LabeledTextField ltfTiltAxisXShift =
    new LabeledTextField("Tilt axis x shift: ");
  private JCheckBox chkLocalAlignments =
    new JCheckBox("Enable local alignments");
  private LabeledTextField ltfNLocalPatches =
    new LabeledTextField("# of local patches (x,y): ");
  private LabeledTextField ltfMinLocalPatchSize =
    new LabeledTextField("Min. local patch size (x,y): ");
  private LabeledTextField ltfMinLocalFiducials =
    new LabeledTextField("Min.# of fiducials (total, each surface): ");

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

  //  Local tilt angle pane
  private JRadioButton rbLocalTiltAngleFixed =
    new JRadioButton("Fixed tilt angles");
  private JRadioButton rbLocalTiltAngleAll =
    new JRadioButton("Solve for all except minimum tilt");
  private JRadioButton rbLocalTiltAngleAutomap =
    new JRadioButton("Group tilt angles (linear)");
  private ButtonGroup bgLocalTiltAngleSolution = new ButtonGroup();
  private JPanel panelLocalTiltAngleSolution = new JPanel();

  private LabeledTextField ltfLocalTiltAngleGroupSize =
    new LabeledTextField("Tilt angle group size: ");
  private LabeledTextField ltfLocalTiltAngleAdditionalGroups =
    new LabeledTextField("Tilt angle additional group list: ");

  //  Magnfication pane
  private JRadioButton rbMagnificationFixed =
    new JRadioButton("Fixed magnification at 1.0");
  private JRadioButton rbMagnificationAll =
    new JRadioButton("Solve for all magnifications");
  private JRadioButton rbMagnificationAutomapLinear =
    new JRadioButton("Group magnification (first order fit)");
  private JRadioButton rbMagnificationAutomapFixed =
    new JRadioButton("Group magnification (zeroth order fit)");
  private ButtonGroup bgMagnificationSolution = new ButtonGroup();
  private JPanel panelMagnificationSolution = new JPanel();

  private LabeledTextField ltfMagnificationReferenceView =
    new LabeledTextField("Magnification reference view: ");
  private LabeledTextField ltfMagnificationGroupSize =
    new LabeledTextField("Magnification group size: ");
  private LabeledTextField ltfMagnificationAdditionalGroups =
    new LabeledTextField("Magnification additional group list: ");

  // Local magnfication pane
  private JRadioButton rbLocalMagnificationFixed =
    new JRadioButton("Fixed LocalMagnification at 1.0");
  private JRadioButton rbLocalMagnificationAll =
    new JRadioButton("Solve for all LocalMagnifications");
  private JRadioButton rbLocalMagnificationAutomapLinear =
    new JRadioButton("Group LocalMagnification (first order fit)");
  private JRadioButton rbLocalMagnificationAutomapFixed =
    new JRadioButton("Group LocalMagnification (zeroth order fit)");
  private ButtonGroup bgLocalMagnificationSolution = new ButtonGroup();
  private JPanel panelLocalMagnificationSolution = new JPanel();

  private LabeledTextField ltfLocalMagnificationReferenceView =
    new LabeledTextField("LocalMagnification reference view: ");
  private LabeledTextField ltfLocalMagnificationGroupSize =
    new LabeledTextField("LocalMagnification group size: ");
  private LabeledTextField ltfLocalMagnificationAdditionalGroups =
    new LabeledTextField("LocalMagnification additional group list: ");

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

  // Distortion pane
  private JRadioButton rbDistortionNone =
    new JRadioButton("No distortion solution");
  private JRadioButton rbDistortionCombined =
    new JRadioButton("X-stretch and skew with the same parameters");
  private JRadioButton rbDistortionIndependent =
    new JRadioButton("Independent X-stretch and skew parameters");
  private ButtonGroup bgDistortionSolution = new ButtonGroup();
  private JPanel panelDistortionSolution = new JPanel();

  private JRadioButton rbXstretchAll =
    new JRadioButton("Independently for each tilt");
  JRadioButton rbXstretchAutomapLinear =
    new JRadioButton("Group x-stretch (first order fit)");
  private JRadioButton rbXstretchAutomapFixed =
    new JRadioButton("Group x-stretch (zeroth order fit)");
  private ButtonGroup bgXstretchSolution = new ButtonGroup();
  private JPanel panelXstretchSolution = new JPanel();

  private LabeledTextField ltfXstretchGroupSize =
    new LabeledTextField("X-stretch group size: ");
  private LabeledTextField ltfXstretchAdditionalGroups =
    new LabeledTextField("X-stretch additional group list: ");

  private JRadioButton rbSkewAll =
    new JRadioButton("Independently for each tilt");
  private JRadioButton rbSkewAutomapLinear =
    new JRadioButton("Group skew (first order fit)");
  private JRadioButton rbSkewAutomapFixed =
    new JRadioButton("Group skew (zeroth order fit)");
  private ButtonGroup bgSkewSolution = new ButtonGroup();
  private JPanel panelSkewSolution = new JPanel();

  private LabeledTextField ltfSkewGroupSize =
    new LabeledTextField("Skew group size: ");
  private LabeledTextField ltfSkewAdditionalGroups =
    new LabeledTextField("Skew additional group list: ");

  //  Local distortion pane
  private JRadioButton rbLocalDistortionNone =
    new JRadioButton("No Local Distortion solution");
  private JRadioButton rbLocalDistortionCombined =
    new JRadioButton("X-stretch and skew with the same parameters");
  private JRadioButton rbLocalDistortionIndependent =
    new JRadioButton("Independent X-stretch and skew parameters");
  private ButtonGroup bgLocalDistortionSolution = new ButtonGroup();
  private JPanel panelLocalDistortionSolution = new JPanel();

  private JRadioButton rbLocalXstretchAll =
    new JRadioButton("Independently for each tilt");
  private JRadioButton rbLocalXstretchAutomapLinear =
    new JRadioButton("Group x-stretch (first order fit)");
  private JRadioButton rbLocalXstretchAutomapFixed =
    new JRadioButton("Group x-stretch (zeroth order fit)");
  private ButtonGroup bgLocalXstretchSolution = new ButtonGroup();
  private JPanel panelLocalXstretchSolution = new JPanel();

  private LabeledTextField ltfLocalXstretchGroupSize =
    new LabeledTextField("X-stretch group size: ");
  private LabeledTextField ltfLocalXstretchAdditionalGroups =
    new LabeledTextField("X-stretch additional group list: ");

  private JRadioButton rbLocalSkewAll =
    new JRadioButton("Independently for each tilt");
  private JRadioButton rbLocalSkewAutomapLinear =
    new JRadioButton("Group LocalSkew (first order fit)");
  private JRadioButton rbLocalSkewAutomapFixed =
    new JRadioButton("Group LocalSkew (zeroth order fit)");
  private ButtonGroup bgLocalSkewSolution = new ButtonGroup();
  private JPanel panelLocalSkewSolution = new JPanel();

  private LabeledTextField ltfLocalSkewGroupSize =
    new LabeledTextField("LocalSkew group size: ");
  private LabeledTextField ltfLocalSkewAdditionalGroups =
    new LabeledTextField("LocalSkew additional group list: ");

  private Dimension dimLTF = new Dimension(10000, 30);

  public TiltalignPanel(String suffix) {
    logSuffix = suffix;

    //  Create the tabs
    createGeneralTab();
    createTiltAngleTab();
    createMagnificationTab();
    createCompressionTab();
    createDistortionTab();
    createLocalTiltAngleTab();
    createLocalMagnificationTab();
    createLocalDistortionTab();

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
    ltfTiltAxisXShift.setText(params.getTiltAxisXShift());
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

    // Local tilt angle solution parameters
    solutionType = params.getLocalTiltSolutionType();
    if (solutionType == 0) {
      rbLocalTiltAngleFixed.setSelected(true);
    }
    if (solutionType == 2) {
      rbLocalTiltAngleAll.setSelected(true);
    }
    if (solutionType == 5) {
      rbLocalTiltAngleAutomap.setSelected(true);
    }
    if (solutionType == 5) {
      ltfLocalTiltAngleGroupSize.setText(
        params.getLocalTiltSolutionGroupSize());
      ltfLocalTiltAngleAdditionalGroups.setText(
        params.getLocalTiltAdditionalGroups());
    }

    //  Magnification solution parameters
    ltfMagnificationReferenceView.setText(
      params.getMagnificationSolutionReferenceView());
    solutionType = params.getMagnificationSolutionType();
    if (solutionType == 0) {
      rbMagnificationFixed.setSelected(true);
    }
    if (solutionType == 1) {
      rbMagnificationAll.setSelected(true);
    }
    if (solutionType == 3) {
      rbMagnificationAutomapLinear.setSelected(true);
    }
    if (solutionType == 4) {
      rbMagnificationAutomapFixed.setSelected(true);
    }

    if (solutionType > 2) {
      ltfMagnificationGroupSize.setText(
        params.getMagnificationSolutionGroupSize());
      ltfMagnificationAdditionalGroups.setText(
        params.getMagnificationSolutionAdditionalGroups());
    }

    //  Local magnification solution parameters
    ltfLocalMagnificationReferenceView.setText(
      params.getLocalMagnificationSolutionReferenceView());
    solutionType = params.getLocalMagnificationSolutionType();
    if (solutionType == 0) {
      rbLocalMagnificationFixed.setSelected(true);
    }
    if (solutionType == 1) {
      rbLocalMagnificationAll.setSelected(true);
    }
    if (solutionType == 3) {
      rbLocalMagnificationAutomapLinear.setSelected(true);
    }
    if (solutionType == 4) {
      rbLocalMagnificationAutomapFixed.setSelected(true);
    }

    if (solutionType > 2) {
      ltfLocalMagnificationGroupSize.setText(
        params.getLocalMagnificationSolutionGroupSize());
      ltfLocalMagnificationAdditionalGroups.setText(
        params.getLocalMagnificationSolutionAdditionalGroups());
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

    //  Distortion solution type
    solutionType = params.getDistortionSolutionType();
    if (solutionType == 0) {
      rbDistortionNone.setSelected(true);
    }
    if (solutionType == 1) {
      rbDistortionCombined.setSelected(true);
    }
    if (solutionType == 2) {
      rbDistortionIndependent.setSelected(true);
    }

    //  X-stretch solution parameters
    solutionType = params.getXstretchSolutionType();
    if (solutionType == 1) {
      rbXstretchAll.setSelected(true);
    }
    if (solutionType == 3) {
      rbXstretchAutomapLinear.setSelected(true);
    }
    if (solutionType == 4) {
      rbXstretchAutomapFixed.setSelected(true);
    }
    if (solutionType > 2) {
      ltfXstretchGroupSize.setText(params.getXstretchSolutionGroupSize());
      ltfXstretchAdditionalGroups.setText(
        params.getXstretchSolutionAdditionalGroups());
    }

    //  Skew solution parameters
    solutionType = params.getSkewSolutionType();
    if (solutionType == 1) {
      rbSkewAll.setSelected(true);
    }
    if (solutionType == 3) {
      rbSkewAutomapLinear.setSelected(true);
    }
    if (solutionType == 4) {
      rbSkewAutomapFixed.setSelected(true);
    }
    if (solutionType > 2) {
      ltfSkewGroupSize.setText(params.getSkewSolutionGroupSize());
      ltfSkewAdditionalGroups.setText(params.getSkewSolutionAdditionalGroups());
    }

    //  Local distortion solution type
    solutionType = params.getLocalDistortionSolutionType();
    if (solutionType == 0) {
      rbLocalDistortionNone.setSelected(true);
    }
    if (solutionType == 1) {
      rbLocalDistortionCombined.setSelected(true);
    }
    if (solutionType == 2) {
      rbLocalDistortionIndependent.setSelected(true);
    }

    //  local x-stretch solution parameters
    solutionType = params.getLocalXstretchSolutionType();
    if (solutionType == 1) {
      rbLocalXstretchAll.setSelected(true);
    }
    if (solutionType == 3) {
      rbLocalXstretchAutomapLinear.setSelected(true);
    }
    if (solutionType == 4) {
      rbLocalXstretchAutomapFixed.setSelected(true);
    }
    if (solutionType > 2) {
      ltfLocalXstretchGroupSize.setText(
        params.getLocalXstretchSolutionGroupSize());
      ltfLocalXstretchAdditionalGroups.setText(
        params.getLocalXstretchSolutionAdditionalGroups());
    }

    //  Local skew solution parameters
    solutionType = params.getLocalSkewSolutionType();
    if (solutionType == 1) {
      rbLocalSkewAll.setSelected(true);
    }
    if (solutionType == 3) {
      rbLocalSkewAutomapLinear.setSelected(true);
    }
    if (solutionType == 4) {
      rbLocalSkewAutomapFixed.setSelected(true);
    }
    if (solutionType > 2) {
      ltfLocalSkewGroupSize.setText(params.getLocalSkewSolutionGroupSize());
      ltfLocalSkewAdditionalGroups.setText(
        params.getLocalSkewSolutionAdditionalGroups());
    }

    updateEnabled();
  }

  public void updateEnabled() {
    //  update all of the enable/disable states
    updateTiltAngleSolutionPanel(null);
    updateMagnificationSolutionPanel(null);
    updateCompressionSolutionPanel(null);
    updateDistortionSolutionPanel(null);
    updateXstretchSolutionPanel(null);
    updateSkewSolutionPanel(null);
    updateLocalAlignmentState(null);
    updateLocalAlignmentState(null);
    updateLocalMagnificationSolutionPanel(null);
    updateLocalDistortionSolutionPanel(null);
    updateLocalXstretchSolutionPanel(null);
    updateLocalSkewSolutionPanel(null);
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

      badParameter = ltfTiltAxisXShift.getLabel();
      params.setTiltAxisXShift(ltfTiltAxisXShift.getText());

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
      if (rbMagnificationAutomapLinear.isSelected())
        type = 3;
      if (rbMagnificationAutomapFixed.isSelected())
        type = 4;
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
      if (rbDistortionNone.isSelected())
        type = 0;
      if (rbDistortionCombined.isSelected())
        type = 1;
      if (rbDistortionIndependent.isSelected())
        type = 2;
      params.setDistortionSolutionType(type);

      if (type > 0) {
        if (rbXstretchAll.isSelected())
          type = 1;
        if (rbXstretchAutomapLinear.isSelected())
          type = 3;
        if (rbXstretchAutomapFixed.isSelected())
          type = 4;
        params.setXstretchType(type);

        if (type > 2) {
          badParameter = ltfXstretchGroupSize.getLabel();
          params.setXstretchSolutionGroupSize(ltfXstretchGroupSize.getText());

          badParameter = ltfXstretchAdditionalGroups.getLabel();
          params.setXstretchSolutionAdditionalGroups(
            ltfXstretchAdditionalGroups.getText());
        }

        if (rbSkewAll.isSelected())
          type = 1;
        if (rbSkewAutomapLinear.isSelected())
          type = 3;
        if (rbSkewAutomapFixed.isSelected())
          type = 4;
        params.setSkewType(type);

        if (type > 2) {
          badParameter = ltfSkewGroupSize.getLabel();
          params.setSkewSolutionGroupSize(ltfSkewGroupSize.getText());

          badParameter = ltfSkewAdditionalGroups.getLabel();
          params.setSkewSolutionAdditionalGroups(
            ltfSkewAdditionalGroups.getText());
        }
      }

      //  Get the local alignment parameters
      // Tilt angle pane
      type = 0;
      if (rbLocalTiltAngleFixed.isSelected())
        type = 0;
      if (rbLocalTiltAngleAll.isSelected())
        type = 2;
      if (rbLocalTiltAngleAutomap.isSelected())
        type = 5;
      params.setLocalTiltSolutionType(type);
      if (type > 2) {
        badParameter = ltfLocalTiltAngleGroupSize.getLabel();
        params.setLocalTiltSolutionGroupSize(
          ltfLocalTiltAngleGroupSize.getText());

        badParameter = ltfLocalTiltAngleAdditionalGroups.getLabel();
        params.setLocalTiltSolutionAdditionalGroups(
          ltfLocalTiltAngleAdditionalGroups.getText());
      }

      // Magnification pane
      badParameter = ltfLocalMagnificationReferenceView.getLabel();
      params.setLocalMagnificationReferenceView(
        ltfLocalMagnificationReferenceView.getText());

      if (rbLocalMagnificationFixed.isSelected())
        type = 0;
      if (rbLocalMagnificationAll.isSelected())
        type = 1;
      if (rbLocalMagnificationAutomapLinear.isSelected())
        type = 3;
      if (rbLocalMagnificationAutomapFixed.isSelected())
        type = 4;
      params.setLocalMagnificationType(type);

      if (type > 2) {
        badParameter = ltfLocalMagnificationGroupSize.getLabel();
        params.setLocalMagnificationSolutionGroupSize(
          ltfLocalMagnificationGroupSize.getText());

        badParameter = ltfLocalMagnificationAdditionalGroups.getLabel();
        params.setLocalMagnificationSolutionAdditionalGroups(
          ltfLocalMagnificationAdditionalGroups.getText());
      }

      // Distortion pane
      if (rbLocalDistortionNone.isSelected())
        type = 0;
      if (rbLocalDistortionCombined.isSelected())
        type = 1;
      if (rbLocalDistortionIndependent.isSelected())
        type = 2;
      params.setLocalDistortionSolutionType(type);

      if (type > 0) {
        if (rbLocalXstretchAll.isSelected())
          type = 1;
        if (rbLocalXstretchAutomapLinear.isSelected())
          type = 3;
        if (rbLocalXstretchAutomapFixed.isSelected())
          type = 4;
        params.setLocalXstretchType(type);

        if (type > 2) {
          badParameter = ltfLocalXstretchGroupSize.getLabel();
          params.setLocalXstretchSolutionGroupSize(
            ltfLocalXstretchGroupSize.getText());

          badParameter = ltfLocalXstretchAdditionalGroups.getLabel();
          params.setLocalXstretchSolutionAdditionalGroups(
            ltfLocalXstretchAdditionalGroups.getText());
        }

        if (rbLocalSkewAll.isSelected())
          type = 1;
        if (rbLocalSkewAutomapLinear.isSelected())
          type = 3;
        if (rbLocalSkewAutomapFixed.isSelected())
          type = 4;
        params.setLocalSkewType(type);

        if (type > 2) {
          badParameter = ltfLocalSkewGroupSize.getLabel();
          params.setLocalSkewSolutionGroupSize(ltfLocalSkewGroupSize.getText());

          badParameter = ltfLocalSkewAdditionalGroups.getLabel();
          params.setLocalSkewSolutionAdditionalGroups(
            ltfLocalSkewAdditionalGroups.getText());
        }
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
    panelAdvanced.setVisible(state);
  }

  void setTiltAndMagnificationDefaults() {
    rbTiltAngleAll.setSelected(true);
    rbDistortionNone.setSelected(true);
    rbLocalDistortionNone.setSelected(true);
    updateEnabled();
  }

  void setDistortionDefaults() {
    rbTiltAngleAutomap.setSelected(true);
    ltfTiltAngleGroupSize.setText(10);

    rbDistortionIndependent.setSelected(true);
    rbXstretchAutomapLinear.setSelected(true);
    ltfXstretchGroupSize.setText(7);
    rbSkewAutomapLinear.setSelected(true);
    ltfSkewGroupSize.setText(11);

    rbLocalDistortionIndependent.setSelected(true);
    rbLocalXstretchAutomapLinear.setSelected(true);
    ltfLocalXstretchGroupSize.setText(7);
    rbLocalSkewAutomapLinear.setSelected(true);
    ltfLocalSkewGroupSize.setText(11);

    updateEnabled();
  }

  void setLargestTab() {
    tabPane.setSelectedComponent(panelDistortionSolution);
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
  void updateLocalAlignmentState(ActionEvent event) {
    boolean state = chkLocalAlignments.isSelected();
    ltfNLocalPatches.setEnabled(state);
    ltfMinLocalPatchSize.setEnabled(state);
    ltfMinLocalFiducials.setEnabled(state);
    tabPane.setEnabledAt(
      tabPane.indexOfComponent(panelLocalTiltAngleSolution),
      state);
    tabPane.setEnabledAt(
      tabPane.indexOfComponent(panelLocalMagnificationSolution),
      state);
    tabPane.setEnabledAt(
      tabPane.indexOfComponent(panelLocalDistortionSolution),
      state);

  }
  //  Soluition panel update methods
  void updateTiltAngleSolutionPanel(ActionEvent event) {
    boolean state = rbTiltAngleAutomap.isSelected();
    ltfTiltAngleGroupSize.setEnabled(state);
    ltfTiltAngleAdditionalGroups.setEnabled(state);
  }

  void updateLocalTiltAngleSolutionPanel(ActionEvent event) {
    boolean state = rbLocalTiltAngleAutomap.isSelected();
    ltfLocalTiltAngleGroupSize.setEnabled(state);
    ltfLocalTiltAngleAdditionalGroups.setEnabled(state);
  }

  void updateMagnificationSolutionPanel(ActionEvent event) {
    boolean state =
      rbMagnificationAutomapLinear.isSelected()
        || rbMagnificationAutomapFixed.isSelected();
    ltfMagnificationGroupSize.setEnabled(state);
    ltfMagnificationAdditionalGroups.setEnabled(state);
  }

  void updateLocalMagnificationSolutionPanel(ActionEvent event) {
    boolean state =
      rbLocalMagnificationAutomapLinear.isSelected()
        || rbLocalMagnificationAutomapFixed.isSelected();
    ltfLocalMagnificationGroupSize.setEnabled(state);
    ltfLocalMagnificationAdditionalGroups.setEnabled(state);
  }

  void updateCompressionSolutionPanel(ActionEvent event) {
    boolean state =
      rbCompressionAutomapLinear.isSelected()
        || rbCompressionAutomapFixed.isSelected();
    ltfCompressionGroupSize.setEnabled(state);
    ltfCompressionAdditionalGroups.setEnabled(state);
  }

  void updateDistortionSolutionPanel(ActionEvent event) {

    //  Xstretch panel state
    boolean state =
      rbDistortionCombined.isSelected() || rbDistortionIndependent.isSelected();
    panelXstretchSolution.setEnabled(state);
    rbXstretchAll.setEnabled(state);
    rbXstretchAutomapLinear.setEnabled(state);
    rbXstretchAutomapFixed.setEnabled(state);

    // Skew panel state
    state = rbDistortionIndependent.isSelected();
    panelSkewSolution.setEnabled(state);
    rbSkewAll.setEnabled(state);
    rbSkewAutomapFixed.setEnabled(state);
    rbSkewAutomapLinear.setEnabled(state);

    updateXstretchSolutionPanel(null);
    updateSkewSolutionPanel(null);
  }

  void updateLocalDistortionSolutionPanel(ActionEvent event) {

    //  Xstretch panel state
    boolean state =
      rbLocalDistortionCombined.isSelected()
        || rbLocalDistortionIndependent.isSelected();
    panelLocalXstretchSolution.setEnabled(state);
    rbLocalXstretchAll.setEnabled(state);
    rbLocalXstretchAutomapLinear.setEnabled(state);
    rbLocalXstretchAutomapFixed.setEnabled(state);

    // Skew panel state
    state = rbLocalDistortionIndependent.isSelected();
    panelLocalSkewSolution.setEnabled(state);
    rbLocalSkewAll.setEnabled(state);
    rbLocalSkewAutomapFixed.setEnabled(state);
    rbLocalSkewAutomapLinear.setEnabled(state);

    updateLocalXstretchSolutionPanel(null);
    updateLocalSkewSolutionPanel(null);
  }

  void updateXstretchSolutionPanel(ActionEvent event) {
    boolean state =
      (!rbXstretchAll.isSelected()) && (!rbDistortionNone.isSelected());
    ltfXstretchGroupSize.setEnabled(state);
    ltfXstretchAdditionalGroups.setEnabled(state);
  }

  void updateLocalXstretchSolutionPanel(ActionEvent event) {
    boolean state =
      (!rbLocalXstretchAll.isSelected())
        && (!rbLocalDistortionNone.isSelected());
    ltfLocalXstretchGroupSize.setEnabled(state);
    ltfLocalXstretchAdditionalGroups.setEnabled(state);
  }

  void updateSkewSolutionPanel(ActionEvent event) {
    boolean state =
      (!rbSkewAll.isSelected()) && rbDistortionIndependent.isSelected();
    ltfSkewGroupSize.setEnabled(state);
    ltfSkewAdditionalGroups.setEnabled(state);
  }

  void updateLocalSkewSolutionPanel(ActionEvent event) {
    boolean state =
      (!rbLocalSkewAll.isSelected())
        && rbLocalDistortionIndependent.isSelected();
    ltfLocalSkewGroupSize.setEnabled(state);
    ltfLocalSkewAdditionalGroups.setEnabled(state);
  }

  private void createRadioBox(
    JPanel panel,
    ButtonGroup group,
    JRadioButton[] items,
    String title,
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
      //items[i].setAlignmentX(1.0f);
    }

    // Set the preferred size, layout and border
    int height = items.length * 30 + 12;
    Dimension dimBoxSize = new Dimension(width, height);
    panel.setPreferredSize(dimBoxSize);
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

    TitledBorder border =
      new TitledBorder(
        BorderFactory.createEtchedBorder(
          new Color(248, 254, 255),
          new Color(121, 124, 136)),
        title);
    panel.setBorder(border);
    //panel.setAlignmentX(1.0f);

  }

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

    ltfTiltAxisXShift.setMaximumSize(dimLTF);
    panelGeneral.add(ltfTiltAxisZShift.getContainer());
    panelGeneral.add(Box.createRigidArea(FixedDim.x0_y5));

    ltfTiltAxisZShift.setMaximumSize(dimLTF);
    panelGeneral.add(ltfTiltAxisXShift.getContainer());
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

  private void createTiltAngleTab() {

    //  Global tilt angle solution objects
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
      "Tilt angle solution type",
      tiltAngleRadioListener);

    ltfTiltAngleGroupSize.setMaximumSize(dimLTF);
    panelTiltAngleSolution.add(ltfTiltAngleGroupSize.getContainer());

    ltfTiltAngleAdditionalGroups.setMaximumSize(dimLTF);
    panelTiltAngleSolution.add(ltfTiltAngleAdditionalGroups.getContainer());
    panelTiltAngleSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    panelTiltAngleSolution.add(Box.createVerticalGlue());

    tabPane.addTab("Tilt Angle", panelTiltAngleSolution);
  }

  private void createLocalTiltAngleTab() {

    //  Local tilt angle solution objects
    JRadioButton[] items = new JRadioButton[3];
    items[0] = rbLocalTiltAngleFixed;
    items[1] = rbLocalTiltAngleAll;
    items[2] = rbLocalTiltAngleAutomap;
    LocalTiltAngleRadioListener LocalTiltAngleRadioListener =
      new LocalTiltAngleRadioListener(this);
    createRadioBox(
      panelLocalTiltAngleSolution,
      bgLocalTiltAngleSolution,
      items,
      "LocalTilt angle solution type",
      LocalTiltAngleRadioListener);

    ltfLocalTiltAngleGroupSize.setMaximumSize(dimLTF);
    panelLocalTiltAngleSolution.add(ltfLocalTiltAngleGroupSize.getContainer());

    ltfLocalTiltAngleAdditionalGroups.setMaximumSize(dimLTF);
    panelLocalTiltAngleSolution.add(
      ltfLocalTiltAngleAdditionalGroups.getContainer());
    panelLocalTiltAngleSolution.add(Box.createRigidArea(FixedDim.x0_y5));

    panelLocalTiltAngleSolution.add(Box.createVerticalGlue());

    tabPane.addTab("LocalTilt Angle", panelLocalTiltAngleSolution);
  }

  private void createMagnificationTab() {
    //  Magnification solution
    ltfMagnificationReferenceView.setMaximumSize(dimLTF);
    panelMagnificationSolution.add(
      ltfMagnificationReferenceView.getContainer());
    JRadioButton[] items = new JRadioButton[3];
    items = new JRadioButton[4];
    items[0] = rbMagnificationFixed;
    items[1] = rbMagnificationAll;
    items[2] = rbMagnificationAutomapLinear;
    items[3] = rbMagnificationAutomapFixed;
    MagnificationRadioListener magnificationRadioListener =
      new MagnificationRadioListener(this);
    createRadioBox(
      panelMagnificationSolution,
      bgMagnificationSolution,
      items,
      "Magnification solution type",
      magnificationRadioListener);

    ltfMagnificationGroupSize.setMaximumSize(dimLTF);
    panelMagnificationSolution.add(ltfMagnificationGroupSize.getContainer());
    ltfMagnificationAdditionalGroups.setMaximumSize(dimLTF);
    panelMagnificationSolution.add(
      ltfMagnificationAdditionalGroups.getContainer());
    panelMagnificationSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    panelMagnificationSolution.add(Box.createVerticalGlue());

    tabPane.addTab("Magnification", panelMagnificationSolution);
  }

  private void createLocalMagnificationTab() {
    //  Local Magnification solution
    ltfLocalMagnificationReferenceView.setMaximumSize(dimLTF);
    panelLocalMagnificationSolution.add(
      ltfLocalMagnificationReferenceView.getContainer());
    JRadioButton[] items = new JRadioButton[3];
    items = new JRadioButton[4];
    items[0] = rbLocalMagnificationFixed;
    items[1] = rbLocalMagnificationAll;
    items[2] = rbLocalMagnificationAutomapLinear;
    items[3] = rbLocalMagnificationAutomapFixed;
    LocalMagnificationRadioListener LocalMagnificationRadioListener =
      new LocalMagnificationRadioListener(this);
    createRadioBox(
      panelLocalMagnificationSolution,
      bgLocalMagnificationSolution,
      items,
      "Local magnification solution type",
      LocalMagnificationRadioListener);

    ltfLocalMagnificationGroupSize.setMaximumSize(dimLTF);
    panelLocalMagnificationSolution.add(
      ltfLocalMagnificationGroupSize.getContainer());
    ltfLocalMagnificationAdditionalGroups.setMaximumSize(dimLTF);
    panelLocalMagnificationSolution.add(
      ltfLocalMagnificationAdditionalGroups.getContainer());
    panelLocalMagnificationSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    panelLocalMagnificationSolution.add(Box.createVerticalGlue());

    tabPane.addTab("LocalMagnification", panelLocalMagnificationSolution);
  }

  private void createCompressionTab() {
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
    tabPane.addTab("Compression", panelCompressionSolution);

  }

  private void createDistortionTab() {
    //  Global distortion solution
    JRadioButton[] items = new JRadioButton[3];
    items[0] = rbDistortionNone;
    items[1] = rbDistortionCombined;
    items[2] = rbDistortionIndependent;
    DistortionRadioListener distortionRadioListener =
      new DistortionRadioListener(this);
    createRadioBox(
      panelDistortionSolution,
      bgDistortionSolution,
      items,
      "Distortion solution type",
      distortionRadioListener);

    items[0] = rbXstretchAll;
    items[1] = rbXstretchAutomapLinear;
    items[2] = rbXstretchAutomapFixed;
    XstretchRadioListener xstretchRadioListener =
      new XstretchRadioListener(this);
    createRadioBox(
      panelXstretchSolution,
      bgXstretchSolution,
      items,
      "X-stretch solution type",
      xstretchRadioListener);

    ltfXstretchGroupSize.setMaximumSize(dimLTF);
    panelXstretchSolution.add(ltfXstretchGroupSize.getContainer());
    ltfXstretchAdditionalGroups.setMaximumSize(dimLTF);
    panelXstretchSolution.add(ltfXstretchAdditionalGroups.getContainer());
    panelXstretchSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    panelXstretchSolution.add(Box.createVerticalGlue());

    items[0] = rbSkewAll;
    items[1] = rbSkewAutomapLinear;
    items[2] = rbSkewAutomapFixed;
    SkewRadioListener skewRadioListener = new SkewRadioListener(this);
    createRadioBox(
      panelSkewSolution,
      bgSkewSolution,
      items,
      "Skew solution type",
      skewRadioListener);

    ltfSkewGroupSize.setMaximumSize(dimLTF);
    panelSkewSolution.add(ltfSkewGroupSize.getContainer());
    ltfSkewAdditionalGroups.setMaximumSize(dimLTF);
    panelSkewSolution.add(ltfSkewAdditionalGroups.getContainer());
    panelSkewSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    panelSkewSolution.add(Box.createVerticalGlue());

    panelDistortionSolution.add(panelXstretchSolution);
    panelDistortionSolution.add(panelSkewSolution);

    tabPane.addTab("Distortion", panelDistortionSolution);

  }

  private void createLocalDistortionTab() {
    JRadioButton[] items = new JRadioButton[3];
    //  Local distortion solution
    items[0] = rbLocalDistortionNone;
    items[1] = rbLocalDistortionCombined;
    items[2] = rbLocalDistortionIndependent;
    LocalDistortionRadioListener localDistortionRadioListener =
      new LocalDistortionRadioListener(this);
    createRadioBox(
      panelLocalDistortionSolution,
      bgLocalDistortionSolution,
      items,
      "Local Distortion solution type",
      localDistortionRadioListener);

    items[0] = rbLocalXstretchAll;
    items[1] = rbLocalXstretchAutomapLinear;
    items[2] = rbLocalXstretchAutomapFixed;
    LocalXstretchRadioListener localXstretchRadioListener =
      new LocalXstretchRadioListener(this);
    createRadioBox(
      panelLocalXstretchSolution,
      bgLocalXstretchSolution,
      items,
      "Local X-stretch solution type",
      localXstretchRadioListener);

    ltfLocalXstretchGroupSize.setMaximumSize(dimLTF);
    panelLocalXstretchSolution.add(ltfLocalXstretchGroupSize.getContainer());
    ltfLocalXstretchAdditionalGroups.setMaximumSize(dimLTF);
    panelLocalXstretchSolution.add(
      ltfLocalXstretchAdditionalGroups.getContainer());
    panelLocalXstretchSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    panelLocalXstretchSolution.add(Box.createVerticalGlue());

    items[0] = rbLocalSkewAll;
    items[1] = rbLocalSkewAutomapLinear;
    items[2] = rbLocalSkewAutomapFixed;
    LocalSkewRadioListener localSkewRadioListener =
      new LocalSkewRadioListener(this);
    createRadioBox(
      panelLocalSkewSolution,
      bgLocalSkewSolution,
      items,
      "Local Skew solution type",
      localSkewRadioListener);

    ltfLocalSkewGroupSize.setMaximumSize(dimLTF);
    panelLocalSkewSolution.add(ltfLocalSkewGroupSize.getContainer());
    ltfLocalSkewAdditionalGroups.setMaximumSize(dimLTF);
    panelLocalSkewSolution.add(ltfLocalSkewAdditionalGroups.getContainer());
    panelLocalSkewSolution.add(Box.createRigidArea(FixedDim.x0_y5));
    panelLocalSkewSolution.add(Box.createVerticalGlue());

    panelLocalDistortionSolution.add(panelLocalXstretchSolution);
    panelLocalDistortionSolution.add(panelLocalSkewSolution);
    panelLocalDistortionSolution.setPreferredSize(new Dimension(400, 350));

    tabPane.addTab("Local Distortion", panelLocalDistortionSolution);

  }

}
class LocalAlignmentsListener implements ActionListener {
  TiltalignPanel panel;

  LocalAlignmentsListener(TiltalignPanel adaptee) {
    panel = adaptee;
  }
  public void actionPerformed(ActionEvent event) {
    panel.updateLocalAlignmentState(event);
  }
}

class TiltAngleRadioListener implements ActionListener {
  TiltalignPanel panel;

  TiltAngleRadioListener(TiltalignPanel adaptee) {
    panel = adaptee;
  }
  public void actionPerformed(ActionEvent event) {
    panel.updateTiltAngleSolutionPanel(event);
  }
}

class LocalTiltAngleRadioListener implements ActionListener {
  TiltalignPanel panel;

  LocalTiltAngleRadioListener(TiltalignPanel adaptee) {
    panel = adaptee;
  }
  public void actionPerformed(ActionEvent event) {
    panel.updateLocalTiltAngleSolutionPanel(event);
  }
}

class MagnificationRadioListener implements ActionListener {
  TiltalignPanel panel;

  MagnificationRadioListener(TiltalignPanel adaptee) {
    panel = adaptee;
  }
  public void actionPerformed(ActionEvent event) {
    panel.updateMagnificationSolutionPanel(event);
  }
}

class LocalMagnificationRadioListener implements ActionListener {
  TiltalignPanel panel;

  LocalMagnificationRadioListener(TiltalignPanel adaptee) {
    panel = adaptee;
  }
  public void actionPerformed(ActionEvent event) {
    panel.updateLocalMagnificationSolutionPanel(event);
  }
}

class CompressionRadioListener implements ActionListener {
  TiltalignPanel panel;

  CompressionRadioListener(TiltalignPanel adaptee) {
    panel = adaptee;
  }
  public void actionPerformed(ActionEvent event) {
    panel.updateCompressionSolutionPanel(event);
  }
}

class DistortionRadioListener implements ActionListener {
  TiltalignPanel panel;

  DistortionRadioListener(TiltalignPanel adaptee) {
    panel = adaptee;
  }
  public void actionPerformed(ActionEvent event) {
    panel.updateDistortionSolutionPanel(event);
  }
}

class LocalDistortionRadioListener implements ActionListener {
  TiltalignPanel panel;

  LocalDistortionRadioListener(TiltalignPanel adaptee) {
    panel = adaptee;
  }
  public void actionPerformed(ActionEvent event) {
    panel.updateLocalDistortionSolutionPanel(event);
  }
}

class XstretchRadioListener implements ActionListener {
  TiltalignPanel panel;

  XstretchRadioListener(TiltalignPanel adaptee) {
    panel = adaptee;
  }
  public void actionPerformed(ActionEvent event) {
    panel.updateXstretchSolutionPanel(event);
  }
}

class LocalXstretchRadioListener implements ActionListener {
  TiltalignPanel panel;

  LocalXstretchRadioListener(TiltalignPanel adaptee) {
    panel = adaptee;
  }
  public void actionPerformed(ActionEvent event) {
    panel.updateLocalXstretchSolutionPanel(event);
  }
}

class SkewRadioListener implements ActionListener {
  TiltalignPanel panel;

  SkewRadioListener(TiltalignPanel adaptee) {
    panel = adaptee;
  }
  public void actionPerformed(ActionEvent event) {
    panel.updateSkewSolutionPanel(event);
  }
}

class LocalSkewRadioListener implements ActionListener {
  TiltalignPanel panel;

  LocalSkewRadioListener(TiltalignPanel adaptee) {
    panel = adaptee;
  }
  public void actionPerformed(ActionEvent event) {
    panel.updateLocalSkewSolutionPanel(event);
  }
}
