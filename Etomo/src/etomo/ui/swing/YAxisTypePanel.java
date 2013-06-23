package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.type.AxisID;
import etomo.type.EnumeratedType;
import etomo.type.EtomoAutodoc;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.FieldLabels;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2009</p>
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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.3  2009/12/23 02:30:04  sueh
 * <p> bug# 1296 Stop taking tooltips from peetprm.adoc.
 * <p>
 * <p> Revision 1.2  2009/12/08 16:07:43  sueh
 * <p> bug# 1287 Added getYAxisType.
 * <p>
 * <p> Revision 1.1  2009/12/08 02:50:59  sueh
 * <p> bug# 1286 Factored out of PeetDialog.
 * <p> </p>
 */

final class YAxisTypePanel {
  public static final String rcsid = "$Id$";

  private static final String Y_AXIS_CONTOUR_LABEL = "End points of contour";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final ButtonGroup bgYAxisType = new ButtonGroup();
  private final RadioButton rbYAxisTypeYAxis = new RadioButton(
      MatlabParam.YAxisType.Y_AXIS, bgYAxisType);
  private final RadioButton rbYAxisTypeParticleModel = new RadioButton(
      MatlabParam.YAxisType.PARTICLE_MODEL, bgYAxisType);
  private final RadioButton rbYAxisTypeContour = new RadioButton(
      MatlabParam.YAxisType.CONTOUR, bgYAxisType, ":  ");

  private final YAxisTypeParent parent;
  private final BaseManager manager;

  private YAxisTypePanel(final BaseManager manager, final YAxisTypeParent parent) {
    this.manager = manager;
    this.parent = parent;
  }

  static YAxisTypePanel getInstance(final BaseManager manager,
      final YAxisTypeParent parent) {
    YAxisTypePanel instance = new YAxisTypePanel(manager, parent);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    YAxisTypeActionListener actionListener = new YAxisTypeActionListener(this);
    rbYAxisTypeYAxis.addActionListener(actionListener);
    rbYAxisTypeParticleModel.addActionListener(actionListener);
    rbYAxisTypeContour.addActionListener(actionListener);
  }

  private void createPanel() {
    // local panels
    JPanel pnlYAxisContour = new JPanel();
    SpacedPanel pnlYaxisType = SpacedPanel.getInstance();
    pnlRoot.setBoxLayout(BoxLayout.X_AXIS);
    pnlRoot.setBorder(new EtchedBorder(FieldLabels.YAXIS_TYPE_LABEL).getBorder());
    pnlRoot.add(pnlYaxisType);
    pnlRoot.add(Box.createRigidArea(FixedDim.x197_y0));
    // YaxisType
    pnlYaxisType.setBoxLayout(BoxLayout.Y_AXIS);
    pnlYaxisType.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlYaxisType.add(rbYAxisTypeYAxis);
    pnlYaxisType.add(rbYAxisTypeParticleModel);
    pnlYaxisType.add(pnlYAxisContour);
    pnlYaxisType.add(Box.createRigidArea(FixedDim.x0_y23));
    // YaxisContour
    pnlYAxisContour.setLayout(new BoxLayout(pnlYAxisContour, BoxLayout.X_AXIS));
    pnlYAxisContour.add(rbYAxisTypeContour.getComponent());
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  /**
   * Load data from MatlabParamFile.  Load only active data after the meta data
   * has been loaded.
   * Do not rely on whether fields are enabled to make decisions in this
   * function; updateDisplay may not have been run with data in the screen.
   * When looking at the settings of dialog fields, make sure that they have
   * already been loaded in THIS function.  This is sometimes the first data-
   * loading function to be run.
   * @param matlabParamFile
   * @param paramatersOnly 
   */
  void setParameters(final MatlabParam matlabParam) {
    MatlabParam.YAxisType yaxisType = matlabParam.getYAxisType();
    if (yaxisType == MatlabParam.YAxisType.Y_AXIS) {
      rbYAxisTypeYAxis.setSelected(true);
    }
    else if (yaxisType == MatlabParam.YAxisType.PARTICLE_MODEL) {
      rbYAxisTypeParticleModel.setSelected(true);
    }
    else if (yaxisType == MatlabParam.YAxisType.CONTOUR) {
      rbYAxisTypeContour.setSelected(true);
    }
  }

  void getParameters(final MatlabParam matlabParam) {
    matlabParam.setYaxisType(((RadioButton.RadioButtonModel) bgYAxisType.getSelection())
        .getEnumeratedType());
  }

  EnumeratedType getYAxisType() {
    return ((RadioButton.RadioButtonModel) bgYAxisType.getSelection())
        .getEnumeratedType();
  }

  void reset() {
    rbYAxisTypeYAxis.setSelected(false);
    rbYAxisTypeParticleModel.setSelected(false);
    rbYAxisTypeContour.setSelected(false);
  }

  private void action(final String actionCommand,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (actionCommand.equals(rbYAxisTypeYAxis.getActionCommand())
        || actionCommand.equals(rbYAxisTypeParticleModel.getActionCommand())
        || actionCommand.equals(rbYAxisTypeContour.getActionCommand())) {
      parent.updateDisplay();
    }
  }

  private void setTooltips() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.PEET_PRM, AxisID.ONLY);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    ReadOnlySection section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME,
        MatlabParam.YAxisType.KEY);
    rbYAxisTypeYAxis.setToolTipText(section);
    rbYAxisTypeParticleModel.setToolTipText(section);
    rbYAxisTypeContour.setToolTipText(section);
  }

  private static final class YAxisTypeActionListener implements ActionListener {
    private final YAxisTypePanel yAxisTypePanel;

    private YAxisTypeActionListener(final YAxisTypePanel yAxisTypePanel) {
      this.yAxisTypePanel = yAxisTypePanel;
    }

    public void actionPerformed(final ActionEvent event) {
      yAxisTypePanel.action(event.getActionCommand(), null);
    }
  }
}
