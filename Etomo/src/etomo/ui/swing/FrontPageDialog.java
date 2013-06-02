package etomo.ui.swing;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.PeetManager;
import etomo.type.AxisID;
import etomo.type.ToolType;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2010</p>
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
 * <p> Revision 1.2  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.10  2010/09/08 19:17:57  sueh
 * <p> bug# 1401 Added reconActionForAutomation
 * <p>
 * <p> Revision 1.9  2010/06/30 21:02:38  sueh
 * <p> bug# 1387 Debuging btnRecon.
 * <p>
 * <p> Revision 1.8  2010/03/27 05:05:48  sueh
 * <p> bug# 1316 Add volume flatten tool button to front page.
 * <p>
 * <p> Revision 1.7  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.6  2009/12/29 18:50:31  sueh
 * <p> bug# 1297 Matched button labels to "New >" menu item labels.
 * <p>
 * <p> Revision 1.5  2009/11/24 00:44:33  sueh
 * <p> bug# 1289 On PEET button being pressed, calling
 * <p> PeetManager.isInterfaceAvaiable before opening PEET interface.
 * <p>
 * <p> Revision 1.4  2009/11/23 17:52:21  sueh
 * <p> bug# 1289 Popping up a message instead of opening the PEET interface if
 * <p> PARTICLE_DIR doesn't exist.
 * <p>
 * <p> Revision 1.3  2009/10/28 17:47:16  sueh
 * <p> bug# 1275 Moved responsibility for closing FrontPageManager to
 * <p> EtomoDirector.
 * <p>
 * <p> Revision 1.2  2009/10/27 20:41:58  sueh
 * <p> bug# 1275 Made class a top-level dialog for FrontPageManager.
 * <p>
 * <p> Revision 1.1  2009/10/23 19:46:48  sueh
 * <p> bug# 1275 Default display.  Contains buttons for choosing one of five
 * <p> interfaces.
 * <p> </p>
 */

public final class FrontPageDialog {
  public static final String rcsid = "$Id$";

  private final JPanel pnlRoot = new JPanel();
  private final MultiLineButton btnRecon = MultiLineButton
      .getDebugInstance(EtomoMenu.RECON_LABEL);
  private final MultiLineButton btnJoin = new MultiLineButton(EtomoMenu.JOIN_LABEL);
  private final MultiLineButton btnNad = new MultiLineButton(EtomoMenu.NAD_LABEL);
  private final MultiLineButton btnGeneric = new MultiLineButton(EtomoMenu.GENERIC_LABEL);
  private final MultiLineButton btnPeet = new MultiLineButton(EtomoMenu.PEET_LABEL);
  private final MultiLineButton btnSerialSections = new MultiLineButton(
      EtomoMenu.SERIAL_SECTIONS_LABEL);
  private final MultiLineButton btnFlattenVolume = new MultiLineButton(
      EtomoMenu.FLATTEN_VOLUME_LABEL);
  private final MultiLineButton btnGpuTiltTest = new MultiLineButton(
      EtomoMenu.GPU_TILT_TEST_LABEL);

  private final BaseManager manager;
  private final AxisID axisID;

  private FrontPageDialog(final BaseManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
  }

  public static FrontPageDialog getInstance(final BaseManager manager, final AxisID axisID) {
    FrontPageDialog instance = new FrontPageDialog(manager, axisID);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // panels
    JPanel pnlProjectLabel = new JPanel();
    JPanel pnlProjects = new JPanel();
    JPanel pnlToolLabel = new JPanel();
    JPanel pnlTools = new JPanel();
    // initialize
    btnRecon.setSize();
    btnJoin.setSize();
    btnNad.setSize();
    btnGeneric.setSize();
    btnPeet.setSize();
    btnSerialSections.setSize();
    btnFlattenVolume.setSize();
    btnGpuTiltTest.setSize();
    // root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setAlignmentX(Box.LEFT_ALIGNMENT);
    pnlRoot.add(pnlProjectLabel);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y3));
    pnlRoot.add(pnlProjects);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlRoot.add(pnlToolLabel);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y3));
    pnlRoot.add(pnlTools);
    // project label
    pnlProjectLabel.setLayout(new BoxLayout(pnlProjectLabel, BoxLayout.X_AXIS));
    pnlProjectLabel.add(new JLabel("New project:"));
    pnlProjectLabel.add(Box.createHorizontalGlue());
    // projects
    pnlProjects.setLayout(new GridLayout(3, 2, 7, 7));
    pnlProjects.add(btnRecon.getComponent());
    pnlProjects.add(btnJoin.getComponent());
    pnlProjects.add(btnPeet.getComponent());
    pnlProjects.add(btnSerialSections.getComponent());
    pnlProjects.add(btnNad.getComponent());
    pnlProjects.add(btnGeneric.getComponent());
    // tool label
    pnlToolLabel.setLayout(new BoxLayout(pnlToolLabel, BoxLayout.X_AXIS));
    pnlToolLabel.add(new JLabel("Tools:"));
    pnlToolLabel.add(Box.createHorizontalGlue());
    // tools
    pnlTools.setLayout(new GridLayout(1, 2, 7, 7));
    pnlTools.add(btnFlattenVolume.getComponent());
    pnlTools.add(btnGpuTiltTest.getComponent());
  }

  private void addListeners() {
    ActionListener actionListener = new FrontPageActionListener(this);
    btnRecon.addActionListener(actionListener);
    btnJoin.addActionListener(actionListener);
    btnNad.addActionListener(actionListener);
    btnGeneric.addActionListener(actionListener);
    btnPeet.addActionListener(actionListener);
    btnSerialSections.addActionListener(actionListener);
    btnFlattenVolume.addActionListener(actionListener);
    btnGpuTiltTest.addActionListener(actionListener);
  }

  public void show() {
    manager.getMainPanel().showProcess(pnlRoot, axisID);
  }

  private void action(ActionEvent actionEvent) {
    String actionCommand = actionEvent.getActionCommand();
    if (actionCommand.equals(btnRecon.getActionCommand())) {
      EtomoDirector.INSTANCE.openTomogram(true, axisID);
    }
    else if (actionCommand.equals(btnJoin.getActionCommand())) {
      EtomoDirector.INSTANCE.openJoin(true, axisID);
    }
    else if (actionCommand.equals(btnNad.getActionCommand())) {
      EtomoDirector.INSTANCE.openAnisotropicDiffusion(true, axisID);
    }
    else if (actionCommand.equals(btnGeneric.getActionCommand())) {
      EtomoDirector.INSTANCE.openGenericParallel(true, axisID);
    }
    else if (actionCommand.equals(btnPeet.getActionCommand())) {
      if (PeetManager.isInterfaceAvailable()) {
        EtomoDirector.INSTANCE.openPeet(true, axisID);
      }
    }
    else if (actionCommand.equals(btnSerialSections.getActionCommand())) {
      EtomoDirector.INSTANCE.openSerialSections(true, axisID);
    }
    else if (actionCommand.equals(btnFlattenVolume.getActionCommand())) {
      EtomoDirector.INSTANCE.openTools(ToolType.FLATTEN_VOLUME);
    }
    else if (actionCommand.equals(btnGpuTiltTest.getActionCommand())) {
      EtomoDirector.INSTANCE.openTools(ToolType.GPU_TILT_TEST);
    }
  }

  private void setTooltips() {
    btnRecon.setToolTipText("Start a new tomographic reconstruction.");
    btnJoin.setToolTipText("Stack tomograms.");
    btnNad.setToolTipText("Run a nonlinear anisotropic diffusion process on a "
        + "tomogram.");
    btnGeneric.setToolTipText("Run a generic parallel process.");
    btnPeet.setToolTipText("Start the interface for the PEET particle averaging "
        + "package.");
    btnGpuTiltTest
        .setToolTipText("Test the reliability of GPU with repeated runs of the Tilt "
            + "program");
  }

  private static final class FrontPageActionListener implements ActionListener {
    private final FrontPageDialog listenee;

    private FrontPageActionListener(FrontPageDialog listenee) {
      this.listenee = listenee;
    }

    public void actionPerformed(ActionEvent actionEvent) {
      listenee.action(actionEvent);
    }
  }
}
