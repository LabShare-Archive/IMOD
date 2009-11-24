package etomo.ui;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;

import etomo.EtomoDirector;
import etomo.PeetManager;
import etomo.type.AxisID;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance(true);
  private final MultiLineButton btnRecon = new MultiLineButton(
      "Tomographic Reconstruction");
  private final MultiLineButton btnJoin = new MultiLineButton("Join Tomograms");
  private final MultiLineButton btnNad = new MultiLineButton(
      "Nonlinear Anisotropic Diffusion");
  private final MultiLineButton btnGeneric = new MultiLineButton(
      "Generic Parallel Process");
  private final MultiLineButton btnPeet = new MultiLineButton("PEET");

  private FrontPageDialog() {
  }

  public static FrontPageDialog getInstance() {
    FrontPageDialog instance = new FrontPageDialog();
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    //local panels
    SpacedPanel pnlReconstruction = SpacedPanel.getInstance();
    SpacedPanel pnlParallelProcesses = SpacedPanel.getInstance();
    SpacedPanel pnlAveraging = SpacedPanel.getInstance();
    //initialize
    btnRecon.setSize();
    btnJoin.setSize();
    btnNad.setSize();
    btnGeneric.setSize();
    btnPeet.setSize();
    //root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.add(pnlReconstruction);
    pnlRoot.add(pnlParallelProcesses);
    pnlRoot.add(pnlAveraging);
    //reconstruction panel
    pnlReconstruction.setBoxLayout(BoxLayout.X_AXIS);
    pnlReconstruction.add(btnRecon.getComponent());
    pnlReconstruction.add(btnJoin.getComponent());
    //parallel process panel
    pnlParallelProcesses.setBoxLayout(BoxLayout.X_AXIS);
    pnlParallelProcesses.add(btnNad.getComponent());
    pnlParallelProcesses.add(btnGeneric.getComponent());
    //averaging panel
    pnlAveraging.setBoxLayout(BoxLayout.X_AXIS);
    pnlAveraging.add(btnPeet.getComponent());
  }

  private void addListeners() {
    ActionListener actionListener = new FrontPageActionListener(this);
    btnRecon.addActionListener(actionListener);
    btnJoin.addActionListener(actionListener);
    btnNad.addActionListener(actionListener);
    btnGeneric.addActionListener(actionListener);
    btnPeet.addActionListener(actionListener);
  }

  public Container getContainer() {
    return pnlRoot.getContainer();
  }

  public boolean usingParallelProcessing() {
    return false;
  }

  private void action(ActionEvent actionEvent) {
    String actionCommand = actionEvent.getActionCommand();
    if (actionCommand.equals(btnRecon.getActionCommand())) {
      EtomoDirector.INSTANCE.openTomogram(true, AxisID.ONLY);
    }
    else if (actionCommand.equals(btnJoin.getActionCommand())) {
      EtomoDirector.INSTANCE.openJoin(true, AxisID.ONLY);
    }
    else if (actionCommand.equals(btnNad.getActionCommand())) {
      EtomoDirector.INSTANCE.openAnisotropicDiffusion(true, AxisID.ONLY);
    }
    else if (actionCommand.equals(btnGeneric.getActionCommand())) {
      EtomoDirector.INSTANCE.openGenericParallel(true, AxisID.ONLY);
    }
    else if (actionCommand.equals(btnPeet.getActionCommand())) {
      if (PeetManager.isInterfaceAvailable()) {
        EtomoDirector.INSTANCE.openPeet(true, AxisID.ONLY);
      }
    }
  }

  private void setTooltips() {
    btnRecon.setToolTipText("Start a new tomographic reconstruction.");
    btnJoin.setToolTipText("Stack tomograms.");
    btnNad.setToolTipText("Run a nonlinear anisotropic diffusion process on a "
        + "tomogram.");
    btnGeneric.setToolTipText("Run a generic parallel process.");
    btnPeet
        .setToolTipText("Start the interface for the PEET particle averaging "
            + "package.");
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
