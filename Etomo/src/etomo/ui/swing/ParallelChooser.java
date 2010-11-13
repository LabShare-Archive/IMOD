package etomo.ui.swing;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;

import etomo.ParallelManager;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
 * <p> Revision 1.2  2008/09/30 22:00:25  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 1.1  2007/11/06 19:53:53  sueh
 * <p> bug# 1047 Main dialog of ParallelManager.  Allows user to choose either a
 * <p> generic parallel process or anisotropic diffusion.
 * <p> </p>
 */
public final class ParallelChooser {
  public static final String rcsid = "$Id$";

  private final SpacedPanel rootPanel = SpacedPanel.getInstance();
  private final MultiLineButton btnGeneric = new MultiLineButton(
      "Generic Parallel Process");
  private final MultiLineButton btnAnisotropicDiffusion = new MultiLineButton(
      "Nonlinear Anisotropic Diffusion");

  private final ParallelManager manager;

  private ParallelChooser(ParallelManager manager) {
    this.manager = manager;
    rootPanel.setBoxLayout(BoxLayout.X_AXIS);
    rootPanel.setBorder(new BeveledBorder("Choose a process").getBorder());
    btnGeneric.setSize();
    rootPanel.add(btnGeneric);
    btnAnisotropicDiffusion.setSize();
    rootPanel.add(btnAnisotropicDiffusion);
  }

  public static ParallelChooser getInstance(ParallelManager manager) {
    ParallelChooser instance = new ParallelChooser(manager);
    instance.addListeners();
    return instance;
  }

  public Container getContainer() {
    return rootPanel.getContainer();
  }

  private void addListeners() {
    ActionListener listener = new PCActionListener(this);
    btnGeneric.addActionListener(listener);
    btnAnisotropicDiffusion.addActionListener(listener);
  }

  private void action(ActionEvent event) {
    String command = event.getActionCommand();
    if (command == btnGeneric.getActionCommand()) {
      rootPanel.setVisible(false);
      manager.openParallelDialog();
    }
    else if (command == btnAnisotropicDiffusion.getActionCommand()) {
      rootPanel.setVisible(false);
      manager.openAnisotropicDiffusionDialog();
    }
  }

  private class PCActionListener implements ActionListener {
    ParallelChooser adaptee;

    PCActionListener(ParallelChooser adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.action(event);
    }
  }
}
