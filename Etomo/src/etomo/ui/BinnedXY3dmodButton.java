package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;

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
 * <p> Revision 1.3  2008/05/03 00:47:54  sueh
 * <p> bug# 847 Made Run3dmodButton constructor private.
 * <p>
 * <p> Revision 1.2  2007/02/09 00:44:22  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.1  2007/02/05 23:33:17  sueh
 * <p> bug# 962 Composite display class containing a spinner and a button.
 * <p> </p>
 */
final class BinnedXY3dmodButton {
  public static final String rcsid = "$Id$";
  private final Run3dmodButton button;
  private final LabeledSpinner spinner;
  private final JLabel label;
  private JPanel panel = null;

  BinnedXY3dmodButton(String label, Run3dmodButtonContainer container) {
    spinner = new LabeledSpinner("Open binned by ", new SpinnerNumberModel(1,
        1, 50, 1));
    this.label = new JLabel(" in X and Y");
    button = Run3dmodButton.get3dmodInstance(label, container);
  }

  Container getContainer() {
    if (panel == null) {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      panel.add(Box.createHorizontalGlue());
      panel.add(Box.createHorizontalGlue());
      panel.add(Box.createHorizontalGlue());
      SpacedPanel borderPanel = new SpacedPanel();
      borderPanel.setBoxLayout(BoxLayout.Y_AXIS);
      borderPanel.setBorder(BorderFactory.createEtchedBorder());
      borderPanel.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
      JPanel spinnerPanel = new JPanel();
      spinnerPanel.setLayout(new BoxLayout(spinnerPanel, BoxLayout.X_AXIS));
      spinnerPanel.add(spinner.getContainer());
      spinnerPanel.add(label);
      borderPanel.add(spinnerPanel);
      button.setSize();
      borderPanel.add(button);
      panel.add(borderPanel.getContainer());
      panel.add(Box.createHorizontalGlue());
      panel.add(Box.createHorizontalGlue());
      panel.add(Box.createHorizontalGlue());
    }
    return panel;
  }
  
   Deferred3dmodButton getButton() {
     return button;
   }

  void setSpinnerToolTipText(String text) {
    spinner.setToolTipText(text);
    label.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  void setButtonToolTipText(String text) {
    button.setToolTipText(text);
  }

  void addActionListener(ActionListener actionListener) {
    button.addActionListener(actionListener);
  }

  String getActionCommand() {
    return button.getActionCommand();
  }

  int getInt() {
    return ((Integer) spinner.getValue()).intValue();
  }

  void setEnabled(boolean enabled) {
    spinner.setEnabled(enabled);
    label.setEnabled(enabled);
    button.setEnabled(enabled);
  }
}
