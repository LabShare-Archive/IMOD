package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.7  2009/09/20 21:32:25  sueh
 * <p> bug# 1268 Added a default value to LabeledSpinner.
 * <p>
 * <p> Revision 1.6  2009/06/05 02:10:29  sueh
 * <p> bug# 1219 Renamed spinner to spBinningXY.  Renamed getInt to
 * <p> setBinningInXandY.
 * <p>
 * <p> Revision 1.5  2008/09/30 20:55:55  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 1.4  2008/05/13 22:59:55  sueh
 * <p> bug# 847 Added getButton.
 * <p>
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
  private final LabeledSpinner spBinningXY;
  private final JLabel label;
  private JPanel panel = null;

  BinnedXY3dmodButton(String label, Run3dmodButtonContainer container) {
    spBinningXY = LabeledSpinner.getInstance("Open binned by ", 1, 1, 50, 1);
    this.label = new JLabel(" in X and Y");
    button = Run3dmodButton.get3dmodInstance(label, container);
  }

  Container getContainer() {
    if (panel == null) {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      panel.setAlignmentX(Component.CENTER_ALIGNMENT);
      panel.add(Box.createHorizontalGlue());
      panel.add(Box.createHorizontalGlue());
      panel.add(Box.createHorizontalGlue());
      SpacedPanel borderPanel = SpacedPanel.getInstance();
      borderPanel.setBoxLayout(BoxLayout.Y_AXIS);
      borderPanel.setBorder(BorderFactory.createEtchedBorder());
      borderPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
      JPanel spinnerPanel = new JPanel();
      spinnerPanel.setLayout(new BoxLayout(spinnerPanel, BoxLayout.X_AXIS));
      spinnerPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
      spinnerPanel.add(spBinningXY.getContainer());
      spinnerPanel.add(label);
      borderPanel.add(spinnerPanel);
      JPanel pnlButtons = new JPanel();
      pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
      pnlButtons.setAlignmentX(Component.CENTER_ALIGNMENT);
      button.setSize();
      pnlButtons.add(button.getComponent());
      borderPanel.add(pnlButtons);
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
    spBinningXY.setToolTipText(text);
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

  int getBinningInXandY() {
    return ((Integer) spBinningXY.getValue()).intValue();
  }

  void setEnabled(boolean enabled) {
    spBinningXY.setEnabled(enabled);
    label.setEnabled(enabled);
    button.setEnabled(enabled);
  }
}
