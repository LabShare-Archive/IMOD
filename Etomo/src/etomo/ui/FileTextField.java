package etomo.ui;

import java.awt.Container;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;

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
 * <p> $Log$ </p>
 */
final class FileTextField {
  public static final String rcsid = "$Id$";

  private final JButton button = new JButton(new ImageIcon(ClassLoader
      .getSystemResource("images/openFile.gif")));
  private final LabeledTextField field;
  private SpacedPanel panel = null;

  FileTextField(String label) {
    field = new LabeledTextField(label);
  }

  Container getContainer() {
    if (panel == null) {
      panel = new SpacedPanel();
      panel.setBoxLayout(BoxLayout.X_AXIS);
      panel.add(field);
      panel.add(button);
      button.setPreferredSize(FixedDim.folderButton);
      button.setMaximumSize(FixedDim.folderButton);
    }
    return panel.getContainer();
  }

  void addActionListener(ActionListener actionListener) {
    button.addActionListener(actionListener);
  }

  void setEnabled(boolean enabled) {
    field.setEditable(enabled);
    button.setEnabled(enabled);
  }

  void setText(String text) {
    field.setText(text);
  }

  String getText() {
    return field.getText();
  }

  void setToolTipText(String text) {
    field.setToolTipText(text);
    button.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }
}
