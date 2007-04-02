package etomo.ui;

import java.awt.Container;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;

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
 * <p> Revision 1.2  2007/03/30 23:49:23  sueh
 * <p> bug# 964 Added an option to create an unlabeled version of class.  The unlabeled
 * <p> version is still named.
 * <p>
 * <p> Revision 1.1  2007/02/22 20:37:39  sueh
 * <p> bug# 964 Moved FileTextField from JoinDialog to the etomo.ui package so that it
 * <p> can be shared.
 * <p> </p>
 */
final class FileTextField {
  public static final String rcsid = "$Id$";

  private final JButton button = new JButton(new ImageIcon(ClassLoader
      .getSystemResource("images/openFile.gif")));
  private final SpacedPanel panel = new SpacedPanel();
  private final TextField field;

  private JLabel label = null;

  FileTextField(String label) {
    this(label, true);
  }

  private FileTextField(String label, boolean labeled) {
    panel.setBoxLayout(BoxLayout.X_AXIS);
    if (labeled) {
      this.label = new JLabel(label);
      panel.add(this.label);
    }
    field = new TextField(label);
    panel.add(field);
    button.setActionCommand(label);
    panel.add(button);
    button.setPreferredSize(FixedDim.folderButton);
    button.setMaximumSize(FixedDim.folderButton);
  }

  static FileTextField getUnlabeledInstance(String actionCommand) {
    return new FileTextField(actionCommand, false);
  }

  /**
   * The action command is the label passed into the constructor, whether or not a
   * label is displayed.  If the label is shared with another field that uses the
   * same action listener, then you will not know which field responded to an action.
   * Use different action listeners when the label is share between fields.
   * @return
   */
  String getActionCommand() {
    return button.getActionCommand();
  }

  Container getContainer() {
    return panel.getContainer();
  }

  void addActionListener(ActionListener actionListener) {
    button.addActionListener(actionListener);
  }

  void setEditable(boolean editable) {
    field.setEditable(editable);
    button.setEnabled(editable);
  }

  void setEnabled(boolean enabled) {
    field.setEnabled(enabled);
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
