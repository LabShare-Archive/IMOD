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
    this(label,true);
  }
  
  private FileTextField(String label,boolean labeled) {
    panel.setBoxLayout(BoxLayout.X_AXIS);
    if (labeled) {
      this.label=new JLabel(label);
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
    return new FileTextField(actionCommand,false);
  }
  
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
