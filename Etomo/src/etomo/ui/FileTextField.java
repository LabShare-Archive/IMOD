package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JLabel;

import etomo.type.ConstStringParameter;

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
 * <p> Revision 1.15  2009/10/15 23:28:36  sueh
 * <p> bug# 1274 In isEmpty testing getText() result.  File is not up to date if the
 * <p> the text is typed in manually.
 * <p>
 * <p> Revision 1.14  2009/04/27 18:00:07  sueh
 * <p> bug# 1211 Added exists, which calls File.exists.
 * <p>
 * <p> Revision 1.13  2009/01/20 20:03:28  sueh
 * <p> bug# 1102 Changed the file button to a SimpleButton and named it after
 * <p> the text field's label.
 * <p>
 * <p> Revision 1.12  2008/11/22 00:18:34  sueh
 * <p> bug# 1155 Fixed problem with outputting a corrupted file path.  The
 * <p> actual file has to be saved because this class sometimes displays a
 * <p> partial file name.
 * <p>
 * <p> Revision 1.11  2008/10/27 20:38:01  sueh
 * <p> bug# 1441 Fixed a bug where the member variable file was not kept up
 * <p> to date.
 * <p>
 * <p> Revision 1.10  2008/09/30 21:01:21  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 1.9  2008/02/26 01:39:35  sueh
 * <p> bug# 1087 Added isEmpty.
 * <p>
 * <p> Revision 1.8  2008/02/19 00:46:59  sueh
 * <p> bug# 1078 Added setFieldWidth.
 * <p>
 * <p> Revision 1.7  2007/11/09 17:46:49  sueh
 * <p> bug# 1047 Added showPartialPath.
 * <p>
 * <p> Revision 1.6  2007/11/06 19:52:41  sueh
 * <p> bug# 1047 Giving access to the File in the text field.
 * <p>
 * <p> Revision 1.5  2007/06/08 22:21:18  sueh
 * <p> bug# 1014 Added clear().
 * <p>
 * <p> Revision 1.4  2007/04/09 21:17:42  sueh
 * <p> bug# 964 Added setText(constEtomoNumber).
 * <p>
 * <p> Revision 1.3  2007/04/02 16:02:15  sueh
 * <p> bug# 964 Commented getActionCommand
 * <p>
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

  private final SimpleButton button = new SimpleButton(new ImageIcon(
      ClassLoader.getSystemResource("images/openFile.gif")));
  private final SpacedPanel panel = SpacedPanel.getInstance();
  private final TextField field;

  private JLabel label = null;
  private boolean showPartialPath = false;
  private boolean debug = false;
  //Must have file because the field may display a shortened name.  Keep file
  //up to date.
  private File file = null;
  private String propertyUserDir = null;
  private Component parent = null;
  private int fileSelectionMode = -1;

  FileTextField(final String label) {
    this(label, true, null);
  }

  private FileTextField(final String label, final boolean labeled,
      String propertyUserDir) {
    this.propertyUserDir = propertyUserDir;
    panel.setBoxLayout(BoxLayout.X_AXIS);
    if (labeled) {
      this.label = new JLabel(label);
      panel.add(this.label);
    }
    field = new TextField(label);
    panel.add(field);
    button.setActionCommand(label);
    button.setName(label);
    panel.add(button);
    button.setPreferredSize(FixedDim.folderButton);
    button.setMaximumSize(FixedDim.folderButton);
  }

  static FileTextField getUnlabeledInstance(final String actionCommand) {
    return new FileTextField(actionCommand, false, null);
  }

  void setFieldWidth(final double width) {
    field.setTextPreferredWidth(width);
  }

  void setAlignmentX(final float alignment) {
    panel.setAlignmentX(alignment);
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

  void addActionListener(final ActionListener actionListener) {
    button.addActionListener(actionListener);
  }

  void addAction(String propertyUserDir, Component parent,
      int fileSelectionMode) {
    this.propertyUserDir = propertyUserDir;
    this.parent = parent;
    this.fileSelectionMode = fileSelectionMode;
    button.addActionListener(new FileTextFieldActionListener(this));
  }

  private void action() {
    //  Open up the file chooser in the current working directory
    JFileChooser chooser = new FileChooser(new File(propertyUserDir));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    if (fileSelectionMode != -1) {
      chooser.setFileSelectionMode(fileSelectionMode);
    }
    int returnVal = chooser.showOpenDialog(parent);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File file = chooser.getSelectedFile();
      try {
        setText(file.getAbsolutePath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
    }
  }

  void clear() {
    file = null;
    field.setText("");
  }

  void setShowPartialPath() {
    showPartialPath = true;
  }

  void setFieldEditable(final boolean editable) {
    field.setEditable(editable);
  }

  void setDebug(final boolean input) {
    debug = input;
  }

  void setEditable(final boolean editable) {
    field.setEditable(editable);
    button.setEnabled(editable);
  }

  void setEnabled(final boolean enabled) {
    field.setEnabled(enabled);
    button.setEnabled(enabled);
  }

  void setButtonEnabled(final boolean enabled) {
    button.setEnabled(enabled);
  }

  boolean isEmpty() {
    return field.getText().matches("\\s*");
  }

  boolean exists() {
    if (file == null) {
      return false;
    }
    return file.exists();
  }

  void setFile(final File file) {
    setFieldFromFile(file);
  }

  File getFile() {
    return file;
  }

  String getFileName() {
    return file.getName();
  }

  String getFileAbsolutePath() {
    return file.getAbsolutePath();
  }

  void setText(final String text) {
    if (text != null && !text.matches("\\s*")) {
      setFieldFromFile(new File(text));
    }
    else {
      setFieldFromFile(null);
    }
  }

  void setText(final ConstStringParameter input) {
    setText(input.toString());
  }

  /**
   * All set commands should call this function.
   * @param inputFile
   */
  private void setFieldFromFile(File inputFile) {
    file = inputFile;
    if (inputFile == null) {
      field.setText("");
      return;
    }
    if (!showPartialPath) {
      field.setText(inputFile.getAbsolutePath());
      return;
    }
    String parent = inputFile.getParent();
    int separatorIndex = parent.toString().lastIndexOf(File.separatorChar);
    if (separatorIndex != -1) {
      parent = parent.substring(separatorIndex);
    }
    StringBuffer text = new StringBuffer("...");
    if (!parent.startsWith(File.separator)) {
      text.append(File.separator);
    }
    text.append(parent);
    if (!parent.endsWith(File.separator)) {
      text.append(File.separator);
    }
    text.append(inputFile.getName());
    field.setText(text.toString());
  }

  String getText() {
    return field.getText();
  }

  void setToolTipText(String text) {
    field.setToolTipText(text);
    panel.setToolTipText(text);
    text = TooltipFormatter.INSTANCE.format(text);
    button.setToolTipText(text);
    if (label != null) {
      label.setToolTipText(text);
    }
  }

  private final class FileTextFieldActionListener implements ActionListener {
    private final FileTextField adaptee;

    private FileTextFieldActionListener(final FileTextField adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action();
    }
  }
}
