package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.filechooser.FileFilter;

import etomo.type.ConstStringParameter;
import etomo.ui.FieldType;

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
 * <p> Revision 1.17  2010/03/27 05:01:18  sueh
 * <p> bug# 1337 Fixed the partial path display in FileTextField.  Updating file from
 * <p> the fields when getting it.
 * <p>
 * <p> Revision 1.16  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
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
final class FileTextField implements FileTextFieldInterface {
  public static final String rcsid = "$Id$";

  private final static Dimension FOLDER_BUTTON_SIZE = FixedDim.folderButton;
  // Assuming the field type is always non-numeric
  private final FieldType FIELD_TYPE = FieldType.STRING;

  private final SimpleButton button = new SimpleButton(new ImageIcon(
      ClassLoader.getSystemResource("images/openFile.gif")));
  private final JPanel panel = new JPanel();
  private final GridBagLayout layout = new GridBagLayout();
  private final GridBagConstraints constraints = new GridBagConstraints();

  private final TextField field;

  private JLabel label = null;

  private boolean debug = false;
  // Must have file because the field may display a shortened name. Keep file
  // up to date.
  private File file = null;
  private String propertyUserDir = null;
  private Component parent = null;
  private int fileSelectionMode = -1;
  private String checkpointValue = null;

  /**
   * Causes the file and the field to be out of sync, since the field no longer
   * displays the absolute path.  When this variable is turned on, the field
   * must be made ineditable.  The field cannot be made editable or enabled
   * while this variable is on.  This variable cannot be turned off.  Default:
   * false.
   */
  private final boolean showPartialPath;

  FileTextField(final String label) {
    this(label, true, null, false);
  }

  private FileTextField(final String label, final boolean labeled,
      String propertyUserDir, boolean showPartialPath) {
    this.propertyUserDir = propertyUserDir;
    this.showPartialPath = showPartialPath;
    panel.setLayout(layout);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weightx = 0.0;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    if (labeled) {
      this.label = new JLabel(label);
      layout.setConstraints(this.label, constraints);
      panel.add(this.label);
    }
    field = new TextField(FIELD_TYPE, label, null);
    field.setTextPreferredSize(new Dimension(250 * (int) Math.round(UIParameters.INSTANCE
        .getFontSizeAdjustment()), FOLDER_BUTTON_SIZE.height));
    constraints.insets = new Insets(0, 0, 0, -1);
    layout.setConstraints(field.getComponent(), constraints);
    panel.add(field.getComponent());
    button.setActionCommand(label);
    button.setName(label);
    constraints.insets = new Insets(0, -1, 0, 0);
    layout.setConstraints(button, constraints);
    panel.add(button);
    button.setPreferredSize(FOLDER_BUTTON_SIZE);
    button.setMaximumSize(FOLDER_BUTTON_SIZE);
    // showPartialPath allows the class to hide the whole absolute path of a file
    // to save display space. Permanantly make the field ineditable.
    if (showPartialPath) {
      field.setEditable(false);
    }
  }

  void setTextPreferredWidth(final int width) {
    field.setTextPreferredSize(new Dimension(width, field.getPreferredSize().height));
  }

  static FileTextField getUnlabeledInstance(final String actionCommand) {
    return new FileTextField(actionCommand, false, null, false);
  }

  static FileTextField getPartialPathInstance(final String label) {
    return new FileTextField(label, true, null, true);
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
    return panel;
  }

  void addActionListener(final ActionListener actionListener) {
    button.addActionListener(actionListener);
  }

  /**
   * @param propertyUserDir
   * @param parent
   * @param fileSelectionMode - JFileChooser.DIRECTORIES_ONLY, etc
   */
  void addAction(String propertyUserDir, Component parent, int fileSelectionMode) {
    this.propertyUserDir = propertyUserDir;
    this.parent = parent;
    this.fileSelectionMode = fileSelectionMode;
    button.addActionListener(new FileTextFieldActionListener(this));
  }

  private void action() {
    // Open up the file chooser in the current working directory
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

  void checkpoint() {
    checkpointValue = getText();
  }

  /**
   * Resets to checkpointValue if checkpointValue has been set.  Otherwise has no effect.
   */
  void resetToCheckpoint() {
    if (checkpointValue == null) {
      return;
    }
    setText(checkpointValue);
  }

  void setFieldEditable(final boolean editable) {
    if (showPartialPath) {
      // Cannot make the field editable if showPartialPath is on.
      return;
    }
    field.setEditable(editable);
  }

  void setDebug(final boolean input) {
    debug = input;
  }

  void setEditable(final boolean editable) {
    if (!showPartialPath) {
      // Cannot make the field editable if showPartialPath is on.
      field.setEditable(editable);
    }
    button.setEnabled(editable);
  }

  void setEnabled(final boolean enabled) {
    if (!showPartialPath || !enabled) {
      // Cannot enable the field if showPartialPath is on.
      field.setEnabled(enabled);
    }
    button.setEnabled(enabled);
  }

  void setVisible(final boolean visible) {
    panel.setVisible(visible);
  }

  void setButtonEnabled(final boolean enabled) {
    button.setEnabled(enabled);
  }

  /**
   * The field is kept up to date by setInternalValues when the file chooser
   * button is pressed.  No need to update values when getting the field.
   * @return
   */
  boolean isEmpty() {
    String text = field.getText();
    return text.matches("\\s*");
  }

  boolean isEditable() {
    return field.isEditable();
  }

  boolean exists() {
    updateInternalValues();
    if (file == null) {
      return false;
    }
    return file.exists();
  }

  public void setFile(final File file) {
    setInternalValues(file);
  }

  public File getFile() {
    updateInternalValues();
    return file;
  }

  String getFileName() {
    updateInternalValues();
    return file.getName();
  }

  String getFileAbsolutePath() {
    updateInternalValues();
    return file.getAbsolutePath();
  }

  public FileFilter getFileFilter() {
    return null;
  }

  void setText(final String text) {
    if (text != null && !text.matches("\\s*")) {
      setInternalValues(new File(text));
    }
    else {
      setInternalValues(null);
    }
  }

  void setText(final ConstStringParameter input) {
    setText(input.toString());
  }

  /**
   * The file can be out of date because the user can edit the field.  Update
   * the file from the field.  If the showPartialPath is on, then the field has
   * never been editable, so no need to update the file.  Call this function
   * before returning any information about the file in non-private function.
   */
  private void updateInternalValues() {
    if (showPartialPath) {
      return;
    }
    String text = field.getText();
    if (text == null || text.matches("\\s*")) {
      file = null;
    }
    else {
      file = new File(text);
    }
  }

  /**
   * Sets the field and the file variable from inputFile.  All set commands
   * should call this function.
   * @param inputFile
   */
  private void setInternalValues(File inputFile) {
    file = inputFile;
    if (inputFile == null) {
      field.setText("");
      return;
    }
    if (!showPartialPath) {
      field.setText(inputFile.getAbsolutePath());
      return;
    }
    // Showing partial path
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

  /**
   * The field is kept up to date by setInternalValues when the file chooser
   * button is pressed.  No need to update values when getting the field.
   * @return
   */
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

  void setFieldToolTipText(String text) {
    field.setToolTipText(text);
    panel.setToolTipText(text);
    text = TooltipFormatter.INSTANCE.format(text);
    if (label != null) {
      label.setToolTipText(text);
    }
  }

  void setButtonToolTipText(String text) {
    button.setToolTipText(text);
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
