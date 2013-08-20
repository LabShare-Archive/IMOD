package etomo.ui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.filechooser.FileFilter;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.ui.FieldType;
import etomo.util.FilePath;
import etomo.util.Utilities;

/**
* <p>Description: Like FileTextField but handles relative paths</p>
* 
* <p>Copyright: Copyright 2011</p>
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
final class FileTextField2 implements FileTextFieldInterface {
  public static final String rcsid = "$Id:$";

  // Assuming the field type is always non-numeric
  private final FieldType STRING_FIELD_TYPE = FieldType.STRING;

  private final JPanel panel = new JPanel();
  private final GridBagLayout layout = new GridBagLayout();
  private final GridBagConstraints constraints = new GridBagConstraints();

  private final SimpleButton button;
  private final TextField field;
  private final JLabel label;
  private final boolean labeled;
  private final BaseManager manager;

  private List<ResultListener> resultListenerList = null;
  private int fileSelectionMode = -1;
  private FileFilter fileFilter = null;
  private boolean absolutePath = false;
  private boolean useTextAsOriginDir = false;
  private boolean turnOffFileHiding = false;
  private String checkpointValue = null;
  /**
   * If origin is valid, it overrides originEtomoRunDir.
   */
  private File origin = null;
  /**
   * If true, then the origin directory of the file is the directory in which etomo was
   * run.  Useful when a dataset location has not been set.
   */
  private boolean originEtomoRunDir = false;

  private FileTextField2(final BaseManager manager, final String label,
      final boolean labeled, final boolean peet) {
    if (!peet) {
      button = new SimpleButton(new ImageIcon(
          ClassLoader.getSystemResource("images/openFile.gif")));
    }
    else {
      button = new SimpleButton(new ImageIcon(
          ClassLoader.getSystemResource("images/openFilePeet.png")));
    }
    button.setName(label);
    field = new TextField(STRING_FIELD_TYPE, label, null);
    this.label = new JLabel(label);
    this.labeled = labeled;
    this.manager = manager;
  }

  /**
   * Get an unlabeled instance with a PEET-style button.  The starting directory for the
   * file chooser and the origin of relative files is the manager's property user
   * directory.
   * @param manager
   * @param name
   * @return
   */
  static FileTextField2 getUnlabeledPeetInstance(final BaseManager manager,
      final String name) {
    FileTextField2 instance = new FileTextField2(manager, name, false, true);
    instance.createPanel();
    instance.addListeners();
    return instance;
  }

  /**
   * Get a labeled instance with a PEET-style button.  The starting directory for the
   * file chooser and the origin of relative files is the manager's property user
   * directory.
   * @param manager
   * @param name
   * @return
   */
  static FileTextField2 getPeetInstance(final BaseManager manager, final String name) {
    FileTextField2 instance = new FileTextField2(manager, name, true, true);
    instance.createPanel();
    instance.addListeners();
    return instance;
  }

  static FileTextField2 getInstance(final BaseManager manager, final String name) {
    FileTextField2 instance = new FileTextField2(manager, name, true, false);
    instance.createPanel();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // init
    field.setTextPreferredSize(new Dimension(250 * (int) Math.round(UIParameters.INSTANCE
        .getFontSizeAdjustment()), FixedDim.folderButton.height));
    button.setName(label.getText());
    button.setPreferredSize(FixedDim.folderButton);
    button.setMaximumSize(FixedDim.folderButton);
    // panel
    panel.setLayout(layout);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weightx = 0.0;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    if (labeled) {
      layout.setConstraints(label, constraints);
      panel.add(label);
    }
    constraints.insets = new Insets(0, 0, 0, -1);
    layout.setConstraints(field.getComponent(), constraints);
    panel.add(field.getComponent());
    constraints.insets = new Insets(0, -1, 0, 0);
    layout.setConstraints(button, constraints);
    panel.add(button);
  }

  private void addListeners() {
    button.addActionListener(new FileTextField2ActionListener(this));
  }

  /**
   * Adds a result listener to a list of result listeners.  A null listener has no effect.
   * @param listener
   */
  void addResultListener(final ResultListener listener) {
    if (listener == null) {
      return;
    }
    if (resultListenerList == null) {
      resultListenerList = new ArrayList<ResultListener>();
    }
    resultListenerList.add(listener);
  }

  Component getRootPanel() {
    return panel;
  }

  /**
   * @return a label suitable for a message - in single quotes and truncated at the colon.
   */
  String getQuotedLabel() {
    return Utilities.quoteLabel(label.getText());
  }

  /**
   * Opens a file chooser and notifies the result listener list.
   */
  private void action() {
    String filePath = getFileChooserLocation();
    JFileChooser chooser = new FileChooser(new File(filePath));
    chooser.setDialogTitle(Utilities.stripLabel(label.getText()));
    if (fileSelectionMode != -1) {
      chooser.setFileSelectionMode(fileSelectionMode);
    }
    if (fileFilter != null) {
      chooser.setFileFilter(fileFilter);
    }
    chooser.setFileHidingEnabled(!turnOffFileHiding);
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    int returnVal = chooser.showOpenDialog(panel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      setFile(chooser.getSelectedFile());
    }
    if (resultListenerList != null && resultListenerList.size() > 0) {
      Iterator<ResultListener> i = resultListenerList.iterator();
      while (i.hasNext()) {
        i.next().processResult(this);
      }
    }
  }

  /**
   * Sets field width with font adjustment.
   * @param width
   */
  void setAdjustedFieldWidth(final double width) {
    field.setTextPreferredWidth(width * UIParameters.INSTANCE.getFontSizeAdjustment());
  }

  void setAbsolutePath(final boolean input) {
    this.absolutePath = input;
  }

  void setOriginEtomoRunDir(final boolean input) {
    this.originEtomoRunDir = input;
  }

  /**
   * Sets the origin member variable which overrides the originEtomoRunDir member variable
   * and the propertyUserDir when it is a valid directory.
   * @return
   */
  void setOrigin(final File input) {
    origin = input;
  }

  void setOrigin(final String input) {
    if (input != null) {
      origin = new File(input);
    }
  }

  /**
   * If useTextAsOriginDir is true, the text in the text field with be where the file
   * chooser opens, if the text field contains a directory.
   * @param input
   */
  void setUseTextAsOriginDir(final boolean input) {
    useTextAsOriginDir = input;
  }

  boolean isEmpty() {
    String text = field.getText();
    return text == null || text.matches("\\s*");
  }

  boolean isEnabled() {
    return button.isEnabled();
  }

  boolean exists() {
    if (!isEmpty()) {
      return getFile().exists();
    }
    return false;
  }

  public File getFile() {
    if (!isEmpty()) {
      return FilePath.buildAbsoluteFile(getOriginDir(), field.getText());
    }
    return null;
  }

  public boolean equals(final FileTextField2 input) {
    if (input == null) {
      return false;
    }
    File file = getFile();
    File inputFile = input.getFile();
    if (file == null) {
      return inputFile == null;
    }
    return file.equals(inputFile);
  }

  /**
   * Saves the current text as the checkpoint.
   */
  void checkpoint() {
    checkpointValue = getText();
  }

  /**
   * 
   * @param alwaysCheck - check for difference even when the field is disables or invisible
   * @return
   */
  boolean isDifferentFromCheckpoint(final boolean alwaysCheck) {
    if (!alwaysCheck && (!isEnabled() || !panel.isVisible())) {
      return false;
    }
    if (checkpointValue == null) {
      return true;
    }
    return !checkpointValue.equals(getText());
  }

  /**
   * Gets the origin directory.
   * @return
   */
  private String getOriginDir() {
    if (origin != null && origin.exists() && origin.isDirectory()) {
      return origin.getAbsolutePath();
    }
    if (manager == null || originEtomoRunDir) {
      return EtomoDirector.INSTANCE.getOriginalUserDir();
    }
    return manager.getPropertyUserDir();
  }

  private String getFileChooserLocation() {
    if (useTextAsOriginDir) {
      File dir = getFile();
      if (dir != null && dir.isDirectory()) {
        return dir.getAbsolutePath();
      }
    }
    return getOriginDir();
  }

  /**
   * Adds the text of the file path to the field.  The file path will be either absolute 
   * or relative depending on the member variable absolutePath.  The directory will be set 
   * to propertyUserDir, unless the member variable originEtomoRunDir is true.  The 
   * directory will be used as the origin when building a relative file, or when building 
   * an absolute file out of a relative file.
   * @param file
   */
  public void setFile(final File file) {
    if (absolutePath) {
      field.setText(FilePath.buildAbsoluteFile(getOriginDir(), file).getPath());
    }
    else {
      field.setText(FilePath.getRelativePath(getOriginDir(), file));
    }
  }

  /**
   * Sets the file selection mode to be used in the file chooser.
   * @param input
   */
  void setFileSelectionMode(final int input) {
    if (input != FileChooser.FILES_ONLY && input != FileChooser.DIRECTORIES_ONLY
        && input != FileChooser.FILES_AND_DIRECTORIES) {
      System.err.println("WARNING: Incorrect file chooser file selection mode: " + input);
      return;
    }
    fileSelectionMode = input;
  }

  void setTurnOffFileHiding(final boolean input) {
    turnOffFileHiding = input;
  }

  void setFileFilter(final FileFilter input) {
    fileFilter = input;
  }

  public FileFilter getFileFilter() {
    return fileFilter;
  }

  String getText() {
    return field.getText();
  }

  void setText(final String text) {
    field.setText(text);
  }

  void clear() {
    field.setText("");
  }

  void setEnabled(final boolean enabled) {
    field.setEnabled(enabled);
    button.setEnabled(enabled);
  }

  void setToolTipText(String text) {
    field.setToolTipText(text);
    text = TooltipFormatter.INSTANCE.format(text);
    panel.setToolTipText(text);
    button.setToolTipText(text);
  }

  void setFieldToolTipText(final String text) {
    field.setToolTipText(text);
  }

  void setButtonToolTipText(final String text) {
    button.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  private final class FileTextField2ActionListener implements ActionListener {
    private final FileTextField2 adaptee;

    private FileTextField2ActionListener(final FileTextField2 adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action();
    }
  }
}
