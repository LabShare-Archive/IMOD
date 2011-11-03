package etomo.ui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JPanel;

import etomo.BaseManager;
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

  private final JPanel panel = new JPanel();
  private final GridBagLayout layout = new GridBagLayout();
  private final GridBagConstraints constraints = new GridBagConstraints();
  private final SimpleButton button = new SimpleButton(new ImageIcon(
      ClassLoader.getSystemResource("images/openFile.gif")));

  private final TextField field;
  private final String propertyUserDir;
  private final String name;

  private String currentDirectory = null;

  private FileTextField2(final BaseManager manager, final String name) {
    field = new TextField(name);
    this.name = name;
    propertyUserDir = manager.getPropertyUserDir();
  }

  static FileTextField2 getInstance(final BaseManager manager, final String name) {
    FileTextField2 instance = new FileTextField2(manager, name);
    instance.createPanel();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // init
    field.setTextPreferredSize(new Dimension(250 * Math.round(UIParameters.INSTANCE
        .getFontSizeAdjustment()), FixedDim.folderButton.height));
    button.setName(name);
    button.setPreferredSize(FixedDim.folderButton);
    button.setMaximumSize(FixedDim.folderButton);
    // panel
    panel.setLayout(layout);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weightx = 0.0;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
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

  Component getRootPanel() {
    return panel;
  }

  private void action() {
    JFileChooser chooser = new FileChooser(new File(
        currentDirectory == null ? propertyUserDir : currentDirectory));
    chooser.setDialogTitle(name);
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    int returnVal = chooser.showOpenDialog(panel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      setFile(chooser.getSelectedFile());
    }
  }

  void setFieldWidth(final double width) {
    field.setTextPreferredWidth(width);
  }

  boolean isEmpty() {
    return field.getText() == null || field.getText().matches("\\s*");
  }

  boolean exists() {
    return Utilities.getFileFromPath(propertyUserDir, field.getText()).exists();
  }

  public File getFile() {
    return Utilities.getFileFromPath(propertyUserDir, field.getText());
  }

  public void setFile(final File file) {
    field.setText(FilePath.getRelativePath(propertyUserDir, file));
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
