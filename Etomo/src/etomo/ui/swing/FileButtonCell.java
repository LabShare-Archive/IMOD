package etomo.ui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

import etomo.storage.ExtensibleFileFilter;
import etomo.type.UITestFieldType;

/**
* <p>Description: </p>
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
final class FileButtonCell extends InputCell {
  public static final String rcsid = "$Id:$";

  private final CurrentDirectory currentDirectory;

  //Field that this button is associated with:
  private ButtonTarget buttonTarget = null;
  private String label = "Open File";
  private FileFilter fileFilter = null;

  private final SimpleButton button = new SimpleButton(new ImageIcon(
      ClassLoader.getSystemResource("images/openFile.gif")));

  FileButtonCell(final CurrentDirectory currentDirectory) {
    this.currentDirectory = currentDirectory;
  }

  static FileButtonCell getInstance(final FileButtonCell fileButtonCell) {
    FileButtonCell instance = new FileButtonCell(fileButtonCell.currentDirectory);
    instance.label = fileButtonCell.label;
    instance.fileFilter = fileButtonCell.fileFilter;
    instance.init();
    instance.addListeners();
    return instance;
  }

  static FileButtonCell getInstance(final CurrentDirectory currentDirectory,
      final String label) {
    FileButtonCell instance = new FileButtonCell(currentDirectory);
    instance.init();
    instance.addListeners();
    return instance;
  }

  private void init() {
    int size = 18;
    Dimension dim = new Dimension(size, size);
    button.setPreferredSize(dim);
    button.setMaximumSize(dim);
  }

  private void addListeners() {
    button.addActionListener(new FileButtonActionListener(this));
  }

  void addTarget(final ButtonTarget input) {
    buttonTarget = input;
  }

  void setHeaders(String tableHeader, HeaderCell rowHeader, HeaderCell columnHeader) {
    label = columnHeader.getText();
    super.setHeaders(tableHeader, rowHeader, columnHeader);
  }

  void setFileFilter(final FileFilter input) {
    fileFilter = input;
  }

  void setFileFilter(final ExtensibleFileFilter input) {
    fileFilter = input;
  }

  Component getComponent() {
    return button;
  }

  UITestFieldType getFieldType() {
    return UITestFieldType.BUTTON;
  }

  int getWidth() {
    return button.getSize().width;
  }

  public void setEnabled(final boolean enabled) {
    button.setEnabled(enabled);
  }

  private void action() {
    JFileChooser chooser = new FileChooser(currentDirectory.getCurrentDirectory());
    chooser.setDialogTitle(label);
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    if (fileFilter != null) {
      chooser.setFileFilter(fileFilter);
    }
    int returnVal = chooser.showOpenDialog(button.getParent());
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      if (buttonTarget != null) {
        File file = chooser.getSelectedFile();
        if (file != null) {
          buttonTarget.setFile(file);
          currentDirectory.setCurrentDirectory(file.getParentFile());
        }
      }
    }
  }

  void setToolTipText(String text) {
    button.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  private static final class FileButtonActionListener implements ActionListener {
    private final FileButtonCell fileButtonCell;

    private FileButtonActionListener(final FileButtonCell fileButtonCell) {
      this.fileButtonCell = fileButtonCell;
    }

    public void actionPerformed(final ActionEvent event) {
      fileButtonCell.action();
    }
  }
}
