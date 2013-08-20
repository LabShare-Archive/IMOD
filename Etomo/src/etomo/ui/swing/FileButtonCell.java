package etomo.ui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.border.BevelBorder;
import javax.swing.filechooser.FileFilter;

import etomo.EtomoDirector;
import etomo.storage.ExtensibleFileFilter;
import etomo.storage.autodoc.AutodocTokenizer;
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

  // Field that this button is associated with:
  private ActionTarget actionTarget = null;
  private String label = null;
  private FileFilter fileFilter = null;

  private final SimpleButton button = new SimpleButton(new ImageIcon(
      ClassLoader.getSystemResource("images/openFilePeet.png")));

  private FileButtonCell(final CurrentDirectory currentDirectory) {
    this.currentDirectory = currentDirectory;
    button.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
    // button.setBorder(BorderFactory.createEtchedBorder());
    Dimension size = button.getPreferredSize();
    if (size.width < size.height) {
      size.width = size.height;
    }
    button.setSize(size);
  }

  static FileButtonCell getInstance(final FileButtonCell fileButtonCell) {
    FileButtonCell instance = new FileButtonCell(fileButtonCell.currentDirectory);
    instance.label = fileButtonCell.label;
    instance.fileFilter = fileButtonCell.fileFilter;
    instance.addListeners();
    return instance;
  }

  static FileButtonCell getInstance(final CurrentDirectory currentDirectory) {
    FileButtonCell instance = new FileButtonCell(currentDirectory);
    instance.addListeners();
    return instance;
  }

  public void add(JPanel panel, GridBagLayout layout, GridBagConstraints constraints) {
    double oldWeightx = constraints.weightx;
    constraints.weightx = 0.0;
    super.add(panel, layout, constraints);
    constraints.weightx = oldWeightx;
  }

  private void addListeners() {
    button.addActionListener(new FileButtonActionListener(this));
  }

  void setActionTarget(final ActionTarget input) {
    actionTarget = input;
  }

  void setHeaders(String tableHeader, HeaderCell rowHeader, HeaderCell columnHeader) {
    if (label == null) {
      label = columnHeader.getText();
    }
    super.setHeaders(tableHeader, rowHeader, columnHeader);
  }

  void setName() {
    String name = convertLabelToName();
    button.setName(name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(getComponent().getName() + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }

  void setLabel(final String input) {
    label = input;
  }

  void setFileFilter(final FileFilter input) {
    fileFilter = input;
  }

  void setFileFilter(final ExtensibleFileFilter input) {
    fileFilter = input;
  }

  FileFilter getFileFilter() {
    return fileFilter;
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
    chooser.setDialogTitle(label == null ? "Open File" : label);
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    if (fileFilter != null) {
      chooser.setFileFilter(fileFilter);
    }
    int returnVal = chooser.showOpenDialog(button.getParent());
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File file = chooser.getSelectedFile();
      if (actionTarget != null) {
        actionTarget.setTargetFile(file);
      }
      if (currentDirectory != null && file != null) {
        currentDirectory.setCurrentDirectory(file.getParentFile());
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
