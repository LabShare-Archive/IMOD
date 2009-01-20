package etomo.ui;

import java.io.File;

import javax.swing.JFileChooser;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.UITestFieldType;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
final class FileChooser extends JFileChooser {
  public static final String rcsid = "$Id$";

  public FileChooser(File currentDirectory) {
    super(currentDirectory);
  }

  public void setDialogTitle(String dialogTitle) {
    super.setDialogTitle(dialogTitle);
    setName(dialogTitle);
  }

  public void setName(String text) {
    String name = Utilities.convertLabelToName(text);
    super.setName(name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(UITestFieldType.MENU_ITEM.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }

}
