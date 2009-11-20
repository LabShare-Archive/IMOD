package etomo.ui;

import java.io.File;

import javax.swing.JFileChooser;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
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
 * <p> $Log$
 * <p> Revision 1.1  2009/01/20 20:02:12  sueh
 * <p> bug# 1102 A self-naming JFileChooser.
 * <p> </p>
 */
public final class FileChooser extends JFileChooser {
  public static final String rcsid = "$Id$";

  public FileChooser(File currentDirectory) {
    super(currentDirectory);
    setName("Open");
  }

  public void setDialogTitle(String dialogTitle) {
    super.setDialogTitle(dialogTitle);
    setName(dialogTitle);
  }

  public void setName(String text) {
    String name = Utilities.convertLabelToName(text);
    super.setName(name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER
          + ' ');
    }
  }
}
