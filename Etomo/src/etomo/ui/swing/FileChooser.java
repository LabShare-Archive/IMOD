package etomo.ui.swing;

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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.2  2009/11/20 17:11:29  sueh
 * <p> bug# 1282 The default title of a file chooser is "Open", so naming
 * <p> instances of this class "open" as the default.
 * <p>
 * <p> Revision 1.1  2009/01/20 20:02:12  sueh
 * <p> bug# 1102 A self-naming JFileChooser.
 * <p> </p>
 */
public final class FileChooser extends JFileChooser {
  public static final String rcsid = "$Id$";

  static final String DEFAULT_TITLE = "Open";

  public FileChooser(File currentDirectory) {
    super(currentDirectory);
    setName(DEFAULT_TITLE);
  }

  public void setDialogTitle(String dialogTitle) {
    super.setDialogTitle(dialogTitle);
    setName(dialogTitle);
  }

  public void setName(String text) {
    String name = Utilities.convertLabelToName(text);
    super.setName(name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
}
