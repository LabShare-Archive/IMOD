package etomo.ui;

import java.awt.Component;

import javax.swing.JTabbedPane;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.UITestField;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
final class TabbedPane extends JTabbedPane {
  public static final String rcsid = "$Id$";

  public void addTab(String title, Component component) {
    super.addTab(title, component);
    int tabCount = getTabCount();
    String name;
    if (tabCount < 0) {
      throw new IllegalStateException("tabCount="+tabCount);
    }
    if (tabCount == 1) {
      name = Utilities.convertLabelToName(title);
      setName(name);
    }
    else {
      name = getName();
    }
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(UITestField.TABBED_PANE.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name
          + AutodocTokenizer.SEPARATOR_CHAR + (tabCount - 1) + " "
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.4  2008/05/30 21:34:46  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.3  2007/12/26 22:34:57  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.2  2007/09/07 00:29:08  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.1  2006/04/25 19:23:04  sueh
 * <p> bug# 787 Made a subclass of JTabbedPane so tabbed panes could be
 * <p> named automatically.  The name is the first tab.
 * <p> </p>
 */