package etomo.ui.swing;

import java.awt.Component;

import javax.swing.JTabbedPane;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.UITestFieldType;
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

  /**
   * Adds a tab, sets the name of the pane on the first tab.  Sets the name of
   * the component, if it does not have a name.
   */
  public void addTab(String title, Component component) {
    super.addTab(title, component);
    if (getTabCount() == 1) {
      setName(title);
    }
  }

  /**
   * Adds a tab, sets the name of the pane on the first tab.  Sets the name of
   * the component, if it does not have a name.
   */
  public void addTab(String title, SpacedPanel spacedPanel) {
    super.addTab(title, spacedPanel.getContainer());
    if (getTabCount() == 1) {
      setName(title);
    }
  }

  public void setTitleAt(int index, String title) {
    super.setTitleAt(index, title);
    if (index == 0) {
      setName(title);
    }
  }

  public void setName(String text) {
    String name = Utilities.convertLabelToName(text);
    super
        .setName(UITestFieldType.TAB.toString() + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.7  2009/11/20 17:37:40  sueh
 * <p> bug# 1282 Added prefixes to all of the field names, so that the fields that
 * <p> are actually abstract buttons (radio buttons, etc) won't be activated by a
 * <p> "bn." field command.
 * <p>
 * <p> Revision 1.6  2009/01/20 20:31:03  sueh
 * <p> bug# 1102 Added setName.
 * <p>
 * <p> Revision 1.5  2008/05/30 22:36:28  sueh
 * <p> bug# 1102 Isolating the etomo.uitest package so it is not need for
 * <p> running EtomoDirector.
 * <p>
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
