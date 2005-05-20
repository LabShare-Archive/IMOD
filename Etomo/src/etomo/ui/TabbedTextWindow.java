package etomo.ui;

import java.awt.Container;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;
import javax.swing.text.StyledEditorKit;

import etomo.type.AxisID;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.4  2005/05/20 03:25:03  sueh
 * <p> bug# 664 In openFiles():  Not displaying align.log if it is over 100k.
 * <p> Catching and recovering from an OutOfMemoryError by removing the
 * <p> file which caused the problem.  This functionality will only be used in
 * <p> Java 1.5 because 1.4 doesn't throw the OutOfMemoryError quickly
 * <p> enough to catch it here.
 * <p>
 * <p> Revision 3.3  2004/04/08 19:07:53  rickg
 * <p> Bug #422 added setDefaultCloseOperation call to constructor
 * <p>
 * <p> Revision 3.2  2003/11/27 00:04:43  rickg
 * <p> Bug# 366 Close file reader when done
 * <p>
 * <p> Revision 3.1  2003/11/10 07:45:23  rickg
 * <p> Task tags moved to bugzilla
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.2  2003/06/04 23:38:52  rickg
 * <p> Added independent labels for tabs
 * <p>
 * <p> Revision 1.1  2003/05/27 08:50:45  rickg
 * <p> Initial revision
 * <p>
 * <p> </p>
 */
public class TabbedTextWindow extends JFrame {
  public static final String rcsid =
    "$Id$";

  private Container mainPanel;
  private JTabbedPane tabPane = new JTabbedPane();

  public TabbedTextWindow(String label) {
    mainPanel = getContentPane();
    mainPanel.add(tabPane);
    setTitle(label);
    setSize(625, 800);
    setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
  }

  /**
   * Open the array of files
   * @param files
   * @throws IOException
   * @throws FileNotFoundException
   */
  public void openFiles(String[] files, String[] labels, AxisID axisID)
      throws IOException, FileNotFoundException {
    FileReader reader;
    int nFiles = files.length;
    for (int i = 0; i < files.length; i++) {
      JEditorPane editorPane = new JEditorPane();
      editorPane.setEditorKit(new StyledEditorKit());
      JScrollPane scrollPane = new JScrollPane(editorPane);
      File file = new File(files[i]);
      try {
        tabPane.add(labels[i], scrollPane);
        //Do not display align.log if it is over 100k.
        //Do not display taResiduals.log if it is over .5m.
        //Do not display tsSolution.log if it is over .5m.
        if ((file.length() > .5 * 1024.0 * 1024.0 && (file.getName()
            .startsWith("taResiduals") || file.getName().startsWith(
            "taSolution")))
            || (file.length() > 100 * 1024 && file.getName()
                .startsWith("align"))) {
          editorPane.setText(file.getName() + " is too large to display");
        }
        else {
          reader = new FileReader(file);
          editorPane.read(reader, file);
          reader.close();
        }
        editorPane.setEditable(false);
      }
      catch (OutOfMemoryError e) {
        e.printStackTrace();
        if (tabPane != null) {
          tabPane.removeAll();
        }
        UIHarness.INSTANCE.openMessageDialog(
            "WARNING:  Ran out of memory.  Will not display log file."
                + "\nPlease close open windows or exit Etomo.",
            "Out of Memory");
        throw e;
      }
    }
  }
}