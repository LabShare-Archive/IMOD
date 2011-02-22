package etomo.ui.swing;

import java.awt.Container;
import java.awt.Font;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;

import etomo.BaseManager;
import etomo.EtomoDirector;
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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.9  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.8  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.7  2008/03/22 00:17:32  sueh
 * <p> bug# 1099 In openFiles not using StyledEditorKit because it takes up a lot
 * <p> of memory and mostly runs from the event loop, meaning the resulting
 * <p> memory errors cannot be caught and cause Etomo to lock up.  In
 * <p> checkSize increased file size limits and increased the padding to a
 * <p> minimum of 15MB, which where Redhat starts to have problems.
 * <p>
 * <p> Revision 3.6  2008/03/19 01:01:44  sueh
 * <p> bug# 1099 Added checkSize and display.
 * <p>
 * <p> Revision 3.5  2005/05/20 21:20:43  sueh
 * <p> bug# 664 Attempting to recover from OutOfMemoryError, version 2:
 * <p> call logFileWindow.dispose() when an OutOfMemoryError is caught.
 * <p> There is no guarentee that this will solve the problem or that the error will
 * <p> be caught in this place.  Tell the user to close windows or Etomo and
 * <p> rethrow the error.
 * <p> Also do not open the residuals or solution logs if they are over .5 mb.
 * <p>
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
final class TabbedTextWindow extends JFrame {
  public static final String rcsid = "$Id$";

  private final AxisID axisID;
  private final String label;

  boolean displayWholeLog = true;
  boolean displayResiduals = true;
  boolean displaySolutions = true;
  boolean displayEverythingElse = true;

  TabbedTextWindow(String label, AxisID axisID) {
    this.label = label;
    this.axisID = axisID;
  }

  /**
   * Open the array of files
   * @param files
   * @throws IOException
   * @throws FileNotFoundException
   */
  boolean openFiles(BaseManager manager, String[] files, String[] labels, AxisID axisID)
      throws IOException, FileNotFoundException {
    checkSize(files);
    StringBuffer error = null;
    JTabbedPane tabPane = null;
    //DisplayEverythingElse is the last boolean to be turned off if there is a
    //memory limitation, so use to decide if a tabbed pane should be displayed.
    if (displayEverythingElse) {
      tabPane = new JTabbedPane();
      Container mainPanel = getContentPane();
      mainPanel.add(tabPane);
      setTitle(label);
      setSize(625, 800);
      setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    }
    else {
      error = new StringBuffer();
      error.append("Unable to display log files:  ");
    }
    for (int i = 0; i < files.length; i++) {
      File file = new File(files[i]);
      String fileName = file.getName();
      if (displayEverythingElse) {
        JEditorPane editorPane = new JEditorPane();
        editorPane.setFont(new Font("monospaced", Font.PLAIN, 12));
        JScrollPane scrollPane = new JScrollPane(editorPane);
        try {
          tabPane.add(labels[i], scrollPane);
          if (fileName.startsWith("align")) {
            display(displayWholeLog, editorPane, file);
          }
          else if (fileName.startsWith("taResiduals")) {
            display(displayResiduals, editorPane, file);
          }
          else if (fileName.startsWith("taSolution")) {
            display(displaySolutions, editorPane, file);
          }
          else {
            display(true, editorPane, file);
          }
        }
        catch (OutOfMemoryError e) {
          e.printStackTrace();
          if (tabPane != null) {
            tabPane.removeAll();
          }
          UIHarness.INSTANCE.openMessageDialog(manager,
              "WARNING:  Ran out of memory.  Will not display log file."
                  + "\nPlease close open log file windows or exit Etomo.",
              "Out of Memory");
          throw e;
        }
      }
      else {
        error.append(fileName);
        if (i < files.length - 1) {
          error.append(", ");
        }
      }
    }
    if (!displayEverythingElse) {
      error.append(".  Not enough available memory.  Close unnecessary windows.");
      UIHarness.INSTANCE.openMessageDialog(manager, error.toString(),
          "Memory Limitation", axisID);
    }
    return displayEverythingElse;
  }

  /**
   * Decide which files to display.  Do not display the whole log if it is
   * over 200K.  Do not display the residual log if it is over 1MB.  Do not
   * display the solutions log if it is too big to fit into the available
   * memory.  Do not display all the other logs if they are too big to fit into
   * the available memory.
   * @param files
   */
  private void checkSize(String[] files) {
    long wholeLogSize = 0;
    long residualsSize = 0;
    long solutionsSize = 0;
    for (int i = 0; i < files.length; i++) {
      File file = new File(files[i]);
      String fileName = file.getName();
      if (fileName.startsWith("align")) {
        wholeLogSize = file.length();
        if (wholeLogSize <= 200 * 1024) {
          return;
        }
      }
      else if (fileName.startsWith("taResiduals")) {
        residualsSize = file.length();
      }
      else if (fileName.startsWith("taSolution")) {
        solutionsSize = file.length();
      }
      if (wholeLogSize > 0 && residualsSize > 0 && solutionsSize > 0) {
        break;
      }
    }
    //Whole log is bigger then 200k
    displayWholeLog = false;
    long everythingElseSize = wholeLogSize - residualsSize - solutionsSize;
    if (residualsSize > 1 * 1024.0 * 1024.0) {
      //Residuals log is bigger then 1MB
      displayResiduals = false;
      residualsSize = 0;
    }
    long memory = EtomoDirector.INSTANCE.getAvailableMemory();
    //Available is available memory - padding.
    //In linux java begins to get unreliable when it has between 14MB -17MB
    //memory available.
    long dangerArea = 15 * 1024 * 1024;
    long available = memory - Math.max(dangerArea, everythingElseSize * 3);
    //AverageFactor was calculated for bug# 1099 by opening log files and taking
    //the average.
    int overhead = 8;
    if (available >= overhead * (residualsSize + solutionsSize + everythingElseSize)) {
      return;
    }
    //Not enough space.  Turn off residuals if they are still being displayed.
    if (displayResiduals) {
      displayResiduals = false;
      if (available >= overhead * (solutionsSize + everythingElseSize)) {
        return;
      }
    }
    //Still not enough space.  Turn off solutions.
    displaySolutions = false;
    //Reduce the padding for minimal log file display.
    available = memory - dangerArea;
    if (available >= overhead * everythingElseSize) {
      return;
    }
    //Not enough space.  Turn off everything.
    displayEverythingElse = false;
  }

  /**forum.java.sun.com/thread.jspa?threadID=5269840&messageID=10133229
   * Read a file into the editor pane.  If displayFile is false, place an error
   * message in the editor pane instead.
   * @param displayFile
   * @param editorPane
   * @param file
   * @throws IOException
   * @throws FileNotFoundException
   */
  private void display(boolean displayFile, JEditorPane editorPane, File file)
      throws IOException, FileNotFoundException {
    if (displayFile) {
      FileReader reader = new FileReader(file);
      editorPane.read(reader, file);
      reader.close();
    }
    else {
      editorPane.setText(file.getName() + " is too large to display");
    }
    editorPane.setEditable(false);
  }
}