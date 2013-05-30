package etomo.storage;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.ui.swing.UIHarness;

/**
* <p>Description: Writes a list of directives to a file.</p>
* 
* <p>Copyright: Copyright 2013</p>
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
public final class DirectiveWriter {
  public static final String rcsid = "$Id:$";

  private final File saveFile;
  private final BaseManager manager;

  private LogFile logFile = null;
  private LogFile.WriterId id = null;

  public DirectiveWriter(final BaseManager manager, final File saveFile) {
    this.saveFile = saveFile;
    this.manager = manager;
  }

  /**
   * Open the file.
   * @return
   */
  public boolean open() {
    if (saveFile == null) {
      UIHarness.INSTANCE.openMessageDialog(manager, "No file set.",
          "Failed to Open Writer");
      return false;
    }
    try {
      logFile = LogFile.getInstance(saveFile);
      id = logFile.openWriter();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Unable to open file:  " + saveFile.getAbsolutePath() + ".  " + e.getMessage(),
          "Unable to Open File");
      return false;
    }
    catch (IOException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Unable to open file:  " + saveFile.getAbsolutePath() + ".  " + e.getMessage(),
          "Unable to Open File");
      return false;
    }
    return true;
  }

  /**
   * Write to the file.  Directives will only be written if at least one of their include
   * booleans is checked.  If the "Any" (axisID == null) include boolean is checked, the
   * A and B forms will not be written.  Writing is halted if an exception is detected.
   * @param comments - (optional) written first
   * @param directiveList - (optional) written second
   */
  public void write(final List<String> comments, final List<Directive> directiveList) {
    if (logFile == null || id == null) {
      return;
    }
    try {
      if (comments != null) {
        Iterator<String> iterator = comments.iterator();
        while (iterator.hasNext()) {
          logFile.write("# " + iterator.next(), id);
          logFile.newLine(id);
        }
      }
      if (directiveList != null) {
        Iterator<Directive> iterator = directiveList.iterator();
        while (iterator.hasNext()) {
          Directive directive = iterator.next();
          if (directive.isInclude(null)) {
            directive.write(null, logFile, id);
          }
          else {
            if (directive.isInclude(AxisID.FIRST)) {
              directive.write(AxisID.FIRST, logFile, id);
            }
            if (directive.isInclude(AxisID.SECOND)) {
              directive.write(AxisID.SECOND, logFile, id);

            }
          }
        }
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to write to file:  "
          + saveFile.getAbsolutePath() + ".  " + e.getMessage(),
          "Unable to Write to File");
    }
    catch (IOException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to write to file:  "
          + saveFile.getAbsolutePath() + ".  " + e.getMessage(),
          "Unable to Write to File");
    }
  }

  /**
   * Close the file.
   */
  public void close() {
    if (logFile != null && id != null) {
      logFile.closeWriter(id);
    }
  }
}
