package etomo.storage;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import etomo.BaseManager;
import etomo.type.DirectiveFileType;
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
      if (logFile.exists()) {
        logFile.backup();
      }
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
   * @param droppedDirectives - a list of directives that could not be edited (written as comments last).
   */
  public void write(final DirectiveFileType type, final List<String> comments,
      final List<Directive> directiveList, final List<String> droppedDirectives) {
    if (logFile == null || id == null) {
      return;
    }
    try {
      logFile.write("# " + type.getLabel(), id);
      logFile.newLine(id);
      if (comments != null) {
        Iterator<String> iterator = comments.iterator();
        while (iterator.hasNext()) {
          logFile.write("# " + iterator.next(), id);
          logFile.newLine(id);
        }
        logFile.newLine(id);
      }
      if (directiveList != null) {
        Iterator<Directive> iterator = directiveList.iterator();
        while (iterator.hasNext()) {
          Directive directive = iterator.next();
          if (directive.isInclude()) {
            directive.write(logFile, id);
          }
        }
      }
      if (droppedDirectives != null && !droppedDirectives.isEmpty()) {
        Iterator<String> iterator = droppedDirectives.iterator();
        if (iterator != null && iterator.hasNext()) {
          logFile.newLine(id);
          logFile.write("# Directives that could not be included:", id);
          logFile.newLine(id);
          while (iterator.hasNext()) {
            logFile.write("# " + iterator.next(), id);
            logFile.newLine(id);
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
