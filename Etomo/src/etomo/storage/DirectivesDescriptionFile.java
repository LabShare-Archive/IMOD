package etomo.storage;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.FileType;
import etomo.ui.swing.UIHarness;

/**
* <p>Description: </p>
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
public final class DirectivesDescriptionFile {
  public static final String rcsid = "$Id:$";

  private LogFile logFile = null;

  public static final DirectivesDescriptionFile INSTANCE = new DirectivesDescriptionFile();

  public void releaseIterator(Iterator iterator) {
    if (logFile != null) {
      logFile.closeRead(iterator.id);
    }
  }

  /**
   * Opens the directives description file if necessary and returns an iterator for the
   * file.  When done with the iterator, call releaseIterator.
   * @param manager
   * @param axisID
   * @return
   */
  public Iterator getIterator(final BaseManager manager, final AxisID axisID) {
    if (logFile == null) {
      if (!open(manager, axisID)) {
        return null;
      }
    }
    try {
      return new Iterator(manager, axisID, logFile, logFile.openReader());
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to get a reader for "
          + FileType.DIRECTIVES_DESCR.getFile(manager, axisID).getAbsolutePath() + ".\n"
          + e.getMessage(), "Open File Failed", axisID);
      return null;
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to get a reader for "
          + FileType.DIRECTIVES_DESCR.getFile(manager, axisID).getAbsolutePath() + ".\n"
          + e.getMessage(), "Open File Failed", axisID);
      return null;
    }
  }

  private synchronized boolean open(final BaseManager manager, final AxisID axisID) {
    if (logFile != null) {
      return true;
    }
    File file = FileType.DIRECTIVES_DESCR.getFile(manager, axisID);
    try {
      logFile = LogFile.getInstance(file);
      return true;
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Unable to open " + file.getAbsolutePath() + ".\n" + e.getMessage(),
          "Open File Failed", axisID);
      return false;
    }
  }

  static final class EtomoColumn {
    static final EtomoColumn BLANK = new EtomoColumn(null);
    static final EtomoColumn NE = new EtomoColumn("NE");
    static final EtomoColumn NES = new EtomoColumn("NES");
    static final EtomoColumn SD = new EtomoColumn("SD");
    static final EtomoColumn SO = new EtomoColumn("SO");

    private final String tag;

    private EtomoColumn(final String tag) {
      this.tag = tag;
    }

    static EtomoColumn getInstance(String input) {
      if (input == null || input.equals("")) {
        return BLANK;
      }
      if (input.equals(NE.tag)) {
        return NE;
      }
      if (input.equals(NES.tag)) {
        return NES;
      }
      if (input.equals(SD.tag)) {
        return SD;
      }
      if (input.equals(SO.tag)) {
        return SO;
      }
      return null;
    }
  }

  static interface DirectiveDescription {
    String getDescription();

    EtomoColumn getEtomoColumn();

    String getName();

    DirectiveValueType getValueType();

    boolean isBatch();

    boolean isTemplate();
  }

  /**
   * Readonly iterator with line filtering and parsing.
   * @author sueh
   *
   */
  public static final class Iterator implements DirectiveDescription {
    private final AxisID axisID;
    private final LogFile.ReaderId id;
    private final LogFile logFile;
    private final BaseManager manager;

    private String[] curLineArray = null;
    private String nextLine = null;
    private boolean started = false;

    private Iterator(final BaseManager manager, final AxisID axisID,
        final LogFile logFile, final LogFile.ReaderId id) {
      this.manager = manager;
      this.axisID = axisID;
      this.logFile = logFile;
      this.id = id;
    }

    public String getDescription() {
      if (curLineArray != null && curLineArray.length > 1) {
        return curLineArray[1];
      }
      return null;
    }

    public DirectiveDescription getDirectiveDescription() {
      return this;
    }

    public EtomoColumn getEtomoColumn() {
      if (curLineArray != null && curLineArray.length > 5) {
        return EtomoColumn.getInstance(curLineArray[5]);
      }
      return null;
    }

    public String getName() {
      if (curLineArray != null && curLineArray.length > 0) {
        return curLineArray[0];
      }
      return null;
    }

    public String getSectionHeader() {
      if (curLineArray != null && curLineArray.length > 0) {
        return curLineArray[0];
      }
      return null;
    }

    public DirectiveValueType getValueType() {
      if (curLineArray != null && curLineArray.length > 2) {
        return DirectiveValueType.getInstance(curLineArray[2]);
      }
      return null;
    }

    /**
     * Moves to the next line which starts with directiveType.
     * @return true if a line which meets to criteria is found
     */
    public boolean hasNext() {
      try {
        if (!started) {
          // Get past the file headers
          logFile.readLine(id);
          logFile.readLine(id);
          logFile.readLine(id);
          started = true;
        }
        // hasNext has been run, but next was not, so line isn't incremented.
        if (nextLine != null) {
          return true;
        }
        do {
          nextLine = logFile.readLine(id);
          if (nextLine == null) {
            return false;
          }
          // Ignore empty lines
        } while (nextLine.equals(""));
        return true;
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
        UIHarness.INSTANCE.openMessageDialog(manager, "Unable to read "
            + FileType.DIRECTIVES_DESCR.getFile(manager, axisID).getAbsolutePath()
            + ".\n" + e.getMessage(), "Open File Failed", axisID);
        return false;
      }
      catch (IOException e) {
        e.printStackTrace();
        UIHarness.INSTANCE.openMessageDialog(manager, "Unable to read "
            + FileType.DIRECTIVES_DESCR.getFile(manager, axisID).getAbsolutePath()
            + ".\n" + e.getMessage(), "Open File Failed", axisID);
        return false;
      }
    }

    public boolean isBatch() {
      if (curLineArray != null && curLineArray.length > 3) {
        return curLineArray[3].compareToIgnoreCase("Y") != 0;
      }
      return false;
    }

    public boolean isSection() {
      if (curLineArray != null && curLineArray.length == 1) {
        return true;
      }
      return false;
    }

    public boolean isTemplate() {
      if (curLineArray != null && curLineArray.length > 4) {
        return curLineArray[4].compareToIgnoreCase("Y") != 0;
      }
      return false;
    }

    /**
     * Increment the line, and null out nextLine
     */
    public void next() {
      curLineArray = nextLine.split(",");
      nextLine = null;
    }

  }
}
