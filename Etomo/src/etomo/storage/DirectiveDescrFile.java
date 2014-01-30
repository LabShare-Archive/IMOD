package etomo.storage;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.Arguments.DebugLevel;
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
public final class DirectiveDescrFile {
  public static final String rcsid = "$Id:$";

  private LogFile logFile = null;
  private File alternateFile = null;

  public static final DirectiveDescrFile INSTANCE = new DirectiveDescrFile();

  private DirectiveDescrFile() {
  }

  public void releaseIterator(Iterator iterator) {
    if (logFile != null) {
      logFile.closeRead(iterator.id);
    }
    if (!logFile.isOpen()) {
      logFile = null;
    }
  }

  /**
   * Sets an alternative directives description file.  Pass null to remove the alternative
   * file.  Has no effect while the log file is in use.
   * @param input
   * @return true if alternateFile
   */
  boolean setFile(final File input) {
    if (input == null) {
      alternateFile = null;
    }
    else if (logFile != null) {
      System.err.println("Warning: unable to use " + input.getAbsolutePath()
          + " as the directives description file because the default file is already "
          + "in use.");
      return false;
    }
    alternateFile = input;
    return true;
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
    File file;
    if (alternateFile == null) {
      file = FileType.DIRECTIVES_DESCR.getFile(manager, axisID);
    }
    else {
      file = alternateFile;
    }
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

  /**
   * Readonly iterator
   * @author sueh
   */
  public static final class Iterator implements DirectiveDescr {
    private static final int NAME_COLUMN_INDEX = 0;
    private static final int DESCR_COLUMN_INDEX = 1;
    private static final int VALUE_TYPE_COLUMN_INDEX = 2;
    private static final int BATCH_COLUMN_INDEX = 3;
    private static final int TEMPLATE_COLUMN_INDEX = 4;
    private static final int ETOMO_COLUMN_INDEX = 5;
    private static final int LABEL_COLUMN_INDEX = 7;
    private static final int CHOICES_COLUMN_INDEX = 8;

    private final AxisID axisID;
    private final LogFile.ReaderId id;
    private final LogFile logFile;
    private final BaseManager manager;

    private String[] curLineArray = null;
    private String nextLine = null;

    private Iterator(final BaseManager manager, final AxisID axisID,
        final LogFile logFile, final LogFile.ReaderId id) {
      this.manager = manager;
      this.axisID = axisID;
      this.logFile = logFile;
      this.id = id;
    }

    public String getDescription() {
      if (curLineArray != null && curLineArray.length > DESCR_COLUMN_INDEX) {
        return curLineArray[DESCR_COLUMN_INDEX];
      }
      return null;
    }

    public String getLabel() {
      if (curLineArray != null && curLineArray.length > LABEL_COLUMN_INDEX) {
        return curLineArray[LABEL_COLUMN_INDEX];
      }
      return null;
    }

    public ChoiceList getChoiceList(final DebugLevel debug) {
      if (curLineArray != null && curLineArray.length > CHOICES_COLUMN_INDEX) {
        return new ChoiceList(curLineArray[CHOICES_COLUMN_INDEX], debug);
      }
      return null;
    }

    public DirectiveDescr getDirectiveDescription() {
      return this;
    }

    public DirectiveDescrEtomoColumn getEtomoColumn() {
      if (curLineArray != null && curLineArray.length > ETOMO_COLUMN_INDEX) {
        return DirectiveDescrEtomoColumn.getInstance(curLineArray[ETOMO_COLUMN_INDEX]);
      }
      return null;
    }

    public String getName() {
      if (curLineArray != null && curLineArray.length > NAME_COLUMN_INDEX) {
        return curLineArray[0];
      }
      return null;
    }

    public String getSectionHeader() {
      if (curLineArray != null && curLineArray.length > NAME_COLUMN_INDEX) {
        return curLineArray[NAME_COLUMN_INDEX];
      }
      return null;
    }

    public DirectiveValueType getValueType() {
      if (curLineArray != null && curLineArray.length > VALUE_TYPE_COLUMN_INDEX) {
        return DirectiveValueType.getInstance(curLineArray[VALUE_TYPE_COLUMN_INDEX]);
      }
      return DirectiveValueType.UNKNOWN;
    }

    /**
     * Moves to the next line if nextLine is null.
     * @return true if there is another line to read and nextLine has been set
     */
    public boolean hasNext() {
      // hasNext has been run, but next was not, so line isn't incremented.
      if (nextLine != null) {
        return true;
      }
      try {
        nextLine = logFile.readLine(id);
        return nextLine != null;
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
      if (curLineArray != null && curLineArray.length > BATCH_COLUMN_INDEX) {
        return curLineArray[BATCH_COLUMN_INDEX].trim().compareToIgnoreCase("Y") == 0;
      }
      return false;
    }

    public boolean isDirective() {
      if (curLineArray != null && curLineArray.length >= 3) {
        return true;
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
      if (curLineArray != null && curLineArray.length > TEMPLATE_COLUMN_INDEX) {
        return curLineArray[TEMPLATE_COLUMN_INDEX].trim().compareToIgnoreCase("Y") == 0;
      }
      return false;
    }

    /**
     * Increment the line, and null out nextLine
     */
    public void next() {
      if (hasNext()) {
        curLineArray = nextLine.split(",");
        nextLine = null;
      }
    }
  }

  /**
   * Creates a list of value/description pairs.  Will not contain any empty entries, but
   * may contain entries without a description.
   * @author sueh
   *
   */
  public static final class ChoiceList {
    private final List<String[]> choiceList;

    private DebugLevel debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();

    private ChoiceList(final String input, final DebugLevel debug) {
      if (debug != null) {
        this.debug = debug;
      }
      String[] choiceArray = input.split("\\s*;\\s*");
      if (choiceArray == null || choiceArray.length < 1) {
        choiceList = null;
      }
      else {
        choiceList = new ArrayList<String[]>();
        for (int i = 0; i < choiceArray.length; i++) {
          if (choiceArray[i] != null && choiceArray[i].length() > 0) {
            String[] array = choiceArray[i].split("\\s*\\:\\s*");
            if (array != null && array.length >= 0) {
              choiceList.add(array);
            }
          }
        }
      }
    }

    public void setDebug(final DebugLevel input) {
      debug = input;
    }

    boolean isEmpty() {
      return choiceList == null || choiceList.isEmpty();
    }

    public int size() {
      if (choiceList != null) {
        return choiceList.size();
      }
      return 0;
    }

    /**
     * If the index is valid, returns the choice description.  If the decription is
     * missing, returns the choice number.  If the index is not valid, returns null.
     * @param index
     * @return
     */
    public String getDescr(final int index) {
      if (!isEmpty() && index >= 0 && index < choiceList.size()) {
        String[] choice = choiceList.get(index);
        if (choice.length > 1) {
          return choice[1];
        }
        return choice[0];
      }
      else {
        return null;
      }
    }

    /**
     * If the index is valid, returns the value.  If the index is not valid, returns null.
     * @param index
     * @return
     */
    public String getValue(final int index) {
      if (!isEmpty() && index >= 0 && index < choiceList.size()) {
        String[] choice = choiceList.get(index);
        return choice[0];
      }
      else {
        return null;
      }
    }
  }
}
