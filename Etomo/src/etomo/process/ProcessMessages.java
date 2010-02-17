package etomo.process;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Vector;

import etomo.BaseManager;
import etomo.storage.LogFile;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class ProcessMessages {
  public static final String rcsid = "$Id$";

  public static final String ERROR_TAG = "ERROR:";
  public static final String WARNING_TAG = "WARNING:";
  private static final String CHUNK_ERROR_TAG = "CHUNK ERROR:";
  private static final String PIP_WARNING_TAG = "PIP WARNING:";
  private static final String INFO_TAG = "INFO:";
  private static final String PIP_WARNING_END_TAG = "Using fallback options in Fortran code";

  //multi line error, warning, and info strings; terminated by an empty line
  private final boolean multiLineMessages;
  private final boolean chunks;

  private OutputBufferManager outputBufferManager = null;
  private BufferedReader bufferedReader = null;
  private String processOutputString = null;
  private String[] processOutputStringArray = null;
  private LogFile logFile = null;
  private LogFile.ReaderId logFileReaderId = null;
  private int index = -1;
  private String line = null;
  private Vector infoList = null;
  private Vector warningList = null;
  private Vector errorList = null;
  private Vector chunkErrorList = null;
  private String successTag1 = null;
  private String successTag2 = null;
  private boolean success = false;

  static ProcessMessages getInstance() {
    return new ProcessMessages(false, false, null, null);
  }

  static ProcessMessages getInstance(String successTag) {
    return new ProcessMessages(false, false, successTag, null);
  }

  static ProcessMessages getInstance(String successTag1, String successTag2) {
    return new ProcessMessages(false, false, successTag1, successTag2);
  }

  final static ProcessMessages getMultiLineInstance() {
    return new ProcessMessages(true, false, null, null);
  }

  final static ProcessMessages getInstanceForParallelProcessing() {
    return new ProcessMessages(false, true, null, null);
  }

  private ProcessMessages(boolean multiLineMessages, boolean chunks,
      String successTag1, String successTag2) {
    this.multiLineMessages = multiLineMessages;
    this.chunks = chunks;
    this.successTag1 = successTag1;
    this.successTag2 = successTag2;
  }

  /**
   * Set processOutput to outputBufferManager and parse it.
   * Function must be synchronized because it relies on member variables to
   * parse processOutput.
   * @param processOutput: OutputBufferManager
   */
  synchronized final void addProcessOutput(BaseManager manager,
      OutputBufferManager processOutput) {
    outputBufferManager = processOutput;
    nextLine(manager);
    while (outputBufferManager != null) {
      parse(manager);
    }
  }

  /**
   * Set processOutput to processOutputStringArray and parse it.
   * Function must be synchronized because it relies on member variables to
   * parse processOutput.
   * @param processOutput: String[]
   */
  synchronized final void addProcessOutput(BaseManager manager,
      String[] processOutput) {
    processOutputStringArray = processOutput;
    nextLine(manager);
    while (processOutputStringArray != null) {
      parse(manager);
    }
  }

  /**
   * Set processOutput to bufferedReader and parse it.
   * Function must be synchronized because it relies on member variables to
   * parse processOutput.
   * @param processOutput: File
   * @throws FileNotFoundException
   */
  synchronized final void addProcessOutput(BaseManager manager,
      File processOutput) throws FileNotFoundException {
    //  Open the file as a stream
    InputStream fileStream = new FileInputStream(processOutput);
    bufferedReader = new BufferedReader(new InputStreamReader(fileStream));
    nextLine(manager);
    while (bufferedReader != null) {
      parse(manager);
    }
  }

  synchronized final void addProcessOutput(BaseManager manager,
      LogFile processOutput) throws LogFile.LockException,
      FileNotFoundException {
    //Open the log file
    logFile = processOutput;
    logFileReaderId = logFile.openReader();
    nextLine(manager);
    while (logFile != null) {
      parse(manager);
    }
  }

  /**
   * Set processOutput to processOutputString and parse it.
   * Function must be synchronized because it relies on member variables to
   * parse processOutput.  Does not work with multi line messages.
   * @param processOutput: String
   */
  synchronized final void addProcessOutput(BaseManager manager,
      String processOutput) {
    if (multiLineMessages) {
      throw new IllegalStateException(
          "multiLineMessages is true but function can only parse one line at a time");
    }
    //  Open the file as a stream
    processOutputString = processOutput;
    nextLine(manager);
    if (line != null) {
      parse(manager);
    }
  }

  synchronized final void add(ProcessMessages processMessages) {
    addError(processMessages);
    if (processMessages.warningList != null
        && processMessages.warningList.size() > 0) {
      getWarningList().addAll(processMessages.warningList);
    }
    if (processMessages.infoList != null && processMessages.infoList.size() > 0) {
      getInfoList().addAll(processMessages.infoList);
    }
    if (processMessages.chunkErrorList != null
        && processMessages.chunkErrorList.size() > 0) {
      getChunkErrorList().addAll(processMessages.chunkErrorList);
    }
  }

  synchronized final void addError(String error) {
    getErrorList().add(error);
  }

  synchronized final void addError() {
    getErrorList().add("");
  }

  synchronized void addError(String[] errors) {
    if (errors == null || errors.length == 0) {
      return;
    }
    for (int i = 0; i < errors.length; i++) {
      getErrorList().add(errors[i]);
    }
  }

  synchronized void addError(ProcessMessages processMessages) {
    if (processMessages.errorList != null
        && processMessages.errorList.size() > 0) {
      getErrorList().addAll(processMessages.getErrorList());
    }
  }

  synchronized void addWarning(String warning) {
    getWarningList().add(warning);
  }

  public int errorListSize() {
    if (errorList == null) {
      return 0;
    }
    return errorList.size();
  }

  public int warningListSize() {
    if (warningList == null) {
      return 0;
    }
    return warningList.size();
  }

  int infoListSize() {
    if (infoList == null) {
      return 0;
    }
    return infoList.size();
  }

  public final String getError(int errorIndex) {
    if (errorList == null || errorIndex < 0 || errorIndex >= errorList.size()) {
      return null;
    }
    return (String) errorList.get(errorIndex);
  }

  public final String getWarning(int warningIndex) {
    if (warningList == null || warningIndex < 0
        || warningIndex >= warningList.size()) {
      return null;
    }
    return (String) warningList.get(warningIndex);
  }

  final String getInfo(int infoIndex) {
    if (infoList == null || infoIndex < 0 || infoIndex >= infoList.size()) {
      return null;
    }
    return (String) infoList.get(infoIndex);
  }

  public String getLastChunkError() {
    if (chunkErrorList == null || chunkErrorList.size() == 0) {
      return null;
    }
    return (String) chunkErrorList.get(chunkErrorList.size() - 1);
  }

  final void print() {
    printError();
    printWarning();
    printInfo();
  }

  boolean isError() {
    return errorList != null && errorList.size() > 0;
  }

  boolean isSuccess() {
    return success;
  }

  public void printError() {
    if (errorList == null) {
      return;
    }
    for (int i = 0; i < errorList.size(); i++) {
      System.err.println((String) errorList.get(i));
    }
  }

  public void printWarning() {
    if (warningList == null) {
      return;
    }
    for (int i = 0; i < warningList.size(); i++) {
      System.err.println((String) warningList.get(i));
    }
  }

  private void printInfo() {
    if (infoList == null) {
      return;
    }
    for (int i = 0; i < infoList.size(); i++) {
      System.err.println((String) infoList.get(i));
    }
  }

  /**
   * Look for a message or a success tag.  Return when something is found or
   * when there is nothing left to look for.
   */
  private void parse(BaseManager manager) {
    if (parsePipWarning(manager)) {
      return;
    }
    if (chunks) {
      if (multiLineMessages) {
        if (parseMultiLineChunkError(manager)) {
          return;
        }
      }
      else {
        if (parseSingleLineChunkError(manager)) {
          return;
        }
      }
    }
    if (multiLineMessages) {
      if (parseMultiLineMessage(manager)) {
        return;
      }
    }
    else {
      if (parseSingleLineMessage(manager)) {
        return;
      }
    }
    //No message has been found on this line, so check it for success tags
    if (parseSuccessLine(manager)) {
      return;
    }
    //parse functions go to the next line only when they find something
    //if all parse functions failed, call nextLine()
    nextLine(manager);
  }

  /**
   * This function should be used to check every line that doesn't contain a
   * message.
   * Looks for success tags in the line.  Sets success = true if all set tags
   * are found.  Tags are checked in order.  Tag 1 must come first and the tags
   * must not overlap.
   * @return true if success found
   */
  private boolean parseSuccessLine(BaseManager manager) {
    if (line == null || (successTag1 == null && successTag2 == null)) {
      return false;
    }
    int tag1Index = 0;
    int tag1Size = 0;
    boolean tag1 = false;
    //try to match tag 1
    if (successTag1 != null) {
      tag1Index = line.indexOf(successTag1);
      if (tag1Index == -1) {
        return false;
      }
      //tag 1 found, set variables
      tag1Size = successTag1.length();
      tag1 = true;
    }
    //check for tag 2
    if (successTag2 == null) {
      //tag 2 wasn't set
      if (tag1) {
        //tag 1 was found - success
        nextLine(manager);
        success = true;
        return true;
      }
      return false;
    }
    //match tag 2
    int tag2Index = line.substring(tag1Index + tag1Size).indexOf(successTag2);
    if (tag2Index == -1) {
      return false;
    }
    nextLine(manager);
    success = true;
    return true;
  }

  /**
   * Looks for pip warnings and adds them to infoList.
   * Pip warnings are multi-line and have a start and end tag.  They may start
   * and end in the middle of a line.
   * Should be run before parseMultiLineMessage or parseSingleListMessage.
   * If a pip warning is found, line will be set to a new line.
   * @return true if pip warning is found
   */
  private boolean parsePipWarning(BaseManager manager) {
    if (line == null) {
      return false;
    }
    //look for a pip warning
    int pipWarningIndex = -1;
    if ((pipWarningIndex = line.indexOf(PIP_WARNING_TAG)) == -1) {
      return false;
    }
    //create message starting at pip warning tag.
    StringBuffer pipWarning = null;
    if (pipWarningIndex > 0) {
      pipWarning = new StringBuffer(line.substring(pipWarningIndex));
    }
    else {
      pipWarning = new StringBuffer(line);
    }
    //check for a one line pip warning.
    int pipWarningEndTagIndex = line.indexOf(PIP_WARNING_END_TAG);
    if (pipWarningEndTagIndex != -1) {
      //check for pip warning ending in the middle of the line
      int pipWarningEndIndex = pipWarningEndTagIndex
          + PIP_WARNING_END_TAG.length();
      if (line.length() > pipWarningEndIndex) {
        //remove the message from the line so the line can continue to be parsed
        line = line.substring(pipWarningEndIndex);
      }
      return true;
    }
    boolean moreLines = nextLine(manager);
    while (moreLines) {
      //end tag not found - add line to the pip warning message
      if ((pipWarningEndTagIndex = line.indexOf(PIP_WARNING_END_TAG)) == -1) {
        pipWarning.append(" " + line);
        moreLines = nextLine(manager);
      }
      else {
        //found the end tag - save the pip warning to infoList
        //check for pip warning ending in the middle of the line
        int pipWarningEndIndex = pipWarningEndTagIndex
            + PIP_WARNING_END_TAG.length();
        if (line.length() > pipWarningEndIndex) {
          pipWarning.append(" " + line.substring(0, pipWarningEndIndex));
          //remove the message from the line so the line can continue to be parsed
          line = line.substring(pipWarningEndIndex);
        }
        else {
          //pip warning takes up the whole line
          pipWarning.append(" " + line);
          nextLine(manager);
        }
        getInfoList().add(pipWarning.toString());
        return true;
      }
    }
    //no more lines - add pip warning to infoList
    getInfoList().add(pipWarning.toString());
    return true;
  }

  /**
   * Looks for single line errors, warnings, and info messages.
   * Messages have start tags and may start in the middle of the line.
   * If a message is found, line will be set to a new line.
   * @return true if a message is found
   */
  private boolean parseSingleLineMessage(BaseManager manager) {
    if (line == null) {
      return false;
    }
    //look for a message
    int errorIndex = line.indexOf(ERROR_TAG);
    int chunkErrorIndex = line.indexOf(CHUNK_ERROR_TAG);
    int warningIndex = line.indexOf(WARNING_TAG);
    int infoIndex = line.indexOf(INFO_TAG);
    //error is true if ERROR: found, but not CHUNK ERROR:
    boolean error = errorIndex != -1 && !(chunks && chunkErrorIndex != -1);
    boolean warning = warningIndex != -1;
    boolean info = infoIndex != -1;
    if (!error && !warning && !info) {
      return false;
    }
    //message found - add to list
    if (error) {
      addElement(getErrorList(), line, errorIndex, ERROR_TAG.length());
    }
    else if (warning) {
      addElement(getWarningList(), line, warningIndex, WARNING_TAG.length());
    }
    else if (info) {
      addElement(getInfoList(), line, infoIndex, INFO_TAG.length());
    }
    nextLine(manager);
    return true;
  }

  /**
   * Looks for single line chunk errors.
   * Messages have start tags and may start in the middle of the line.
   * If a chunk error is found, line will be set to a new line.
   * @return true if a chunk error is found
   */
  private boolean parseSingleLineChunkError(BaseManager manager) {
    if (line == null) {
      return false;
    }
    //look for a message
    int chunkErrorIndex = line.indexOf(CHUNK_ERROR_TAG);
    boolean chunkError = chunkErrorIndex != -1;
    if (!chunkError) {
      return false;
    }
    //message found - add to list
    if (chunkError) {
      addElement(getChunkErrorList(), line, chunkErrorIndex, CHUNK_ERROR_TAG
          .length());
    }
    nextLine(manager);
    return true;
  }

  /**
   * Looks for multi-line errors, warnings, and info messages.
   * Messages have start tags and may start in the middle of the line.
   * Messages end with an empty line.
   * If a message is found, line will be set to a new line.
   * @return true if a message is found
   */
  private boolean parseMultiLineMessage(BaseManager manager) {
    if (line == null) {
      return false;
    }
    //look for a message
    int errorIndex = line.indexOf(ERROR_TAG);
    int chunkErrorIndex = line.indexOf(CHUNK_ERROR_TAG);
    int warningIndex = line.indexOf(WARNING_TAG);
    int infoIndex = line.indexOf(INFO_TAG);
    //error is true if ERROR: found, but not CHUNK ERROR:
    boolean error = errorIndex != -1 && !(chunks && chunkErrorIndex != -1);
    boolean warning = warningIndex != -1;
    boolean info = infoIndex != -1;
    if (!error && !warning && !info) {
      return false;
    }
    //set the index of the message tag
    int messageIndex = -1;
    if (error) {
      messageIndex = errorIndex;
    }
    else if (warning) {
      messageIndex = warningIndex;
    }
    else if (info) {
      messageIndex = infoIndex;
    }
    //create the message starting from the message tag
    StringBuffer messageBuffer = null;
    if (messageIndex > 0) {
      messageBuffer = new StringBuffer(line.substring(messageIndex));
    }
    else {
      messageBuffer = new StringBuffer(line);
    }
    boolean moreLines = nextLine(manager);
    while (moreLines) {
      if (line.length() == 0) {
        //end of message - add message in a list
        String message = messageBuffer.toString();
        if (error) {
          getErrorList().add(message);
        }
        else if (warning) {
          getWarningList().add(message);
        }
        else if (info) {
          getInfoList().add(message);
        }
        nextLine(manager);
        return true;
      }
      else {//add current line to the message
        messageBuffer.append(" " + line);
        moreLines = nextLine(manager);
      }
    }
    //no more lines - add message to list
    String message = messageBuffer.toString();
    if (error) {
      getErrorList().add(message);
    }
    else if (warning) {
      getWarningList().add(message);
    }
    else if (info) {
      getInfoList().add(message);
    }
    return true;
  }

  /**
   * Looks for multi-line errors.
   * Messages have start tags and may start in the middle of the line.
   * Messages end with an empty line.
   * If a chunk error is found, line will be set to a new line.
   * @return true if a chunk error is found
   */
  private boolean parseMultiLineChunkError(BaseManager manager) {
    if (line == null) {
      return false;
    }
    //look for a message
    int chunkErrorIndex = line.indexOf(CHUNK_ERROR_TAG);
    boolean chunkError = chunkErrorIndex != -1;
    if (!chunkError) {
      return false;
    }
    //set the index of the message tag
    int messageIndex = -1;
    if (chunkError) {
      messageIndex = chunkErrorIndex;
    }
    //create the message starting from the message tag
    StringBuffer messageBuffer = null;
    if (messageIndex > 0) {
      messageBuffer = new StringBuffer(line.substring(messageIndex));
    }
    else {
      messageBuffer = new StringBuffer(line);
    }
    boolean moreLines = nextLine(manager);
    while (moreLines) {
      if (line.length() == 0) {
        //end of message - add message in a list
        String message = messageBuffer.toString();
        if (chunkError) {
          getChunkErrorList().add(message);
        }
        nextLine(manager);
        return true;
      }
      else {//add current line to the message
        messageBuffer.append(" " + line);
        moreLines = nextLine(manager);
      }
    }
    //no more lines - add message to list
    String message = messageBuffer.toString();
    if (chunkError) {
      getChunkErrorList().add(message);
    }
    return true;
  }

  /**
   * Figure out which type of process output is being read and call the
   * corresponding nextLine function.
   * @return
   */
  private final boolean nextLine(BaseManager manager) {
    if (outputBufferManager != null) {
      return nextOutputBufferManagerLine();
    }
    if (bufferedReader != null) {
      return nextBufferedReaderLine();
    }
    if (logFile != null) {
      return nextLogFileLine(manager);
    }
    if (processOutputString != null) {
      line = processOutputString;
      processOutputString = null;
      return true;
    }
    if (processOutputStringArray != null) {
      return nextStringArrayLine();
    }
    return false;
  }

  /**
   * Increment index and place the entry at index in outputBufferManager into line.
   * Trim line.
   * Return true if line can be set to a new line
   * Return false, sets outputBufferManager and line to null, and sets index to
   * -1 when there is nothing left in outputBufferManager.
   */
  private final boolean nextOutputBufferManagerLine() {
    index++;
    if (index >= outputBufferManager.size()) {
      index = -1;
      outputBufferManager = null;
      line = null;
      return false;
    }
    line = outputBufferManager.get(index).trim();
    return true;
  }

  /**
   * Increment index and place the entry at index in processOutputStringArray into line.
   * Trim line.
   * Return true if line can be set to a new line
   * Return false, sets processOutputStringArray and line to null, and sets index to
   * -1 when there is nothing left in processOutputStringArray.
   */
  private final boolean nextStringArrayLine() {
    index++;
    if (index >= processOutputStringArray.length) {
      index = -1;
      processOutputStringArray = null;
      line = null;
      return false;
    }
    line = processOutputStringArray[index].trim();
    return true;
  }

  /**
   * Increment index and place the entry at index in bufferedReader into line.
   * Trim line.
   * Return true if line can be set to a new line
   * Return false, and sets bufferedReader and line to null when there is
   * nothing left in bufferedReader.
   * Does not change index.
   */
  private final boolean nextBufferedReaderLine() {
    try {
      if ((line = bufferedReader.readLine()) == null) {
        bufferedReader = null;
        return false;
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      bufferedReader = null;
      line = null;
      return false;
    }
    return true;
  }

  private final boolean nextLogFileLine(BaseManager manager) {
    try {
      if ((line = logFile.readLine(logFileReaderId)) == null) {
        logFile.closeReader(logFileReaderId);
        logFile = null;
        logFileReaderId = null;
        return false;
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      logFile.closeReader(logFileReaderId);
      logFile = null;
      logFileReaderId = null;
      line = null;
      return false;
    }
    catch (IOException e) {
      e.printStackTrace();
      logFile.closeReader(logFileReaderId);
      logFile = null;
      logFileReaderId = null;
      line = null;
      return false;
    }
    return true;
  }

  /**
   * Add a substring of line, from startIndex to end of line, to list.
   * @param list
   * @param line
   * @param startIndex
   */
  private final void addElement(Vector list, String line, int startIndex,
      int tagSize) {
    if (startIndex + tagSize < line.length()) {
      if (startIndex > 0) {
        list.add(line.substring(startIndex));
      }
      else {
        list.add(line);
      }
    }
  }

  /**
   * Returns errorList.  Never returns null.  
   * @return
   */
  private final Vector getErrorList() {
    if (errorList == null) {
      errorList = new Vector();
    }
    return errorList;
  }

  /**
   * Returns warningList.  Never returns null.  
   * @return
   */
  private final Vector getWarningList() {
    if (warningList == null) {
      warningList = new Vector();
    }
    return warningList;
  }

  /**
   * Returns infoList.  Never returns null.  
   * @return
   */
  private Vector getInfoList() {
    if (infoList == null) {
      infoList = new Vector();
    }
    return infoList;
  }

  /**
   * Returns chunkErrorList.  Never returns null.  
   * @return
   */
  private Vector getChunkErrorList() {
    if (chunkErrorList == null) {
      chunkErrorList = new Vector();
    }
    return chunkErrorList;
  }

  public String toString() {
    StringBuffer buffer = new StringBuffer();
    if (infoList != null) {
      for (int i = 0; i < infoList.size(); i++) {
        buffer.append(infoList.get(i) + "\n");
      }
    }
    if (warningList != null) {
      for (int i = 0; i < warningList.size(); i++) {
        buffer.append(warningList.get(i) + "\n");
      }
    }
    if (errorList != null) {
      for (int i = 0; i < errorList.size(); i++) {
        buffer.append(errorList.get(i) + "\n");
      }
    }
    if (chunkErrorList != null) {
      for (int i = 0; i < chunkErrorList.size(); i++) {
        buffer.append(chunkErrorList.get(i) + "\n");
      }
    }
    return buffer.toString();
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.11  2009/05/02 01:08:46  sueh
 * <p> bug# 1216 In addElement fixed a problem where empty messages where
 * <p> saved.
 * <p>
 * <p> Revision 1.10  2009/02/04 23:26:53  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.9  2008/01/14 21:58:21  sueh
 * <p> bug# 1050 Added getInstance(String successTag) for finding message with one
 * <p> successTag.
 * <p>
 * <p> Revision 1.8  2006/10/10 05:13:05  sueh
 * <p> bug# 931 Added addProcessOutput(LogFile).
 * <p>
 * <p> Revision 1.7  2006/08/03 21:30:59  sueh
 * <p> bug# 769 Fixed parse():  once a message is found, return.  This means that all
 * <p> line will be checked for all messages.  Added parseSuccessLine().
 * <p>
 * <p> Revision 1.6  2006/06/14 00:09:03  sueh
 * <p> bug# 785 Not saving to error list when saving to chunk error list
 * <p>
 * <p> Revision 1.5  2006/05/22 22:50:34  sueh
 * <p> bug# 577 Added toString().
 * <p>
 * <p> Revision 1.4  2006/03/16 01:53:06  sueh
 * <p> Made constructor private
 * <p>
 * <p> Revision 1.3  2005/11/30 21:15:11  sueh
 * <p> bug# 744 Adding addProcessOutput(String[]) to get standard out error
 * <p> messages.
 * <p>
 * <p> Revision 1.2  2005/11/19 02:38:58  sueh
 * <p> bug# 744 Added parsing and separate storage for chunk errors.  Added
 * <p> addProcessOutput(String) for output that must be handled one line at a
 * <p> time.
 * <p>
 * <p> Revision 1.1  2005/11/02 21:59:50  sueh
 * <p> bug# 754 Class to parse and hold error, warning, and information
 * <p> messages.  Message can also be set directly in this class without
 * <p> parsing.  Can parse or set messages from multiple sources.
 * <p> </p>
 */
