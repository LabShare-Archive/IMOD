package etomo.process;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
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

  private static final String ERROR_TAG = "ERROR:";
  private static final String[] ERROR_TAGS = { ERROR_TAG, "Errno", "Traceback" };
  private static final boolean[] ALWAYS_MULTI_LINE = { false, false, true };
  public static final String WARNING_TAG = "WARNING:";
  private static final String CHUNK_ERROR_TAG = "CHUNK ERROR:";
  private static final String PIP_WARNING_TAG = "PIP WARNING:";
  private static final String INFO_TAG = "INFO:";
  private static final String PIP_WARNING_END_TAG = "Using fallback options in main program";
  private static final int MAX_MESSAGE_SIZE = 10;
  private static final String[] IGNORE_TAG = { "prnstr('ERROR:", "log.write('ERROR:" };
  private static final String LOG_FILE_TAG = "LOGFILE:";

  private final boolean chunks;
  private final BaseManager manager;

  private OutputBufferManager outputBufferManager = null;
  private BufferedReader bufferedReader = null;
  private String processOutputString = null;
  private String[] processOutputStringArray = null;
  private LogFile logFile = null;
  private LogFile.ReaderId logFileReaderId = null;
  private int index = -1;
  private String line = null;
  private List<String> infoList = null;
  private List<String> warningList = null;
  private List<String> errorList = null;
  private List<String> chunkErrorList = null;
  private String successTag1 = null;
  private String successTag2 = null;
  private boolean success = false;
  // multi line error, warning, and info strings; terminated by an empty line
  // may be turned off temporarily
  private boolean multiLineAllMessages = false;
  private boolean multiLineWarning = false;
  private boolean multiLineInfo = false;
  private String messagePrependTag = null;
  private String messagePrepend = null;

  public void dumpState() {
    System.err.print("[chunks:" + chunks + ",processOutputString:" + processOutputString
        + ",processOutputStringArray:");
    if (processOutputStringArray != null) {
      System.err.print("{");
      for (int i = 0; i < processOutputStringArray.length; i++) {
        System.err.print(processOutputStringArray[i]);
        if (i < processOutputStringArray.length - 1) {
          System.err.print(",");
        }
      }
      System.err.print("}");
    }
    System.err.print(",index:" + index + ",line:" + line + ",infoList:");
    if (infoList != null) {
      System.err.println(infoList.toString());
    }
    System.err.print(",warningList:");
    if (warningList != null) {
      System.err.println(warningList.toString());
    }
    System.err.print(",errorList:");
    if (errorList != null) {
      System.err.println(errorList.toString());
    }
    System.err.print(",chunkErrorList:");
    if (chunkErrorList != null) {
      System.err.println(chunkErrorList.toString());
    }
    System.err.print(",successTag1:" + successTag1 + ",successTag2:" + successTag2
        + ",\nsuccess:" + success + ",multiLineMessages:" + multiLineAllMessages + "]");
  }

  static ProcessMessages getInstance(final BaseManager manager) {
    return new ProcessMessages(manager, false, false, null, null, false, false);
  }

  static ProcessMessages getInstance(final BaseManager manager, final String successTag) {
    return new ProcessMessages(manager, false, false, successTag, null, false, false);
  }

  static ProcessMessages getInstance(final BaseManager manager, final String successTag1,
      String successTag2) {
    return new ProcessMessages(manager, false, false, successTag1, successTag2, false,
        false);
  }

  static ProcessMessages getMultiLineInstance(final BaseManager manager) {
    return new ProcessMessages(manager, true, false, null, null, false, false);
  }

  static ProcessMessages getInstanceForParallelProcessing(final BaseManager manager,
      final boolean multiLineMessages) {
    return new ProcessMessages(manager, multiLineMessages, true, null, null, false, false);
  }

  static ProcessMessages getMultiLineInstance(final BaseManager manager,
      final boolean multiLineWarning, final boolean multiLineInfo) {
    return new ProcessMessages(manager, false, false, null, null, multiLineWarning,
        multiLineInfo);
  }

  private ProcessMessages(final BaseManager manager, final boolean multiLineMessages,
      final boolean chunks, final String successTag1, final String successTag2,
      final boolean multiLineWarning, final boolean multiLineInfo) {
    this.multiLineAllMessages = multiLineMessages;
    this.multiLineWarning = multiLineWarning;
    this.multiLineInfo = multiLineInfo;
    this.chunks = chunks;
    this.successTag1 = successTag1;
    this.successTag2 = successTag2;
    this.manager = manager;
  }

  /**
   * While messagePrependTag is set, the most recent line containing the tag will be
   * saved to messagePrepend and added to the beginning of the next error/warning message.
   * MessagePrepend will be deleted after it is used.  If no error or warning message
   * appears, messagePrepend will not be used and the tag will have no effect.  All other
   * types of message tags take precedence over this tag.  Passing null to this function
   * will cause both messagePrependTag and messagePrepend to be set to null.
   * @param tag
   */
  void setMessagePrependTag(final String tag) {
    messagePrependTag = tag;
    if (messagePrependTag == null) {
      messagePrepend = null;
    }
  }

  /**
   * Set processOutput to outputBufferManager and parse it.
   * Function must be synchronized because it relies on member variables to
   * parse processOutput.
   * @param processOutput: OutputBufferManager
   */
  synchronized void addProcessOutput(final OutputBufferManager processOutput) {
    outputBufferManager = processOutput;
    nextLine();
    while (outputBufferManager != null) {
      parse();
    }
  }

  /**
   * Set processOutput to processOutputStringArray and parse it.
   * Function must be synchronized because it relies on member variables to
   * parse processOutput.
   * @param processOutput: String[]
   */
  synchronized void addProcessOutput(final String[] processOutput) {
    processOutputStringArray = processOutput;
    nextLine();
    while (processOutputStringArray != null) {
      parse();
    }
  }

  /**
   * Set processOutput to bufferedReader and parse it.
   * Function must be synchronized because it relies on member variables to
   * parse processOutput.
   * @param processOutput: File
   * @throws FileNotFoundException
   */
  synchronized void addProcessOutput(final File processOutput)
      throws FileNotFoundException {
    // Open the file as a stream
    InputStream fileStream = new FileInputStream(processOutput);
    bufferedReader = new BufferedReader(new InputStreamReader(fileStream));
    nextLine();
    while (bufferedReader != null) {
      parse();
    }
  }

  synchronized void addProcessOutput(final LogFile processOutput)
      throws LogFile.LockException, FileNotFoundException {
    // Open the log file
    logFile = processOutput;
    logFileReaderId = logFile.openReader();
    nextLine();
    while (logFile != null) {
      parse();
    }
  }

  /**
   * Set processOutput to processOutputString and parse it.
   * Function must be synchronized because it relies on member variables to
   * parse processOutput.  Temporarily turns off multi-line messages.
   * @param processOutput: String
   */
  synchronized void addProcessOutput(final String processOutput) {
    // Open the file as a stream
    processOutputString = processOutput;
    boolean oldMultiLineMessages = multiLineAllMessages;
    multiLineAllMessages = false;
    boolean oldMultiLineWarning = multiLineWarning;
    multiLineWarning = false;
    boolean oldMultiLineInfo = multiLineInfo;
    multiLineInfo = false;
    nextLine();
    // processOutput may be broken up into multiple lines
    if (line != null) {
      parse();
    }
    multiLineAllMessages = oldMultiLineMessages;
    multiLineWarning = oldMultiLineWarning;
    multiLineInfo = oldMultiLineInfo;
  }

  synchronized void add(final ProcessMessages processMessages) {
    addError(processMessages);
    if (processMessages.warningList != null && processMessages.warningList.size() > 0) {
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

  synchronized void addError(final String error) {
    getErrorList().add(error);
  }

  synchronized void addError() {
    getErrorList().add("");
  }

  synchronized void addWarning() {
    getWarningList().add("");
  }

  synchronized void addError(final String[] errors) {
    if (errors == null || errors.length == 0) {
      return;
    }
    for (int i = 0; i < errors.length; i++) {
      getErrorList().add(errors[i]);
    }
  }

  synchronized void addError(final ProcessMessages processMessages) {
    if (processMessages.errorList != null && processMessages.errorList.size() > 0) {
      getErrorList().addAll(processMessages.getErrorList());
    }
  }

  synchronized void addWarning(final String warning) {
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

  public String getError(final int errorIndex) {
    if (errorList == null || errorIndex < 0 || errorIndex >= errorList.size()) {
      return null;
    }
    return errorList.get(errorIndex);
  }

  public String getWarning(final int warningIndex) {
    if (warningList == null || warningIndex < 0 || warningIndex >= warningList.size()) {
      return null;
    }
    return warningList.get(warningIndex);
  }

  public String getInfo(final int infoIndex) {
    if (infoList == null || infoIndex < 0 || infoIndex >= infoList.size()) {
      return null;
    }
    return infoList.get(infoIndex);
  }

  /**
   * Returns messages which contain matchString.
   * @param matchString
   * @return
   */
  public List<String> getInfoList(final String matchString) {
    if (!isInfo()) {
      return null;
    }
    List<String> matches = new ArrayList<String>();
    Iterator<String> i = infoList.iterator();
    while (i.hasNext()) {
      String message = i.next();
      if (message.indexOf(matchString) != -1) {
        matches.add(message);
      }
    }
    if (matches.size() != 0) {
      return matches;
    }
    return null;
  }

  public String getLastChunkError() {
    if (chunkErrorList == null || chunkErrorList.size() == 0) {
      return null;
    }
    return chunkErrorList.get(chunkErrorList.size() - 1);
  }

  public String getLastWarning() {
    if (warningList == null || warningList.size() == 0) {
      return null;
    }
    return warningList.get(warningList.size() - 1);
  }

  final void print() {
    printError();
    printWarning();
    printInfo();
  }

  boolean isError() {
    return errorList != null && errorList.size() > 0;
  }

  public boolean isInfo() {
    return infoList != null && infoList.size() > 0;
  }

  boolean isSuccess() {
    return success;
  }

  public void printError() {
    if (errorList == null) {
      return;
    }
    for (int i = 0; i < errorList.size(); i++) {
      System.err.println(errorList.get(i));
    }
  }

  public void printWarning() {
    if (warningList == null) {
      return;
    }
    for (int i = 0; i < warningList.size(); i++) {
      System.err.println(warningList.get(i));
    }
  }

  private void printInfo() {
    if (infoList == null) {
      return;
    }
    for (int i = 0; i < infoList.size(); i++) {
      System.err.println(infoList.get(i));
    }
  }

  /**
   * Look for a message or a success tag.  Return when something is found or
   * when there is nothing left to look for.
   */
  private void parse() {
    if (parsePipWarning()) {
      return;
    }
    if (chunks) {
      if (multiLineAllMessages) {
        if (parseMultiLineChunkError()) {
          return;
        }
      }
      else {
        if (parseSingleLineChunkError()) {
          return;
        }
      }
    }
    if (multiLineAllMessages) {
      if (parseMultiLineMessage()) {
        return;
      }
    }
    else {
      if (parseSingleLineMessage()) {
        return;
      }
    }
    // No message has been found on this line, so check it for success tags
    if (parseSuccessLine()) {
      return;
    }
    parseMessagePrepend();
    // parse functions go to the next line only when they find something
    // if all parse functions failed, call nextLine()
    nextLine();
  }

  /**
   * Sets messagePrepend.  Never called nextLine.
   */
  private void parseMessagePrepend() {
    if (line != null && messagePrependTag != null
        && line.indexOf(messagePrependTag) != -1) {
      messagePrepend = line;
    }
  }

  /**
   * This function should be used to check every line that doesn't contain a
   * message.
   * Looks for success tags in the line.  Sets success = true if all set tags
   * are found.  Tags are checked in order.  Tag 1 must come first and the tags
   * must not overlap.
   * @return true if success found
   */
  private boolean parseSuccessLine() {
    if (line == null || (successTag1 == null && successTag2 == null)) {
      return false;
    }
    int tag1Index = 0;
    int tag1Size = 0;
    boolean tag1 = false;
    // try to match tag 1
    if (successTag1 != null) {
      tag1Index = line.indexOf(successTag1);
      if (tag1Index == -1) {
        return false;
      }
      // tag 1 found, set variables
      tag1Size = successTag1.length();
      tag1 = true;
    }
    // check for tag 2
    if (successTag2 == null) {
      // tag 2 wasn't set
      if (tag1) {
        // tag 1 was found - success
        nextLine();
        success = true;
        return true;
      }
      return false;
    }
    // match tag 2
    int tag2Index = line.substring(tag1Index + tag1Size).indexOf(successTag2);
    if (tag2Index == -1) {
      return false;
    }
    nextLine();
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
  private boolean parsePipWarning() {
    if (line == null) {
      return false;
    }
    // look for a pip warning
    int pipWarningIndex = -1;
    if ((pipWarningIndex = line.indexOf(PIP_WARNING_TAG)) == -1) {
      return false;
    }
    // create message starting at pip warning tag.
    StringBuffer pipWarning = new StringBuffer();
    if (messagePrepend != null) {
      pipWarning.append(messagePrepend + "\n");
      messagePrepend = null;
    }
    if (pipWarningIndex > 0) {
      pipWarning.append(line.substring(pipWarningIndex));
    }
    else {
      pipWarning.append(line);
    }
    // check for a one line pip warning.
    int pipWarningEndTagIndex = line.indexOf(PIP_WARNING_END_TAG);
    if (pipWarningEndTagIndex != -1) {
      // check for pip warning ending in the middle of the line
      int pipWarningEndIndex = pipWarningEndTagIndex + PIP_WARNING_END_TAG.length();
      if (line.length() > pipWarningEndIndex) {
        // remove the message from the line so the line can continue to be parsed
        line = line.substring(pipWarningEndIndex);
      }
      return true;
    }
    boolean moreLines = nextLine();
    while (moreLines) {
      // end tag not found - add line to the pip warning message
      if ((pipWarningEndTagIndex = line.indexOf(PIP_WARNING_END_TAG)) == -1) {
        pipWarning.append(" " + line);
        moreLines = nextLine();
      }
      else {
        // found the end tag - save the pip warning to infoList
        // check for pip warning ending in the middle of the line
        int pipWarningEndIndex = pipWarningEndTagIndex + PIP_WARNING_END_TAG.length();
        if (line.length() > pipWarningEndIndex) {
          pipWarning.append(" " + line.substring(0, pipWarningEndIndex));
          // remove the message from the line so the line can continue to be parsed
          line = line.substring(pipWarningEndIndex);
        }
        else {
          // pip warning takes up the whole line
          pipWarning.append(" " + line);
          nextLine();
        }
        getInfoList().add(pipWarning.toString());
        return true;
      }
    }
    // no more lines - add pip warning to infoList
    getInfoList().add(pipWarning.toString());
    return true;
  }

  /**
   * Looks for single line errors, warnings, and info messages.
   * Messages have start tags and may start in the middle of the line.
   * If a message is found, line will be set to a new line.
   * Looks for a log file entry.  A log file entry is a single line.  The tag may have
   * text preceeding it.  The text following the tag is the path of a file, the contents
   * of which should be added to _project.log.
   * @return true if a message is found
   */
  private boolean parseSingleLineMessage() {
    if (line == null) {
      return false;
    }
    int logFileIndex = line.indexOf(LOG_FILE_TAG);
    if (logFileIndex != -1) {
      File file = new File(manager.getPropertyUserDir(), line.substring(
          logFileIndex + LOG_FILE_TAG.length()).trim());
      if (file.exists() && file.isFile() && file.canRead()) {
        if (manager != null) {
          manager.logMessage(file);
        }
        else {
          System.err.println(LOG_FILE_TAG + " " + file.getAbsolutePath());
          try {
            LogFile logFile = LogFile.getInstance(file);
            LogFile.ReaderId id = logFile.openReader();
            String logFileLine = null;
            while ((logFileLine = logFile.readLine(id)) != null) {
              System.err.println(logFileLine);
            }
          }
          catch (LogFile.LockException e) {
            e.printStackTrace();
            System.err.println("Unable to process " + line + ".  " + e.getMessage());
          }
          catch (FileNotFoundException e) {
            e.printStackTrace();
            System.err.println("Unable to process " + line + ".  " + e.getMessage());
          }
          catch (IOException e) {
            e.printStackTrace();
            System.err.println("Unable to process " + line + ".  " + e.getMessage());
          }
        }
      }
      else {
        System.err.println("Warning: unable to log from file:" + file.getAbsolutePath());
      }
    }
    // look for a message
    int errorIndex = -1;
    int errorTagIndex = -1;
    for (int i = 0; i < ERROR_TAGS.length; i++) {
      errorIndex = line.indexOf(ERROR_TAGS[i]);
      errorTagIndex = i;
      if (errorIndex != -1) {
        break;
      }
    }
    // Turn off the error if an ignore-tag is found.
    if (errorIndex != -1) {
      for (int i = 0; i < IGNORE_TAG.length; i++) {
        if (line.indexOf(IGNORE_TAG[i]) != -1) {
          errorIndex = -1;
          errorTagIndex = -1;
          break;
        }
      }
    }
    // Switch to multi-line error parsing if this error is always a multi-line error.
    if (errorIndex != -1 && errorTagIndex != -1 && ALWAYS_MULTI_LINE[errorTagIndex]) {
      return parseMultiLineMessage();
    }
    int chunkErrorIndex = line.indexOf(CHUNK_ERROR_TAG);
    int warningIndex = line.indexOf(WARNING_TAG);
    if (warningIndex != -1 && multiLineWarning) {
      return parseMultiLineMessage();
    }
    int infoIndex = line.indexOf(INFO_TAG);
    if (infoIndex != -1 && multiLineInfo) {
      return parseMultiLineMessage();
    }
    // error is true if ERROR: found, but not CHUNK ERROR:
    boolean error = errorIndex != -1 && errorTagIndex != -1
        && !(chunks && chunkErrorIndex != -1);
    boolean warning = warningIndex != -1;
    boolean info = infoIndex != -1;
    if (!error && !warning && !info) {
      return false;
    }
    // message found - add to list
    StringBuffer buffer = new StringBuffer();
    if (messagePrepend != null) {
      buffer.append(messagePrepend + "\n");
      messagePrepend = null;
    }
    buffer.append(line);
    if (error) {
      addElement(getErrorList(), buffer.toString(), errorIndex,
          ERROR_TAGS[errorTagIndex].length());
    }
    else if (warning) {
      addElement(getWarningList(), buffer.toString(), warningIndex, WARNING_TAG.length());
    }
    else if (info) {
      addElement(getInfoList(), buffer.toString(), infoIndex, INFO_TAG.length());
    }
    nextLine();
    return true;
  }

  /**
   * Looks for single line chunk errors.
   * Messages have start tags and may start in the middle of the line.
   * If a chunk error is found, line will be set to a new line.
   * @return true if a chunk error is found
   */
  private boolean parseSingleLineChunkError() {
    if (line == null) {
      return false;
    }
    // look for a message
    int chunkErrorIndex = line.indexOf(CHUNK_ERROR_TAG);
    boolean chunkError = chunkErrorIndex != -1;
    if (!chunkError) {
      return false;
    }
    // message found - add to list
    if (chunkError) {
      // Errors may be added to the chunk error line. They are errors associated with the
      // chunk and should not be treated as process errors, so put them on separate lines
      // but keep them with the chunk error.
      int errorIndex = line
          .indexOf(ERROR_TAG, chunkErrorIndex + CHUNK_ERROR_TAG.length());
      if (errorIndex == -1) {
        addElement(getChunkErrorList(), line, chunkErrorIndex, CHUNK_ERROR_TAG.length());
      }
      else {
        StringBuffer buffer = new StringBuffer();
        if (messagePrepend != null) {
          buffer.append(messagePrepend + "\n");
          messagePrepend = null;
        }
        buffer.append(line.substring(0, errorIndex) + "\n");
        while (errorIndex != -1) {
          int nextErrorIndex = line.indexOf(ERROR_TAG, errorIndex + ERROR_TAG.length());
          if (nextErrorIndex != -1) {
            buffer.append(line.substring(errorIndex, nextErrorIndex) + "\n");
          }
          else {
            buffer.append(line.substring(errorIndex));
          }
          errorIndex = nextErrorIndex;
        }
        addElement(getChunkErrorList(), buffer.toString(), chunkErrorIndex,
            CHUNK_ERROR_TAG.length());
      }
    }
    nextLine();
    return true;
  }

  public static int getErrorIndex(String line) {
    int index = -1;
    for (int j = 0; j < ProcessMessages.ERROR_TAGS.length; j++) {
      index = line.indexOf(ProcessMessages.ERROR_TAGS[j]);
      if (index != -1) {
        return index;
      }
    }
    return -1;
  }

  /**
   * Looks for multi-line errors, warnings, and info messages.
   * Messages have start tags and may start in the middle of the line.
   * Messages end with an empty line.
   * If a message is found, line will be set to a new line.
   * @return true if a message is found
   */
  private boolean parseMultiLineMessage() {
    if (line == null) {
      return false;
    }
    // look for a message
    int errorIndex = getErrorIndex(line);
    if (errorIndex != -1) {
      for (int i = 0; i < IGNORE_TAG.length; i++) {
        if (line.indexOf(IGNORE_TAG[i]) != -1) {
          errorIndex = -1;
        }
      }
    }
    int chunkErrorIndex = line.indexOf(CHUNK_ERROR_TAG);
    int warningIndex = line.indexOf(WARNING_TAG);
    int infoIndex = line.indexOf(INFO_TAG);
    // error is true if ERROR: found, but not CHUNK ERROR:
    boolean error = errorIndex != -1 && !(chunks && chunkErrorIndex != -1);
    boolean warning = warningIndex != -1;
    boolean info = infoIndex != -1;
    if (!error && !warning && !info) {
      return false;
    }
    // set the index of the message tag
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
    // create the message starting from the message tag
    StringBuffer messageBuffer = new StringBuffer();
    if (messagePrepend != null) {
      messageBuffer.append(messagePrepend + "\n");
      messagePrepend = null;
    }
    if (messageIndex > 0) {
      messageBuffer.append(line.substring(messageIndex));
    }
    else {
      messageBuffer.append(line);
    }
    boolean moreLines = nextLine();
    int count = 0;
    while (moreLines) {
      if (line.length() == 0 || count > MAX_MESSAGE_SIZE) {
        // end of message or message is too big - add message in a list
        messageBuffer.append("\n");
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
        nextLine();
        return true;
      }
      else {// add current line to the message
        messageBuffer.append("\n" + line);
        moreLines = nextLine();
        count++;
      }
    }
    // no more lines - add message to list
    messageBuffer.append("\n");
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
  private boolean parseMultiLineChunkError() {
    if (line == null) {
      return false;
    }
    // look for a message
    int chunkErrorIndex = line.indexOf(CHUNK_ERROR_TAG);
    boolean chunkError = chunkErrorIndex != -1;
    if (!chunkError) {
      return false;
    }
    // set the index of the message tag
    int messageIndex = -1;
    if (chunkError) {
      messageIndex = chunkErrorIndex;
    }
    // create the message starting from the message tag
    StringBuffer messageBuffer = new StringBuffer();
    if (messagePrepend != null) {
      messageBuffer.append(messagePrepend + "\n");
      messagePrepend = null;
    }
    if (messageIndex > 0) {
      messageBuffer.append(line.substring(messageIndex));
    }
    else {
      messageBuffer.append(line);
    }
    boolean moreLines = nextLine();
    int count = 0;
    while (moreLines) {
      if (line.length() == 0 || count > MAX_MESSAGE_SIZE) {
        // end of message or message is too long - add message in a list
        messageBuffer.append("\n");
        String message = messageBuffer.toString();
        if (chunkError) {
          getChunkErrorList().add(message);
        }
        nextLine();
        return true;
      }
      else {// add current line to the message
        messageBuffer.append("\n" + line);
        moreLines = nextLine();
        count++;
      }
    }
    // no more lines - add message to list
    messageBuffer.append("\n");
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
  private boolean nextLine() {
    boolean retval = false;
    if (outputBufferManager != null) {
      retval = nextOutputBufferManagerLine();
    }
    else if (bufferedReader != null) {
      retval = nextBufferedReaderLine();
    }
    else if (logFile != null) {
      retval = nextLogFileLine();
    }
    else if (processOutputString != null) {
      line = processOutputString;
      processOutputString = null;
      retval = true;
    }
    else if (processOutputStringArray != null) {
      retval = nextStringArrayLine();
    }
    return retval;
  }

  /**
   * Increment index and place the entry at index in outputBufferManager into line.
   * Trim line.
   * Return true if line can be set to a new line
   * Return false, sets outputBufferManager and line to null, and sets index to
   * -1 when there is nothing left in outputBufferManager.
   */
  private boolean nextOutputBufferManagerLine() {
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
  private boolean nextStringArrayLine() {
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
  private boolean nextBufferedReaderLine() {
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

  private boolean nextLogFileLine() {
    try {
      if ((line = logFile.readLine(logFileReaderId)) == null) {
        logFile.closeRead(logFileReaderId);
        logFile = null;
        logFileReaderId = null;
        return false;
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      logFile.closeRead(logFileReaderId);
      logFile = null;
      logFileReaderId = null;
      line = null;
      return false;
    }
    catch (IOException e) {
      e.printStackTrace();
      logFile.closeRead(logFileReaderId);
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
  private void addElement(final List<String> list, final String line,
      final int startIndex, final int tagSize) {
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
  private List<String> getErrorList() {
    if (errorList == null) {
      errorList = new Vector<String>();
    }
    return errorList;
  }

  /**
   * Returns warningList.  Never returns null.  
   * @return
   */
  private List<String> getWarningList() {
    if (warningList == null) {
      warningList = new Vector<String>();
    }
    return warningList;
  }

  /**
   * Returns infoList.  Never returns null.  
   * @return
   */
  private List<String> getInfoList() {
    if (infoList == null) {
      infoList = new Vector<String>();
    }
    return infoList;
  }

  /**
   * Returns chunkErrorList.  Never returns null.  
   * @return
   */
  private List<String> getChunkErrorList() {
    if (chunkErrorList == null) {
      chunkErrorList = new Vector<String>();
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
 * <p> Revision 1.15  2011/02/22 04:09:08  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.14  2010/11/13 16:03:45  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.13  2010/06/18 16:23:31  sueh
 * <p> bug# 1385 Added addWarning and getLastWarning.
 * <p>
 * <p> Revision 1.12  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
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
