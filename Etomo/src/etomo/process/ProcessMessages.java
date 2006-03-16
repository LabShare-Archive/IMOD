package etomo.process;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Vector;

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
  private int index = -1;
  private String line = null;
  private Vector infoList = null;
  private Vector warningList = null;
  private Vector errorList = null;
  private Vector chunkErrorList = null;

  final static ProcessMessages getInstance() {
    return new ProcessMessages(false, false);
  }

  final static ProcessMessages getMultiLineInstance() {
    return new ProcessMessages(true, false);
  }

  final static ProcessMessages getInstanceForParallelProcessing() {
    return new ProcessMessages(false, true);
  }

  private ProcessMessages(boolean multiLineMessages, boolean chunks) {
    this.multiLineMessages = multiLineMessages;
    this.chunks = chunks;
  }

  /**
   * Set processOutput to outputBufferManager and parse it.
   * Function must be synchronized because it relies on member variables to
   * parse processOutput.
   * @param processOutput: OutputBufferManager
   */
  synchronized final void addProcessOutput(OutputBufferManager processOutput) {
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
  synchronized final void addProcessOutput(String[] processOutput) {
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
  synchronized final void addProcessOutput(File processOutput)
      throws FileNotFoundException {
    //  Open the file as a stream
    InputStream fileStream = new FileInputStream(processOutput);
    bufferedReader = new BufferedReader(new InputStreamReader(fileStream));
    nextLine();
    while (bufferedReader != null) {
      parse();
    }
  }

  /**
   * Set processOutput to processOutputString and parse it.
   * Function must be synchronized because it relies on member variables to
   * parse processOutput.  Does not work with multi line messages.
   * @param processOutput: String
   */
  synchronized final void addProcessOutput(String processOutput) {
    if (multiLineMessages) {
      throw new IllegalStateException(
          "multiLineMessages is true but function can only parse one line at a time");
    }
    //  Open the file as a stream
    processOutputString = processOutput;
    nextLine();
    if (line != null) {
      parse();
    }
  }

  synchronized final void add(ProcessMessages processMessages) {
    addError(processMessages);
    if (processMessages.warningList != null
        && processMessages.warningList.size() > 0) {
      getWarningList().addAll(processMessages.warningList);
    }
    if (processMessages.infoList != null
        && processMessages.infoList.size() > 0) {
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

  synchronized final void addError(String[] errors) {
    if (errors == null || errors.length == 0) {
      return;
    }
    for (int i = 0; i < errors.length; i++) {
      getErrorList().add(errors[i]);
    }
  }

  synchronized final void addError(ProcessMessages processMessages) {
    if (processMessages.errorList != null
        && processMessages.errorList.size() > 0) {
      getErrorList().addAll(processMessages.getErrorList());
    }
  }

  synchronized final void addWarning(String warning) {
    getWarningList().add(warning);
  }

  public final int errorListSize() {
    if (errorList == null) {
      return 0;
    }
    return errorList.size();
  }

  public final int warningListSize() {
    if (warningList == null) {
      return 0;
    }
    return warningList.size();
  }

  final int infoListSize() {
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

  public final String getLastChunkError() {
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

  final boolean isError() {
    return errorList != null && errorList.size() > 0;
  }

  public final void printError() {
    if (errorList == null) {
      return;
    }
    for (int i = 0; i < errorList.size(); i++) {
      System.err.println((String) errorList.get(i));
    }
  }

  public final void printWarning() {
    if (warningList == null) {
      return;
    }
    for (int i = 0; i < warningList.size(); i++) {
      System.err.println((String) warningList.get(i));
    }
  }

  private final void printInfo() {
    if (infoList == null) {
      return;
    }
    for (int i = 0; i < infoList.size(); i++) {
      System.err.println((String) infoList.get(i));
    }
  }

  private final void parse() {
    parsePipWarning();
    if (chunks) {
      if (multiLineMessages) {
        parseMultiLineChunkError();
      }
      else {
        parseSingleLineChunkError();
      }
    }
    if (multiLineMessages) {
      parseMultiLineMessage();
    }
    else {
      parseSingleLineMessage();
    }
  }

  /**
   * Looks for pip warnings and adds them to infoList.
   * Pip warnings are multi-line and have a start and end tag.  They may start
   * and end in the middle of a line.
   * Should be run before parseMultiLineMessage or parseSingleListMessage.
   * When it is done, line will be pointing to an unparsed string.
   */
  private final void parsePipWarning() {
    if (line == null) {
      return;
    }
    //look for a pip warning
    int pipWarningIndex = -1;
    if ((pipWarningIndex = line.indexOf(PIP_WARNING_TAG)) == -1) {
      return;
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
      return;
    }
    boolean moreLines = nextLine();
    while (moreLines) {
      //end tag not found - add line to the pip warning message
      if ((pipWarningEndTagIndex = line.indexOf(PIP_WARNING_END_TAG)) == -1) {
        pipWarning.append(" " + line);
        moreLines = nextLine();
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
          nextLine();
        }
        getInfoList().add(pipWarning.toString());
        return;
      }
    }
    //no more lines - add pip warning to infoList
    getInfoList().add(pipWarning.toString());
  }

  /**
   * Looks for single line errors, warnings, and info messages.
   * Messages have start tags and may start in the middle of the line.
   * When it is done, line will be pointing to an unparsed string.
   */
  private final void parseSingleLineMessage() {
    if (line == null) {
      return;
    }
    //look for a message
    int errorIndex = line.indexOf(ERROR_TAG);
    int warningIndex = line.indexOf(WARNING_TAG);
    int infoIndex = line.indexOf(INFO_TAG);
    boolean error = errorIndex != -1;
    boolean warning = warningIndex != -1;
    boolean info = infoIndex != -1;
    if (!error && !warning && !info) {
      //no message - go to next line
      nextLine();
      return;
    }
    //message found - add to list
    if (error) {
      addElement(getErrorList(), line, errorIndex);
    }
    else if (warning) {
      addElement(getWarningList(), line, warningIndex);
    }
    else if (info) {
      addElement(getInfoList(), line, infoIndex);
    }
    nextLine();
    return;
  }
  
  /**
   * Looks for single line chunk errors.
   * Messages have start tags and may start in the middle of the line.
   * When it is done, line will be pointing to an unparsed string.
   */
  private final void parseSingleLineChunkError() {
    if (line == null) {
      return;
    }
    //look for a message
    int chunkErrorIndex = line.indexOf(CHUNK_ERROR_TAG);
    boolean chunkError = chunkErrorIndex != -1;
    if (!chunkError) {
      //no message - go to next line
      nextLine();
      return;
    }
    //message found - add to list
    if (chunkError) {
      addElement(getChunkErrorList(), line, chunkErrorIndex);
    }
    nextLine();
    return;
  }

  /**
   * Looks for multi-line errors, warnings, and info messages.
   * Messages have start tags and may start in the middle of the line.
   * Messages end with an empty line.
   * When it is done, line will be pointing to an unparsed string.
   */
  private final void parseMultiLineMessage() {
    if (line == null) {
      return;
    }
    //look for a message
    int errorIndex = line.indexOf(ERROR_TAG);
    int warningIndex = line.indexOf(WARNING_TAG);
    int infoIndex = line.indexOf(INFO_TAG);
    boolean error = errorIndex != -1;
    boolean warning = warningIndex != -1;
    boolean info = infoIndex != -1;
    if (!error && !warning && !info) {
      //no message - go to next line
      nextLine();
      return;
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
    boolean moreLines = nextLine();
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
        nextLine();
        return;
      }
      else {//add current line to the message
        messageBuffer.append(" " + line);
        moreLines = nextLine();
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
  }
  
  /**
   * Looks for multi-line errors.
   * Messages have start tags and may start in the middle of the line.
   * Messages end with an empty line.
   * When it is done, line will be pointing to an unparsed string.
   */
  private final void parseMultiLineChunkError() {
    if (line == null) {
      return;
    }
    //look for a message
    int chunkErrorIndex = line.indexOf(CHUNK_ERROR_TAG);
    boolean chunkError = chunkErrorIndex != -1;
    if (!chunkError) {
      //no message - go to next line
      nextLine();
      return;
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
    boolean moreLines = nextLine();
    while (moreLines) {
      if (line.length() == 0) {
        //end of message - add message in a list
        String message = messageBuffer.toString();
        if (chunkError) {
          getChunkErrorList().add(message);
        }
        nextLine();
        return;
      }
      else {//add current line to the message
        messageBuffer.append(" " + line);
        moreLines = nextLine();
      }
    }
    //no more lines - add message to list
    String message = messageBuffer.toString();
    if (chunkError) {
      getChunkErrorList().add(message);
    }
  }


  /**
   * Figure out which type of process output is being read and call the
   * corresponding nextLine function.
   * @return
   */
  private final boolean nextLine() {
    if (outputBufferManager != null) {
      return nextOutputBufferManagerLine();
    }
    if (bufferedReader != null) {
      return nextBufferedReaderLine();
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

  /**
   * Add a substring of line, from startIndex to end of line, to list.
   * @param list
   * @param line
   * @param startIndex
   */
  private final void addElement(Vector list, String line, int startIndex) {
    if (startIndex > 0) {
      list.add(line.substring(startIndex));
    }
    else {
      list.add(line);
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
  private final Vector getInfoList() {
    if (infoList == null) {
      infoList = new Vector();
    }
    return infoList;
  }

  /**
   * Returns chunkErrorList.  Never returns null.  
   * @return
   */
  private final Vector getChunkErrorList() {
    if (chunkErrorList == null) {
      chunkErrorList = new Vector();
    }
    return chunkErrorList;
  }
}
/**
 * <p> $Log$
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