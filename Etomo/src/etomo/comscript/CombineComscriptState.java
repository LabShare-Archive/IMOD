package etomo.comscript;

import etomo.ApplicationManager;
import etomo.ui.TomogramCombinationDialog;

/**
* <p>Description: Represents the state of the combine.com.  Used to modify
* combine.com.  Contains information about which commands will be run.
* Also knows about watched files like patch.out.</p>
*
* <p>Copyright: Copyright 2004 </p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* Univeristy of Colorado</p>
*
* @author $$Author$$
*
* @version $$Revision$$
*
* <p> $Log$
* <p> Revision 1.3  2004/08/20 21:29:06  sueh
* <p> bug# 508 Made the match string static.  Added equals() functions for
* <p> testing.
* <p> Added:
* <p> String COMSCRIPT_MATCH_STRING
* <p> String notEqualsReason
* <p> equals(CombineComscriptState that)
* <p> getNotEqualsReason()
* <p> Deleted:
* <p> String comscriptMatchString
* <p> Changed:
* <p> String getComscriptMatchString()
* <p> initializeComscriptMatchString()
* <p> selfTest(int state)
* <p>
* <p> $Revision 1.2  2004/08/19 20:01:55  sueh
* <p> $bug# 508 Generated a regular expression that will match all 
* <p> $the comscript names handled by this object.
* <p> $Added:
* <p> $String comscriptMatchString
* <p> $getComscriptMatchString()
* <p> $initializeComscriptMatchString()
* <p> $Changed:
* <p> $CombineComscriptState()
* <p> $selfTest(int state)
* <p> $
* <p> $Revision 1.1  2004/08/19 00:36:18  sueh
* <p> $bug# 508 adding state object for combine.com to update
* <p> $combine.com, keep track of which commands will run,
* <p> $and keep track of information required to run command.com
* <p> </p>
*/
public class CombineComscriptState implements ComscriptState {
  public static final String rcsid = "$$Id$$";

  public static final String COMSCRIPT_NAME = "combine";
  public static final String COMSCRIPT_WATCHED_FILE = "combine.out";
  
  private static final String COMMANDS[] =
    {
      "solvematch",
      "matchvol1",
      "patchcorr",
      "matchorwarp",
      "volcombine" };
  
  private static final String WATCHED_FILES[] =
    {
      null,
      null,
      "patch.out",
      null,
      null };
      
  private static final String DIALOG_PANES[] =
    {
      TomogramCombinationDialog.lblInitial,
      TomogramCombinationDialog.lblInitial,
      TomogramCombinationDialog.lblFinal,
      TomogramCombinationDialog.lblFinal,
      TomogramCombinationDialog.lblFinal };
   
  public static final int NULL_INDEX = -1;
  public static final int SOLVEMATCH_INDEX = 0;
  public static final int MATCHVOL1_INDEX = 1;
  public static final int PATCHCORR_INDEX = 2; 
  public static final int MATCHORWARP_INDEX = 3; 
  public static final int VOLCOMBINE_INDEX = 4;  
  public static final int NUM_COMMANDS = COMMANDS.length;
  private static final char LABEL_DELIMITER = ':';
  
  private static final String SUCCESS_TEXT =
    "COMBINE SUCCESSFULLY COMPLETED";
  private static final String THROUGH_TEXT = " THROUGH ";
    
  private static final int CONSTRUCTED_STATE = 1;
  private static final int INITIALIZED_STATE = 2;
  private static final int START_COMMAND_SET_STATE = 3;
  private static final int END_COMMAND_SET_STATE = 4;
  private static String COMSCRIPT_MATCH_STRING = null;
  
  private int startCommand = NULL_INDEX;
  private int endCommand = NULL_INDEX;
  
  //testing variables
  private String notEqualsReason = null;
  private boolean selfTest = false;
    
  public CombineComscriptState() {
    initializeComscriptMatchString();
    selfTest = ApplicationManager.isSelfTest();
    runSelfTest(CONSTRUCTED_STATE);
  }
  
  /**
   * initialize instance from combine.com
   * @param comScriptManager
   * @return
   */
  public boolean initialize(ComScriptManager comScriptManager) {
    if (!loadStartCommand(comScriptManager)) {
      runSelfTest(INITIALIZED_STATE);
      return false;
    } 
    loadEndCommand(comScriptManager);
    runSelfTest(INITIALIZED_STATE);
    return true;
  }

  /**
   * sets startCommand in combine.com
   * @param startCommand
   * @param comScriptManager
   */
  public void setStartCommand(int startCommand, 
    ComScriptManager comScriptManager) {
    if (startCommand < 0 || startCommand >= NUM_COMMANDS) {
      throw new IndexOutOfBoundsException();
    }
    this.startCommand = startCommand;
    GotoParam gotoParam = new GotoParam();
    gotoParam.setLabel(COMMANDS[startCommand]);
    comScriptManager.saveCombine(gotoParam);
    runSelfTest(START_COMMAND_SET_STATE);
  }
  
  /**
   * sets endCommand in combine.com
   * @param endCommand
   * @param comScriptManager
   */
  public void setEndCommand(int endCommand, ComScriptManager comScriptManager) {
    if (endCommand < 0 || endCommand >= NUM_COMMANDS) {
      throw new IndexOutOfBoundsException();
    }
    this.endCommand = endCommand;
    String commandLabel = toLabel(VOLCOMBINE_INDEX);
    //if the endCommand is the last command (volcombine), remove the exit 
    //success commands from after the volcombine label, if they are there
    if (endCommand == NUM_COMMANDS - 1) {
      comScriptManager.deleteFromCombine(EchoParam.COMMAND_NAME, commandLabel);
      comScriptManager.deleteFromCombine(ExitParam.COMMAND_NAME, commandLabel);
    }
    else {
      //if the endCommand is not the last command (must be matchorwarp), add
      //or update exit success commands after the volcombine label
      //insert echo param if it is not there, otherwise update it
      EchoParam echoParam = new EchoParam();
      echoParam.setString(SUCCESS_TEXT + THROUGH_TEXT + COMMANDS[endCommand].toUpperCase());
      int echoIndex = comScriptManager.saveCombine(echoParam, commandLabel);
      //insert exit param if it is not there, otherwise update it
      ExitParam exitParam = new ExitParam();
      exitParam.setResultValue(0);
      comScriptManager.saveCombine(exitParam, echoIndex);
    }
    runSelfTest(END_COMMAND_SET_STATE);
  }
  
  /**
   * 
   * @return true if volcombine will run
   */ 
  public boolean isRunVolcombine() {
    return endCommand >= VOLCOMBINE_INDEX;
  }

  /**
   * 
   */
  public int getStartCommand() {
    return startCommand;
  }
  
  /**
   * 
   */
  public int getEndCommand() {
    return endCommand;
  }
  
  /**
   * 
   */
  public String getCommand(int commandIndex) {
    if (commandIndex == NULL_INDEX) {
      return null;
    }
    return COMMANDS[commandIndex];
  }
  
  /**
   * 
   */
  public String getWatchedFile(int commandIndex) {
    if (commandIndex == NULL_INDEX) {
      return null;
    }
    return WATCHED_FILES[commandIndex];
  }
  
  public String getComscriptName() {
    return COMSCRIPT_NAME;
  }
  
  public String getComscriptWatchedFile() {
    return COMSCRIPT_WATCHED_FILE;
  }
  
  /**
   * @return
   */
  public static String getComscriptMatchString() {
    if (COMSCRIPT_MATCH_STRING == null) {
      initializeComscriptMatchString();
    }
    return COMSCRIPT_MATCH_STRING;
  }


  public static String getSuccessText() {
    return SUCCESS_TEXT;
  }
  
  public static String getDialogPane(int commandIndex) {
    if (commandIndex == NULL_INDEX) {
      return null;
    }
    return DIALOG_PANES[commandIndex];
  }
  
  /**
   * convert a command name to a command index
   * @param commandName
   * @return
   */
  public static int getCommandIndex(String commandName) {
    for (int i = 0; i < NUM_COMMANDS; i++) {
      if (commandName.equals(COMMANDS[i])) {
        return i;
      }
    }
    return NULL_INDEX;
  }
  
  /**
   * convert a command index to a label string
   * @param commandIndex
   * @return
   */
  private static String toLabel(int commandIndex) {
    return COMMANDS[commandIndex] + LABEL_DELIMITER;
  }
  
  /**
   * load startCommand from combine.com
   * @param comScriptManager
   * @return
   */
  private boolean loadStartCommand(ComScriptManager comScriptManager) {
    //check the first goto to see which command will be run first
    GotoParam gotoParam = comScriptManager.getGotoParamFromCombine();
    //backward compatibility - old combine.com did not have this goto
    if (gotoParam == null) {
      return false;
    }
    startCommand = getCommandIndex(gotoParam.getLabel());
    return true;
  }
  
  /**
   * load end command from combine.com.  Check to see if combine.com is exiting
   * before running volcombine.
   * @param comScriptManager
   */
  private void loadEndCommand(ComScriptManager comScriptManager) {
    EchoParam echoParam = 
      comScriptManager.getEchoParamFromCombine(toLabel(VOLCOMBINE_INDEX));
    if (echoParam != null && echoParam.getString().startsWith(SUCCESS_TEXT)) {
      endCommand = VOLCOMBINE_INDEX - 1;
    }
    else {
      endCommand = VOLCOMBINE_INDEX;
    }
  }
  
  /**
   * create a regular expression which matches the comscripts in managed by
   * this object
   *
   */
  private static void initializeComscriptMatchString() {
    //match 0 or more characters and a word boundary
    StringBuffer stringBuffer = new StringBuffer(".*\\b[");
    //match 1 or the strings in COMMANDS
    for (int i = 0; i < NUM_COMMANDS; i++) {
      stringBuffer.append("\\Q" + COMMANDS[i] + "\\E");
    }
    //match ".com", a word boundary, and 0 or more characters
    stringBuffer.append("]\\.com\\b.*");
    COMSCRIPT_MATCH_STRING = stringBuffer.toString();
  }
  
  //Testing code
  
  /**
   * test for incorrect member variable settings in CombineComscriptState.
   * @param state
   */
  public void selfTest(int state) {
    String stateString = null;
    switch (state) {
      case CONSTRUCTED_STATE :
      stateString = "After construction:  ";
        if (NULL_INDEX >= 0 && NULL_INDEX < NUM_COMMANDS) {
          throw new IllegalStateException(stateString 
              + "NULL_INDEX can't be a valid index number.  "              + "NULL_INDEX=" + NULL_INDEX);
        }
        if (startCommand != NULL_INDEX || endCommand != NULL_INDEX) {
          throw new IllegalStateException(stateString 
              + "startCommand and endCommand should start as NULL_INDEX.  "
              + "startCommand=" + startCommand + ",endCommand=" + endCommand);
        }
        if (COMMANDS.length != NUM_COMMANDS) {
          throw new IllegalStateException(stateString
              + "NUM_COMMANDS should be equal to the size of COMMMANDS.  "
              + "NUM_COMMANDS=" + NUM_COMMANDS + ",COMMANDS.length="
              + COMMANDS.length);
        }
        if (WATCHED_FILES.length != NUM_COMMANDS) {
          throw new IllegalStateException(stateString
              + "NUM_COMMANDS should be equal to the size of WATCHED_FILES.  "              + "NUM_COMMANDS=" + NUM_COMMANDS + ",WATCHED_FILES.length="
              + WATCHED_FILES.length);
        }  
        if (DIALOG_PANES.length != NUM_COMMANDS) {
          throw new IllegalStateException(stateString
              + "NUM_COMMANDS should be equal to the size of DIALOG_PANES.  "              + "NUM_COMMANDS=" + NUM_COMMANDS + ",DIALOG_PANES.length="
              + DIALOG_PANES.length);
        }
        if (COMSCRIPT_MATCH_STRING == null) {
          throw new NullPointerException(stateString
              + "ComscriptMatchString cannot be null.");
        }
        String comscriptName = null;
        for (int i = 0; i < NUM_COMMANDS; i++) {
          comscriptName = COMMANDS[i] + ".com";
          if (!comscriptName.matches(COMSCRIPT_MATCH_STRING)) {
            throw new IllegalStateException(stateString
                + "The regular expression comscriptMatchString must match each "
                + "comscript name.  "
                + "comscriptMatchString'" + COMSCRIPT_MATCH_STRING 
                + ",comscriptName=" + comscriptName);
          }
        }
        break;
        
      case INITIALIZED_STATE :
        stateString = "After initialize:  ";
        if (startCommand != NULL_INDEX || endCommand != NULL_INDEX) {
          if (startCommand > endCommand) {
            throw new IllegalStateException(stateString
                + "StartCommand and endCommand must not be NULL_INDEX.  "
                + "startCommand=" + startCommand + ",endCommand=" + endCommand
                + "NULL_INDEX=" + NULL_INDEX);
          }
          if (startCommand < 0 || startCommand >= NUM_COMMANDS 
              || endCommand < 0 || endCommand >= NUM_COMMANDS) {
            throw new IndexOutOfBoundsException(stateString
                + "StartCommand and endCommand must be valid index values.  "
                + "startCommand=" + startCommand + ",endCommand=" + endCommand
                + ",NUM_COMMANDS=" + NUM_COMMANDS);
          }
        }
        break;
        
      case START_COMMAND_SET_STATE :
      stateString = "After set start command:  ";
        if (startCommand == NULL_INDEX) {
          throw new IllegalStateException(stateString
              + "StartCommand must not be NULL_INDEX.  "
              + "startCommand=" + startCommand + "NULL_INDEX=" + NULL_INDEX);
        }
        if (startCommand < 0 || startCommand >= NUM_COMMANDS) {
          throw new IndexOutOfBoundsException(stateString
              + "StartCommand must be a valid index value.  "
              + "startCommand=" + startCommand + ",NUM_COMMANDS=" 
              + NUM_COMMANDS);
        }
        break;
        
      case END_COMMAND_SET_STATE :
        stateString = "After set end command:  ";
        if (endCommand == NULL_INDEX) {
          throw new IllegalStateException(stateString
          + "EndCommand must not be NULL_INDEX.  "
          + "endCommand=" + endCommand + "NULL_INDEX=" + NULL_INDEX);
        }
        if (endCommand < 0 || endCommand >= NUM_COMMANDS) {
          throw new IndexOutOfBoundsException(stateString
          + "EndCommand must be a valid index value.  "
          + "endCommand=" + endCommand + ",NUM_COMMANDS=" + NUM_COMMANDS);
        }
        if (endCommand != VOLCOMBINE_INDEX && endCommand != MATCHORWARP_INDEX) {
          throw new IllegalStateException(stateString 
              + "EndCommand can only refer to volcombine or matchorwarp.  "
              + "endCommand=" + endCommand + ",VOLCOMBINE_INDEX="
              + VOLCOMBINE_INDEX+ ",MATCHORWARP_INDEX=" + MATCHORWARP_INDEX);
        }
        if (endCommand < VOLCOMBINE_INDEX && isRunVolcombine()) {
          throw new IllegalStateException(stateString
              + "IsRunVolcombine() should not be true when endCommand is less "              + "then" + "VOLCOMBINE_INDEX.  "
              + "endCommand=" + endCommand + ",VOLCOMBINE_INDEX=" 
              + VOLCOMBINE_INDEX);
        }
        if (endCommand >= VOLCOMBINE_INDEX && !isRunVolcombine()) {
          throw new IllegalStateException(stateString
              + "IsRunVolcombine() should not be false when endCommand is "
              + "greater or equal to" + "VOLCOMBINE_INDEX.  "
              + "endCommand=" + endCommand + ",VOLCOMBINE_INDEX=" 
              + VOLCOMBINE_INDEX);
        }
        break;
        
      default :
        throw new IllegalStateException("Unknown state.  state=" + state);
    }
  }

  /**
   * Runs selfTest(int) when selfTest is set
   * @param selfTest
   * @param state
   */
  private void runSelfTest(int state) {
    if (!selfTest) {
      return;
    }
    selfTest(state);
  }
  
  public boolean equals(CombineComscriptState that) {
    if (startCommand != that.startCommand) {
      notEqualsReason = "StartCommand is not equal.  this.startCommand="
          + startCommand + ",that.startCommand=" + that.startCommand;
      return false;
    }
    if (endCommand != that.endCommand) {
      notEqualsReason = "EndCommand is not equal.  this.endCommand="
          + endCommand + ",that.endCommand=" + that.endCommand;
      return false;
    }
    notEqualsReason = null;
    return true;
  }
  
  public String getNotEqualsReason() {
    return notEqualsReason;
  }

}
