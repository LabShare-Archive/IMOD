package etomo.comscript;

import java.util.LinkedList;

/**
 * <p>Description: This class models a single command within an IMOD com script
 * file.  It handles header comments, the command line, command line arguments
 * and standard input for the command.</p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 2.3  2003/07/11 23:18:20  rickg
 * <p> Adde key/value processing methods
 * <p>
 * <p> Revision 2.2  2003/07/09 16:00:58  rickg
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 2.1  2003/03/06 01:19:17  rickg
 * <p> Combine changes in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ComScriptCommand {
  public static final String rcsid =
    "$Id$";

  private boolean keywordValuePairs = false;

  private String[] headerComments = new String[0];
  private String command = null;
  private String[] commandLineArgs = new String[0];
  private LinkedList stdinArgs = new LinkedList();

  /**
   * Default constructor.  A zero length String array is created to represent
   * the header comments and command line arguments, the command is null.  A
   * zero length String array will be returned for the input argument array.
   */
  public ComScriptCommand() {
  }

  /**
   * Copy constructor.
   */
  public ComScriptCommand(ComScriptCommand src) {
    keywordValuePairs = src.isKeywordValuePairs();
    headerComments = src.getHeaderComments();
    command = src.getCommand();
    commandLineArgs = src.getCommandLineArgs();

    ComScriptInputArg[] inputArgs = src.getInputArguments();
    for (int i = 0; i < inputArgs.length; i++) {
      stdinArgs.add(inputArgs[i]);
    }
  }

  /**
   * Set the header comments, the comment block which preceeds the command line.
   * @param a String array containing the header comments for the command.
   */
  public void setHeaderComments(String[] headerComments) {
    //  make a defensive copy of the array
    this.headerComments = new String[headerComments.length];
    for (int i = 0; i < headerComments.length; i++) {
      this.headerComments[i] = headerComments[i];
    }
  }

  public void appendHeaderComments(String[] moreComments) {
    String[] oldComments = headerComments;
    headerComments = new String[oldComments.length + moreComments.length];
    for (int i = 0; i < oldComments.length; i++) {
      headerComments[i] = oldComments[i];
    }
    for (int i = 0; i < moreComments.length; i++) {
      headerComments[i + oldComments.length] = moreComments[i];
    }
  }

  /**
   * Return the header comments.
   * @return a copy of the header comments as a String array.
   */
  public String[] getHeaderComments() {
    if (headerComments == null) {
      return null;
    }
    //  make a defensive copy of the array
    String[] safeArray = new String[headerComments.length];
    for (int i = 0; i < headerComments.length; i++) {
      safeArray[i] = headerComments[i];
    }
    return safeArray;
  }

  /**
   * Set the command string
   * @param a String containing the command.
   */
  public void setCommand(String command) {
    this.command = command;
  }

  /**
   * Get the command string.
   * @return the command as a string.
   */
  public String getCommand() {
    return command;
  }

  /**
   * Set the command line arguments for the script command.
   * @param a String array containing the command line arguments for the command
   * .
   */
  public void setCommandLineArgs(String[] args) {

    if (args.length == 1 && args[0].equals("-StandardInput")) {
      commandLineArgs = new String[args.length];
      commandLineArgs[0] = args[0];
      keywordValuePairs = true;
    }
    else {
      //  make a defensive copy of the array
      commandLineArgs = new String[args.length];
      for (int i = 0; i < args.length; i++) {
        commandLineArgs[i] = args[i];
      }
    }
  }

  /**
   * Append addition arguments onto the command line argument list.
   * @param a String array containing the additional command line arguments to
   * be added.
   */
  public void appendCommandLineArgs(String[] append) {
    String[] existingArgs = commandLineArgs;
    commandLineArgs = new String[existingArgs.length + append.length];

    for (int i = 0; i < existingArgs.length; i++) {
      commandLineArgs[i] = existingArgs[i];
    }

    for (int i = 0; i < append.length; i++) {
      commandLineArgs[i + existingArgs.length] = append[i];
    }
  }

  /**
   * Get the command line arguments.  Each argument (white space separated
   * command line entry) is store in a separate element of the return String[].
   * @return a String array containing the comman line arguments.
   */
  public String[] getCommandLineArgs() {
    if (commandLineArgs == null) {
      return null;
    }
    //  make a defensive copy of the array
    String[] safeArray = new String[commandLineArgs.length];
    for (int i = 0; i < commandLineArgs.length; i++) {
      safeArray[i] = commandLineArgs[i];
    }
    return safeArray;
  }

  /**
   * Append to the input argument to the input argument list
   * @param a ComScriptInputArg which is copied into the internal input
   * argument list.
   */
  public void appendInputArgument(ComScriptInputArg inputArg) {
    stdinArgs.add(new ComScriptInputArg(inputArg));
  }

  /**
   * Get the command input arguments.  These are the values (and associated
   * comments) that are meant for the standard input to the command.
   * @return a array of stdinArgs, this is a copy of the internal
   * representation.
   */
  public ComScriptInputArg[] getInputArguments() {
    ComScriptInputArg[] inputArgs = new ComScriptInputArg[stdinArgs.size()];
    return (ComScriptInputArg[]) stdinArgs.toArray(inputArgs);
  }

  /**
   * Set the ith input argument to the supplied parameter
   * @param index an integer specifying which argument to replace.
   * @param inputArg a ComScriptInputArg containing the input line and comments
   */
  public void setInputArgument(int index, ComScriptInputArg inputArg) {
    //  create a defensive copy of the input argument object
    stdinArgs.set(index, new ComScriptInputArg(inputArg));
  }

  /**
   * Set all of the input arguments, erasing any existing input arguments.
   * @param inputArgs a ComScriptInputArg array containing all of the input
   * arguments
   */
  public void setInputArguments(ComScriptInputArg[] inputArgs) {
    //  Clear the current list
    stdinArgs.clear();

    //  create a defensive copy of the input argument object
    for (int i = 0; i < inputArgs.length; i++) {
      stdinArgs.add(new ComScriptInputArg(inputArgs[i]));
    }
  }

  /**
   * Switch to using keyword/value pairs.  All of the current input arguments
   * are deleted, and -StandardInput is added to the command line.  This
   * should be done right before updating all of the values for converting a
   * old style script to new.  The function has no effect if the script is
   * already using keyword/value pairs.
   */
  public void useKeywordValue() {
    if (!keywordValuePairs) {
      stdinArgs.clear();
      keywordValuePairs = true;
      commandLineArgs = new String[1];
      commandLineArgs[0] = "-StandardInput";
    }
  }

  /**
   * Returns true if the command uses keyword/value pairs and the specified
   * keyword is being used  
   * @param keyword
   * @return
   */
  public boolean hasKeyword(String keyword) throws InvalidParameterException {
    if (!keywordValuePairs) {
      throw new InvalidParameterException(
        "Command " + command + " does not use keyword/value pairs");
    }
    if (findKey(keyword) >= 0) {
      return true;
    }
    return false;
  }

  // TODO these need to throw appropriate exceptions

  public String getValue(String keyword) throws InvalidParameterException {
    int idx = findKey(keyword);
    if (idx >= 0) {
      ComScriptInputArg inputArg = (ComScriptInputArg) stdinArgs.get(idx);
      String[] tokens = inputArg.getArgument().trim().split("\\s+", 2);
      if (tokens.length < 2) {
        throw new InvalidParameterException(tokens[0] + " has no parameters");
      }
      return tokens[1];
    }
    return "";
  }

  /**
   * Sets the specified key to the specified value.  The key will be created if
   * it does not exist.
   * @param keyword
   * @param value
   */
  public void setValue(String keyword, String value) {
    ComScriptInputArg inputArg;
    int idx = findKey(keyword);
    if (idx == -1) {
      inputArg = new ComScriptInputArg();
      stdinArgs.add(inputArg);
    }
    else {
      inputArg = (ComScriptInputArg) stdinArgs.get(idx);
    }
    inputArg.setArgument(keyword + "\t" + value);
  }

  /**
   * Add a new key and value to the standard input argument list
   * @param keyword
   * @param value
   */
  public void addKey(String keyword, String value) {
    ComScriptInputArg newArg = new ComScriptInputArg();
    newArg.setArgument(keyword + "\t" + value);
    stdinArgs.add(newArg);
  }

  /**
   * Delete the comScriptInputArg elements it contains the specified key 
   * @param keyword
   */
  public void deleteKey(String keyword) {
    int idx = findKey(keyword);
    if (idx >= 0) {
      stdinArgs.remove(idx);
    }
  }

  /**
   * Returns the index within the stdinArgs list of the first
   * ComScriptInputArg containing the specified key or -1 if the keyword is 
   * not found.
   * @param keyword
   * @return Index into the linked list
   */
  private int findKey(String keyword) {
    for (int i = 0; i < stdinArgs.size(); i++) {
      ComScriptInputArg inputArg = (ComScriptInputArg) stdinArgs.get(i);
      String[] tokens = inputArg.getArgument().trim().split("\\s+", 2);
      if (tokens[0].equals(keyword)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * @return
   */
  public boolean isKeywordValuePairs() {
    return keywordValuePairs;
  }

}
