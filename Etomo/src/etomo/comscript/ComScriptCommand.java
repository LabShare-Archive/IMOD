package etomo.comscript;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.Vector;

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
 * <p> Revision 3.7  2010/09/24 00:54:05  sueh
 * <p> bug# 1404 Returning a boolean from deleteKey.
 * <p>
 * <p> Revision 3.6  2009/09/22 20:51:11  sueh
 * <p> bug# 1259 In order to process nonstandard tilt.com, added
 * <p> caseInsensitive and separateWithASpace.
 * <p>
 * <p> Revision 3.5  2005/05/09 22:44:40  sueh
 * <p> bug# 658 Handling keyword equals null in hasKeyword() and findKey().
 * <p>
 * <p> Revision 3.4  2005/01/25 21:25:18  sueh
 * <p> bug 567 Return a null from getValue() instead of throwing an
 * <p> InvalidParameterException, when a parameter has no value.  Since a
 * <p> boolean may be represented by either 0 or 1, or just the presence or lack
 * <p> of presence of a keyword, a parameter with no value should be allowed.
 * <p>
 * <p> Revision 3.3  2004/04/26 23:39:26  rickg
 * <p> remove using the iterator
 * <p>
 * <p> Revision 3.2  2004/04/26 20:15:42  rickg
 * <p> Added interface to handle successive accumulation keywords
 * <p> untested
 * <p>
 * <p> Revision 3.1  2004/03/05 18:15:55  sueh
 * <p> bug# 250 add getCommandLineLength() - get the number of parameters
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.4  2003/07/25 22:56:59  rickg
 * <p> Added keywordValuePairs to copy constructor
 * <p>
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
  public static final String rcsid = "$Id$";

  //Case insensitive when finding keywords
  private final boolean caseInsensitive;
  //Use a space instead of \t - tab doesn't work when tilt.com to create samples
  //in tomo pos.
  private final String divider;

  private boolean keywordValuePairs = false;

  private String[] headerComments = new String[0];
  private String command = null;
  private String[] commandLineArgs = new String[0];
  private LinkedList stdinArgs = new LinkedList();

  private boolean debug = false;

  /**
   * Default constructor.  A zero length String array is created to represent
   * the header comments and command line arguments, the command is null.  A
   * zero length String array will be returned for the input argument array.
   */
  public ComScriptCommand(boolean caseInsensitive, boolean separateWithASpace) {
    this.caseInsensitive = caseInsensitive;
    if (separateWithASpace) {
      divider = " ";
    }
    else {
      divider = "\t";
    }
  }

  /**
   * Copy constructor.
   */
  public ComScriptCommand(ComScriptCommand src) {
    this.caseInsensitive = src.caseInsensitive;
    this.divider = src.divider;
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
    if (args.length == 1
        && (args[0].equals("-StandardInput") || (caseInsensitive && args[0]
            .equalsIgnoreCase("-StandardInput")))) {
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
   * Sets debug.
   * @param debug
   * @return the previous setting of debug
   */
  public boolean setDebug(boolean input) {
    boolean oldDebug = debug;
    debug = input;
    return oldDebug;
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

  public int getCommandLineLength() {
    if (commandLineArgs == null) {
      return 0;
    }
    return commandLineArgs.length;
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
      throw new InvalidParameterException("Command " + command
          + " does not use keyword/value pairs");
    }
    if (keyword == null) {
      return false;
    }
    if (findKey(keyword) >= 0) {
      return true;
    }
    return false;
  }

  /**
   * Returns the (first) value associated with the specified keyword or an empty
   * string if the keyowrd is not present.
   * @param keyword
   * @return
   * @throws InvalidParameterException
   */
  public String getValue(String keyword) throws InvalidParameterException {
    int idx = findKey(keyword);
    if (idx >= 0) {
      ComScriptInputArg inputArg = (ComScriptInputArg) stdinArgs.get(idx);
      String[] tokens = inputArg.getArgument().trim().split("\\s+", 2);
      if (tokens.length < 2) {
        return null;
      }
      return tokens[1];
    }
    return "";
  }

  /**
   * Returns all of the values associate with a keyword
   * @param keyword
   * @return
   */
  public String[] getValues(String keyword) {
    Vector values = new Vector();
    Iterator itStdinArgs = stdinArgs.iterator();
    while (itStdinArgs.hasNext()) {
      ComScriptInputArg inputArg = (ComScriptInputArg) itStdinArgs.next();

      String[] tokens = inputArg.getArgument().trim().split("\\s+", 2);
      if (tokens[0].equals(keyword)
          || (caseInsensitive && tokens[0].equalsIgnoreCase(keyword))) {
        if (tokens.length > 1) {
          values.add(tokens[1]);
        }
      }
    }
    if (values.size() > 0) {
      return (String[]) values.toArray(new String[values.size()]);
    }
    return new String[0];
  }

  /**
   * Sets the specified key to the specified value.  The key will be created if
   * it does not exist.
   * @param keyword
   * @param value
   */
  public void setValue(String keyword, String value) {
    if (debug) {
      System.out.println("setValue:keyword=" + keyword + ",value=" + value);
    }
    ComScriptInputArg inputArg;
    int idx = findKey(keyword);
    if (idx == -1) {
      inputArg = new ComScriptInputArg();
      stdinArgs.add(inputArg);
    }
    else {
      inputArg = (ComScriptInputArg) stdinArgs.get(idx);
    }
    boolean oldDebug = false;
    if (debug) {
      System.out.println("ComScriptCommand:setValue:keyword=" + keyword + ",divider="
          + divider + ",value=" + value);
      inputArg.setDebug(debug);
    }
    inputArg.setArgument(keyword + divider + value);
    if (debug) {
      inputArg.setDebug(oldDebug);
    }
  }

  /**
   * Sets the keyword the values specified replacing any existing values
   * @param keyword
   * @param values
   */
  public void setValues(String keyword, String[] values) {
    deleteKeyAll(keyword);
    ComScriptInputArg inputArg;
    for (int i = 0; i < values.length; i++) {
      inputArg = new ComScriptInputArg();
      stdinArgs.add(inputArg);
      inputArg.setArgument(keyword + divider + values[i]);
    }
  }

  public String toString() {
    return "[" + getCommand() + " " + stdinArgs + "]";
  }

  /**
   * Add a new key and value to the standard input argument list
   * @param keyword
   * @param value
   */
  public void addKey(String keyword, String value) {
    ComScriptInputArg newArg = new ComScriptInputArg();
    newArg.setArgument(keyword + divider + value);
    stdinArgs.add(newArg);
  }

  /**
   * Delete the comScriptInputArg elements it contains the specified key 
   * @param keyword
   * @return true if the keyword was found, false if it was not found
   */
  public boolean deleteKey(String keyword) {
    int idx = findKey(keyword);
    if (idx >= 0) {
      stdinArgs.remove(idx);
      return true;
    }
    return false;
  }

  /**
   * Delete all instances of the the keyword the std input arguments
   * @param keyword
   */
  public void deleteKeyAll(String keyword) {
    Iterator itStdinArgs = stdinArgs.iterator();
    while (itStdinArgs.hasNext()) {
      ComScriptInputArg inputArg = (ComScriptInputArg) itStdinArgs.next();

      String[] tokens = inputArg.getArgument().trim().split("\\s+", 2);
      if (tokens[0].equals(keyword)
          || (caseInsensitive && tokens[0].equalsIgnoreCase(keyword))) {
        itStdinArgs.remove();
      }
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
    if (keyword == null) {
      return -1;
    }
    for (int i = 0; i < stdinArgs.size(); i++) {
      ComScriptInputArg inputArg = (ComScriptInputArg) stdinArgs.get(i);
      String[] tokens = inputArg.getArgument().trim().split("\\s+", 2);
      if (tokens[0].equals(keyword)
          || (caseInsensitive && tokens[0].equalsIgnoreCase(keyword))) {
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