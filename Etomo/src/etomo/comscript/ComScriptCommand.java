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

  private String[] headerComments = new String[0];
  private String command = null;
  private String[] commandLineArgs = new String[0];
  private LinkedList comScriptInputArgs = new LinkedList();

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
    headerComments = src.getHeaderComments();
    command = src.getCommand();
    commandLineArgs = src.getCommandLineArgs();
    ComScriptInputArg[] inputArgs = src.getInputArguments();
    for (int i = 0; i < inputArgs.length; i++) {
      comScriptInputArgs.add(inputArgs[i]);
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
    for(int i=0; i < oldComments.length; i++){
      headerComments[i] = oldComments[i];
    }
    for(int i=0; i < moreComments.length; i++){
      headerComments[i+oldComments.length] = moreComments[i];
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
  public void setCommandLineArgs(String[] commandLineArgs) {
    //  make a defensive copy of the array
    this.commandLineArgs = new String[commandLineArgs.length];
    for (int i = 0; i < commandLineArgs.length; i++) {
      this.commandLineArgs[i] = commandLineArgs[i];
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
    comScriptInputArgs.add(new ComScriptInputArg(inputArg));
  }

  /**
   * Get the command input arguments.  These are the values (and associated
   * comments) that are meant for the stanndard input to the command.
   * @return a array of ComScriptInputArgs, this is a copy of the internal
   * representation.
   */
  public ComScriptInputArg[] getInputArguments() {
    ComScriptInputArg[] inputArgs =
      new ComScriptInputArg[comScriptInputArgs.size()];
    return (ComScriptInputArg[]) comScriptInputArgs.toArray(inputArgs);
  }

  /**
   * Set the ith input argument to the supplied parameter
   * @param index an integer specifying which argument to replace.
   * @param inputArg a ComScriptInputArg containing the input line and comments
   */
  public void setInputArgument(int index, ComScriptInputArg inputArg) {
    //  create a defensive copy of the input argument object
    comScriptInputArgs.set(index, new ComScriptInputArg(inputArg));
  }

  /**
   * Set all of the input arguments, erasing any existing input arguments.
   * @param inputArgs a ComScriptInputArg array containing all of the input
   * arguments
   */
  public void setInputArguments(ComScriptInputArg[] inputArgs) {
    //  Clear the current list
    comScriptInputArgs.clear();

    //  create a defensive copy of the input argument object
    for (int i = 0; i < inputArgs.length; i++) {
      comScriptInputArgs.add(new ComScriptInputArg(inputArgs[i]));
    }
  }

}
