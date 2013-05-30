/**
 * <p>Description: This object models a IMOD Com script.</p>
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
 * <p> Revision 3.13  2009/09/22 20:50:43  sueh
 * <p> bug# 1259 In order to process nonstandard tilt.com, added
 * <p> caseInsensitive and separateWithASpace.
 * <p>
 * <p> Revision 3.12  2007/09/07 00:17:21  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 3.11  2006/10/13 22:19:03  sueh
 * <p> bug# 927 Added getScriptCommandIndex(String,int,boolean).
 * <p>
 * <p> Revision 3.10  2005/09/01 17:45:34  sueh
 * <p> bug# 688 putting temporary prints (for finding cause of undistort
 * <p> parameters being set in xcorr) into the error log
 * <p>
 * <p> Revision 3.9  2005/08/24 23:06:13  sueh
 * <p> bug# 715 Temporarily adding print statement to writeComFile() in case
 * <p> problem with xcorr.com having arguments meant for undistort.com comes
 * <p> up again.
 * <p>
 * <p> Revision 3.8  2005/06/17 00:30:39  sueh
 * <p> bug# 685 Added getName() to get the name of the file without the path.
 * <p>
 * <p> Revision 3.7  2005/05/20 21:15:19  sueh
 * <p> bug# 664 writeComFile(): do not attempt to save to a file if the
 * <p> memory is very low.  If the save fails, the file could be truncated.
 * <p>
 * <p> Revision 3.6  2004/08/19 00:42:45  sueh
 * <p> bug# 508 Allow deletion of commands, creating commands based on a
 * <p> command index number, finding a command base on a command
 * <p> index number.  Added createCommand(String,int),
 * <p> deleteCommand(int), getScriptCommand(String,int,boolean),
 * <p> getScriptCommandIndex(String,int)
 * <p>
 * <p> Revision 3.5  2004/06/24 18:33:48  sueh
 * <p> bug# 482 getScriptCommandIndex will add a command that
 * <p> it doesn't find when addNew is on.
 * <p>
 * <p> Revision 3.4  2004/04/19 19:23:32  sueh
 * <p> bug# 409 creating command when ComScript is empty
 * <p>
 * <p> Revision 3.3  2004/04/12 16:57:24  sueh
 * <p> bug# 409 allow ComScript to function when the comscript
 * <p> file doesn't exist.
 * <p>
 * <p> Revision 3.2  2004/03/12 00:04:22  rickg
 * <p> Comment fixes
 * <p>
 * <p> Revision 3.1  2004/03/04 00:46:54  rickg
 * <p> Bug# 406 Correctly write out command when it isn't the first in the
 * <p> script
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.6  2003/07/11 23:20:13  rickg
 * <p> Automatically switch of comment parsing if keyword/value file
 * <p>
 * <p> Revision 2.5  2003/03/20 17:21:30  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.4  2003/03/07 07:22:49  rickg
 * <p> combine layout in progress
 * <p>
 * <p> Revision 2.3  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.2  2003/03/06 01:19:17  rickg
 * <p> Combine changes in progress
 * <p>
 * <p> Revision 2.1  2003/02/24 23:29:05  rickg
 * <p> comment fix to match Eclipse tags
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2  2003/01/08 00:57:04  rickg
 * <p> Reformat
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
package etomo.comscript;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import etomo.EtomoDirector;

//TODO check for necessary defensive copying

public class ComScript {
  public static final String rcsid = "$Id$";

  private File comFile;
  private ArrayList scriptCommands = new ArrayList();

  private boolean parseComments = true;

  private boolean commandLoaded = false; // true when at least one command has be found or

  // created

  public ComScript(File comFile) {
    this.comFile = comFile;
  }

  /**
   * Copy constructor, creates a deep copy of the supplied ComScript object.
   * @param srcComScript the ComScript to be copied
   */
  public ComScript(File comFile, ComScript srcComScript) {
    this.comFile = comFile;
    // Copy the ComScriptCommands from the source object
    int nCommands = srcComScript.getCommandCount();
    scriptCommands.ensureCapacity(nCommands);
    for (int i = 0; i < nCommands; i++) {
      scriptCommands.add(srcComScript.getScriptCommand(i));
    }
  }

  /**
   * Read in the specified com script from the file system parsing the command,
   * comments and arguments into an internal representation.
   * @throws FileNotFoundException from FileReader.
   * @throws IOException from BufferedReader.
   * @throws BadComScriptException if the com script has a syntax error.
   */
  public void readComFile(boolean caseInsensitive, boolean separateWithASpace)
      throws FileNotFoundException, IOException, BadComScriptException {

    // Open the com file for reading using a buffered reader
    if (!comFile.exists()) {
      return;
    }
    BufferedReader in = null;
    in = new BufferedReader(new FileReader(comFile));

    // Read in the lines of the command file, assigning each one to the correct
    // list
    boolean flgContinuation = false;
    ComScriptCommand currentScriptCommand = null;
    ArrayList currentCommentBlock = new ArrayList();

    String line;
    int lineNumber = 0;
    while ((line = in.readLine()) != null) {
      lineNumber++;

      // If the first character is a pound place on the comment list
      if (line.startsWith("#")) {
        currentCommentBlock.add(line);
      }

      // If the first character is a $ not followed by !
      // create a new ComScriptCommand object and insert onto scriptCommands
      // insert the current comment block into the header comments then clear
      // the current comment block
      // parse the command line setting the command and command line
      // arguments
      else if (line.matches("^\\$[^!].*")) {
        currentScriptCommand = new ComScriptCommand(caseInsensitive, separateWithASpace);
        scriptCommands.add(currentScriptCommand);

        if (currentCommentBlock.size() > 0) {
          String[] commentArray = new String[currentCommentBlock.size()];
          commentArray = (String[]) currentCommentBlock.toArray(commentArray);
          currentScriptCommand.setHeaderComments(commentArray);
          currentCommentBlock.clear();
        }

        // Split the line into tokens at whitespace boundaries
        String noDollarSign = line.substring(1);
        noDollarSign = noDollarSign.trim();
        String[] tokens = noDollarSign.split("\\s+");
        currentScriptCommand.setCommand(tokens[0]);
        commandLoaded = true;

        // Check to if see if this line is continued
        int nShrink = 1;
        if (tokens[tokens.length - 1].equals("\\")) {
          nShrink = 2;
          flgContinuation = true;
        }

        String[] cmdLineArgs = new String[tokens.length - nShrink];
        for (int i = 0; i < cmdLineArgs.length; i++) {
          cmdLineArgs[i] = tokens[i + 1];
        }
        currentScriptCommand.setCommandLineArgs(cmdLineArgs);
        // Force the comment parsing from the standard input lines to off
        // if a keyword/value pair input format is detected
        if (currentScriptCommand.isKeywordValuePairs()) {
          parseComments = false;
        }
      }

      // Otherwise the line is assumed to be an input parmeter to the current
      // command or a continuation line
      else {
        if (flgContinuation) {
          // Get any comments associated with the continuation line and add
          // them to the header comments
          if (currentCommentBlock.size() > 0) {
            String[] commentArray = new String[currentCommentBlock.size()];
            commentArray = (String[]) currentCommentBlock.toArray(commentArray);
            currentScriptCommand.appendHeaderComments(commentArray);
            currentCommentBlock.clear();
          }

          // Split the line into tokens checking to see if the last token is
          // another line continuation
          String[] tokens = line.trim().split("\\s+");
          int nShrink = 0;
          if (tokens[tokens.length - 1].equals("\\")) {
            nShrink = 1;
            flgContinuation = true;
          }
          else {
            flgContinuation = false;
          }

          String[] cmdLineArgs = new String[tokens.length - nShrink];
          for (int i = 0; i < cmdLineArgs.length; i++) {
            cmdLineArgs[i] = tokens[i];
          }
          currentScriptCommand.appendCommandLineArgs(cmdLineArgs);

        }
        else {
          if (currentScriptCommand == null) {
            String description = "Input parameter found before command in "
                + comFile.getAbsoluteFile() + " line: " + String.valueOf(lineNumber);
            if (in != null) {
              in.close();
            }
            throw new BadComScriptException(description);
          }

          ComScriptInputArg inputArg = new ComScriptInputArg();

          if (currentCommentBlock.size() > 0) {
            String[] commentArray = new String[currentCommentBlock.size()];
            commentArray = (String[]) currentCommentBlock.toArray(commentArray);
            inputArg.setComments(commentArray);
            currentCommentBlock.clear();
          }

          inputArg.setArgument(line, parseComments);

          currentScriptCommand.appendInputArgument(inputArg);
        }
      }
    }
    // Close the com script
    in.close();
  }

  /**
   * Get the command names in script as a string array.  The array
   * returned is in the same order as the commands in the script.
   * @return a String array containing the commands in the script.
   */
  public String[] getCommandArray() {
    String[] commandArray = new String[scriptCommands.size()];
    for (int i = 0; i < commandArray.length; i++) {
      ComScriptCommand command = (ComScriptCommand) scriptCommands.get(i);
      commandArray[i] = command.getCommand();
    }
    return commandArray;
  }

  /**
   * Return the specified ComSrciptCommand element according to the commandArray
   * described in getCommandArray
   */
  public ComScriptCommand getScriptCommand(int index) {
    return (ComScriptCommand) scriptCommands.get(index);
  }

  /**
   * Return the first instance of ComScriptCommand with the specified command 
   * @param cmdName a String containing the name of the command.
   * @return the first ComScriptCommand in the collection that matches cmdName,
   * null if no command with the specified name is found.
   */
  public ComScriptCommand getScriptCommand(String cmdName, boolean caseInsensitive,
      boolean separateWithASpace) throws BadComScriptException {
    if (!commandLoaded) {
      createCommand(cmdName, caseInsensitive, separateWithASpace);
    }
    for (int i = 0; i < scriptCommands.size(); i++) {
      ComScriptCommand command = (ComScriptCommand) scriptCommands.get(i);
      if (command.getCommand().equals(cmdName)) {
        return command;
      }
    }
    throw (new BadComScriptException("Did not find command: " + cmdName));
  }

  /**
   * Return the instance of ComScriptCommand with the specified command
   * corresponding to commandIndex
   * @param cmdName a String containing the name of the command.
   * @return the first ComScriptCommand in the collection that matches cmdName,
   * null if no command with the specified name is found.
   */
  public ComScriptCommand getScriptCommand(String cmdName, int commandIndex,
      boolean addNew, boolean caseInsensitive, boolean separateWithASpace)
      throws BadComScriptException {
    if (!commandLoaded) {
      createCommand(cmdName, commandIndex, caseInsensitive, separateWithASpace);
    }
    ComScriptCommand command = (ComScriptCommand) scriptCommands.get(commandIndex);
    if (command.getCommand().equals(cmdName)) {
      return command;
    }
    if (addNew) {
      createCommand(cmdName, commandIndex, caseInsensitive, separateWithASpace);
      command = (ComScriptCommand) scriptCommands.get(commandIndex);
      if (command.getCommand().equals(cmdName)) {
        return command;
      }
    }
    throw (new BadComScriptException("Did not find command: " + cmdName + " at index "
        + commandIndex));
  }

  /**
   * 
   * @param cmdName
   * @return
   */
  protected int createCommand(String cmdName, boolean caseInsensitive,
      boolean separateWithASpace) {
    ComScriptCommand currentScriptCommand = new ComScriptCommand(caseInsensitive,
        separateWithASpace);
    scriptCommands.add(currentScriptCommand);
    currentScriptCommand.setCommand(cmdName);
    commandLoaded = true;
    return scriptCommands.size() - 1;
  }

  /**
   * 
   * @param cmdName
   * @param commandIndex
   * @return
   */
  protected void createCommand(String cmdName, int commandIndex, boolean caseInsensitive,
      boolean separateWithASpace) {
    ComScriptCommand currentScriptCommand = new ComScriptCommand(caseInsensitive,
        separateWithASpace);
    scriptCommands.add(commandIndex, currentScriptCommand);
    currentScriptCommand.setCommand(cmdName);
    commandLoaded = true;
  }

  public void deleteCommand(int commandIndex) {
    scriptCommands.remove(commandIndex);
    if (scriptCommands.isEmpty()) {
      commandLoaded = true;
    }
  }

  /**
   * Return the index of the specified command or -1 if the command is not
   * present in the script
   * @param cmdName the name of the command to find
   * @return index of the command or -1 if not present
   */
  public int getScriptCommandIndex(String cmdName, boolean caseInsensitive,
      boolean separateWithASpace) {
    return getScriptCommandIndex(cmdName, false, caseInsensitive, separateWithASpace);
  }

  public int getScriptCommandIndex(String cmdName, boolean addNew,
      boolean caseInsensitive, boolean separateWithASpace) {
    if (!commandLoaded) {
      createCommand(cmdName, caseInsensitive, separateWithASpace);
    }
    for (int i = 0; i < scriptCommands.size(); i++) {
      ComScriptCommand command = (ComScriptCommand) scriptCommands.get(i);
      if (command.getCommand().equals(cmdName)) {
        return i;
      }
    }
    if (addNew) {
      return createCommand(cmdName, caseInsensitive, separateWithASpace);
    }
    return -1;
  }

  public int getScriptCommandIndex(String cmdName, int commandIndex, boolean addNew,
      boolean caseInsensitive, boolean separateWithASpace) {
    if (!commandLoaded) {
      createCommand(cmdName, caseInsensitive, separateWithASpace);
    }
    ComScriptCommand command = (ComScriptCommand) scriptCommands.get(commandIndex);
    if (command.getCommand().equals(cmdName)) {
      return commandIndex;
    }
    if (addNew) {
      return createCommand(cmdName, caseInsensitive, separateWithASpace);
    }
    return -1;
  }

  /**
   * 
   * @param cmdName
   * @param commandIndex
   * @return
   */
  public int getScriptCommandIndex(String cmdName, int commandIndex,
      boolean caseInsensitive, boolean separateWithASpace) {
    if (!commandLoaded) {
      createCommand(cmdName, caseInsensitive, separateWithASpace);
    }
    ComScriptCommand command = (ComScriptCommand) scriptCommands.get(commandIndex);
    if (command.getCommand().equals(cmdName)) {
      return commandIndex;
    }
    return -1;
  }

  /**
   * Write out the command file currently represented by this object
   */
  public void writeComFile() throws FileNotFoundException, IOException,
      BadComScriptException {
    if (!EtomoDirector.INSTANCE.isMemoryAvailable()) {
      return;
    }
    // TEMP
    System.err.println("write " + comFile.getName());
    // Open the com file for writing using a buffered writer
    BufferedWriter out = new BufferedWriter(new FileWriter(comFile));

    // Write out the the list of sript commands
    Iterator cmdIterator = scriptCommands.iterator();
    while (cmdIterator.hasNext()) {
      ComScriptCommand scriptCommand = (ComScriptCommand) cmdIterator.next();

      // Write out the header comments if they exist
      String[] headerComments = scriptCommand.getHeaderComments();
      for (int i = 0; i < headerComments.length; i++) {
        out.write(headerComments[i]);
        out.newLine();
      }

      // Write out the the command and any command line arguments
      out.write("$" + scriptCommand.getCommand());
      String[] commandArguments = scriptCommand.getCommandLineArgs();
      for (int i = 0; i < commandArguments.length; i++) {
        out.write(" " + commandArguments[i]);
      }
      out.newLine();

      // Write the input arguments for the command
      ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
      for (int i = 0; i < inputArgs.length; i++) {
        String[] argComments = inputArgs[i].getComments();
        for (int j = 0; j < argComments.length; j++) {
          out.write(argComments[j]);
          out.newLine();
        }
        out.write(inputArgs[i].getArgument());
        out.newLine();
      }
    }
    out.close();
  }

  /**
   * Set the specified ScriptCommand object to a new ScriptCommand object
   * @param index the index of the ScriptCommand object to replace.
   * @param scriptCommand the new ScriptCommand object.
   */
  public void setScriptComand(int index, ComScriptCommand newScriptCommand) {
    // make a defensive copy of the scriptCommand object
    scriptCommands.set(index, new ComScriptCommand(newScriptCommand));
  }

  /**
   * Remove the specified ComScriptCommand from the collection.
   * @param index the index of the ComScriptCommand to remove.
   */
  public void removeScriptCommand(int index) {
    scriptCommands.remove(index);
  }

  /**
   * Add the specified ComScriptCommand to the end of the collection.
   * @param index the index of the ComScriptCommand to add.
   */
  public void addScriptCommand(ComScriptCommand newScriptCommand) {
    scriptCommands.add(new ComScriptCommand(newScriptCommand));
  }

  /**
   * Get the com file name.
   * @return a String containing the absolute path of com file.
   */
  public String getComFileName() {
    return comFile.getAbsolutePath();
  }

  /**
   * Get the com file name.
   * @return a String containing the name of the com file.
   */
  public String getName() {
    return comFile.getName();
  }

  public boolean isCommandLoaded() {
    return commandLoaded;
  }

  /**
   * Get the number of commands in the script
   * @return an integer describing the number of commands in the script.
   */
  public int getCommandCount() {
    return scriptCommands.size();
  }

  public void setParseComments(boolean state) {
    parseComments = state;
  }
}
