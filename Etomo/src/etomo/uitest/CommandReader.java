package etomo.uitest;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.LinkedList;

import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.Statement;
import etomo.storage.autodoc.StatementLocation;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlyStatement;
import etomo.storage.autodoc.ReadOnlyStatementList;
import etomo.storage.autodoc.SectionLocation;
import etomo.type.AxisID;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class CommandReader {
  public static final String rcsid = "$Id$";

  private final ReadOnlyAutodoc autodoc;
  private final LinkedList locationStack = new LinkedList();
  private final String autodocName;
  private final String sectionType;

  private ReadOnlyStatementList list = null;
  private SectionLocation sectionLoc = null;
  private StatementLocation statementLoc = null;
  private boolean readingSections = false;
  private boolean readingStatements = false;
  private boolean verbose = false;
  private boolean done = false;
  private String name = null;
  private ReadOnlyAutodoc functionAutodoc = null;
  private File functionLocationSourceDir = null;
  private AxisID axisID = null;
  private String info = null;
  private boolean function = false;
  private boolean debug = false;

  public CommandReader(ReadOnlyAutodoc autodoc, String sectionType) {
    this.autodoc = autodoc;
    this.sectionType = sectionType;
    sectionLoc = autodoc.getSectionLocation(sectionType);
    list = autodoc;
    name = autodoc.getName();
    autodocName = name;
    functionAutodoc = autodoc;
    setInfo();
  }

  public void setFunctionLocationSourceDir(File functionLocationSourceDir) {
    this.functionLocationSourceDir = functionLocationSourceDir;
  }

  public void setAxisID(AxisID axisID) {
    this.axisID = axisID;
    setInfo();
  }

  public void setVerbose() {
    verbose = true;
  }

  public boolean isReadingSections() {
    return readingSections;
  }

  public boolean isReadingStatements() {
    return readingStatements;
  }

  public boolean isDone() {
    return done;
  }

  public void setDebug(boolean input) {
    debug = input;
  }

  public void setDone() {
    done = true;
  }

  public String getName() {
    return name;
  }

  public void setInfo() {
    StringBuffer buffer = new StringBuffer();
    if (axisID != null) {
      buffer.append(axisID.toString() + ":");
    }
    if (name != null) {
      buffer.append(name.toString() + ":");
    }
    if (function) {
      if (functionAutodoc != null) {
        buffer.append(functionAutodoc.getName() + ":");
      }
      if (list != null) {
        buffer.append(list.getName() + ":");
      }
    }
    if (buffer.length() == 0) {
      info = CommandReader.class.getName();
    }
    else {
      info = buffer.toString();
    }
  }

  public String getInfo() {
    return info;
  }

  /**
   * Gets the next command in the current section.  An adoc command will cause
   * this function to push the current autodoc and open the autodoc
   * specified in the adoc command.  If the function is unable to get a command,
   * it will attempt to pop to a previous autodoc.  If a null command is passed
   * to it, the function will create a new command and return it, otherwise it
   * will reuse the command parameter.
   * @param command
   * @return
   */
  public UITestCommand nextCommand(UITestCommand command,
      UITestCommandFactory factory) {
    readingStatements = true;
    //create command if it is null
    if (command == null) {
      if (factory == null) {
        throw new IllegalArgumentException(
            "command is null and factory is null");
      }
      command = factory.newCommand();
    }
    command.reset();
    if (list == null) {
      return command;
    }
    //if starting a list, get the statement location
    if (statementLoc == null) {
      statementLoc = list.getStatementLocation();
    }
    //get the statement, ignoring comments and empty lines
    ReadOnlyStatement statement;
    do {
      statement = list.nextStatement(statementLoc);
    } while (statement != null
        && (statement.getType() == Statement.Type.COMMENT || statement
            .getType() == Statement.Type.EMPTY_LINE));
    if (statement == null) {
      //if this is the end of a function section, end the function and read the
      //next command from the calling autodoc
      if (function) {
        endFunction();
        return nextCommand(command, factory);
      }
      return command;
    }
    if (verbose) {
      System.err.println(axisID.toString() + ": " + statement.getString());
    }
    //place the statement in command
    command.set(statement);
    if (command.isFunctionLocation()) {
      //if the function location command (adoc) is found, change the function
      //autodoc and get the next command
      setFunctionAutodoc(command);
      return nextCommand(command, factory);
    }
    if (command.isFunction()) {
      //if the function command is found, get the function section and get the
      //first command from it
      callFunction(command);
      return nextCommand(command, factory);
    }
    return command;
  }

  public void nextSection() {
    if (sectionLoc == null) {
      throw new IllegalStateException(axisID
          + ": do not call nextSection on a secondary autodoc");
    }
    list = autodoc.nextSection(sectionLoc);
    setSection();
  }

  public void setSection(String sectionName) {
    if (sectionLoc == null) {
      throw new IllegalStateException(axisID
          + ": do not call setSection on a secondary autodoc");
    }
    list = autodoc.getSection(sectionType, sectionName);
    setSection();
  }

  private void setSection() {
    readingSections = true;
    statementLoc = null;
    name = null;
    readingStatements = false;
    if (list == null) {
      getInfo();
      done = true;
      if (verbose) {
        System.err.println(autodoc.getName() + ":  done with test.");
      }
      return;
    }
    name = list.getName();
    if (debug) {
    System.out.println("name=" + name);}
    getInfo();
    if (verbose) {
      System.err.println(axisID.toString() + ":" + list.getString());
    }
  }

  /**
   * Pushes the current location onto the stack and gets a new location based
   * on the function call.
   * @param command
   */
  private void callFunction(UITestCommand command) {
    if (functionLocationSourceDir == null) {
      throw new IllegalStateException(
          "A secondary autodo source directory must be set when secondary autodocs are in use.");
    }
    if (!command.isFunction()) {
      throw new IllegalStateException("command=" + command);
    }
    //push old location
    Location location = new Location(list, sectionLoc, statementLoc, function);
    locationStack.addLast(location);
    //set values from the current autodoc
    list = functionAutodoc.getSection(command.getAction().toString(), command
        .getValue());
    if (list == null) {
      throw new IllegalStateException(functionAutodoc.getName()
          + ":missing section:" + command.toString());
    }
    sectionLoc = null;
    statementLoc = null;
    function = true;
    setInfo();
    if (verbose) {
      System.err.println(axisID.toString() + ":start:" + list.getString());
    }
  }

  private void setFunctionAutodoc(UITestCommand command) {
    String functionLocation = command.getValue();
    if (functionLocation == null) {
      functionAutodoc = autodoc;
    }
    else {
      try {
        functionAutodoc = AutodocFactory.getInstance(functionLocationSourceDir,
            command.getValue(), AxisID.ONLY);
      }
      catch (FileNotFoundException e) {
        e.printStackTrace();
        return;
      }
      catch (IOException e) {
        e.printStackTrace();
        return;
      }
      catch (LogFile.ReadException e) {
        e.printStackTrace();
        return;
      }
      catch (LogFile.FileException e) {
        e.printStackTrace();
        return;
      }
    }
  }

  /**
   * the the top autodoc status off of the stack and makes it the current
   * autodoc
   */
  private void endFunction() {
    Location location = (Location) locationStack.removeLast();
    if (location == null) {
      list = null;
      return;
    }
    if (verbose) {
      System.err.println(axisID.toString() + ":end:" + list.getString());
    }
    list = location.getList();
    sectionLoc = location.getSectionLoc();
    statementLoc = location.getStatementLoc();
    function = location.isFunction();
    setInfo();
  }

  private static final class Location {
    private final ReadOnlyStatementList list;
    private final SectionLocation sectionLoc;
    private final StatementLocation statementLoc;
    private final boolean function;

    Location(ReadOnlyStatementList list, SectionLocation sectionLoc,
        StatementLocation statementLoc, boolean function) {
      this.list = list;
      this.sectionLoc = sectionLoc;
      this.statementLoc = statementLoc;
      this.function = function;
    }

    ReadOnlyStatementList getList() {
      return list;
    }

    SectionLocation getSectionLoc() {
      return sectionLoc;
    }

    StatementLocation getStatementLoc() {
      return statementLoc;
    }

    boolean isFunction() {
      return function;
    }

    public String toString() {
      return "list=" + list.getName() + ",\nsectionLoc=" + sectionLoc
          + ",\nstatementLoc=" + statementLoc;
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2008/05/30 21:35:55  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.3  2008/01/31 20:21:24  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.2  2007/04/09 19:36:31  sueh
 * <p> Change NameValuePair to an abstract class called statement and child classes
 * <p> representing name/value pair, comment, empty line, and subsection.  Made
 * <p> delimiter change an attribute of the name/value pair class.  Added
 * <p> ReadOnlyStatement to provide a public interface for Statement classes.
 * <p>
 * <p> Revision 1.1  2007/03/21 18:10:21  sueh
 * <p> bug# 964 Moved AdocCommand classes out of the autodoc package because
 * <p> they not part of the autodoc.
 * <p>
 * <p> Revision 1.8  2007/03/08 21:44:14  sueh
 * <p> bug# 964 Ignore comment and empty-line name/value pairs.
 * <p>
 * <p> Revision 1.7  2007/03/05 21:28:19  sueh
 * <p> bug# 964 Stop controlling autodoc instances, except for the standard ones.
 * <p>
 * <p> Revision 1.6  2007/03/01 01:15:03  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 1.5  2006/08/30 16:50:13  sueh
 * <p> bug# 852 Printing the function calls in verbose mode
 * <p>
 * <p> Revision 1.4  2006/08/18 23:14:01  sueh
 * <p> bug# 852 callFunction:  throw an exception when the function isn't found
 * <p>
 * <p> Revision 1.3  2006/08/08 17:45:57  sueh
 * <p> bug# 852 Removed the secondary autodoc functionality.  Added functionality to
 * <p> call a function section.  Added functionality to set the location of a function
 * <p> section.
 * <p>
 * <p> Revision 1.2  2006/05/18 20:52:19  sueh
 * <p> made compatible with java 1.4 (class getSimpleName doesn't exist in 1.4)
 * <p>
 * <p> Revision 1.1  2006/04/28 20:55:16  sueh
 * <p> bug# 787 Was UITestSection.  Reads name/value pairs sequentially in
 * <p> Autodoc files.  Can global or section name/value pairs.  Can jump to a
 * <p> section via section name.  Can only read from one section type.  Parses
 * <p> each name/value pair with an AdocCommand.
 * <p>
 * <p> Revision 1.1  2006/04/25 19:40:56  sueh
 * <p> bug# 787 Manages pairs, sections, and secondary autodocs.  Can get the
 * <p> next section or the next pair.  Automatically switches to and from
 * <p> secondary autodocs.
 * <p> </p>
 */
