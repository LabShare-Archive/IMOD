package etomo.uitest;

import junit.framework.Assert;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.storage.autodoc.ReadOnlySectionList;
import etomo.storage.autodoc.ReadOnlyStatement;
import etomo.storage.autodoc.ReadOnlyStatementList;
import etomo.storage.autodoc.SectionLocation;
import etomo.storage.autodoc.StatementLocation;
import etomo.type.AxisID;

/**
 * <p>Description: Reads statements in order from one or many sections or a
 * subsections.  Receives either an autodoc or a section to read from.</p>
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
public final class CommandReader extends Assert {
  public static final String rcsid = "$Id$";

  private final ReadOnlySectionList sectionList;
  private final AxisID axisID;
  private final String sectionType;
  private final VariableList variableList;

  private SectionLocation sectionLocation = null;
  private ReadOnlyStatementList statementList = null;
  private StatementLocation statementLocation = null;
  private boolean done = false;
  private boolean debug = false;

  /**
   * Create a reader that reads all the sections of sectionType in the autodoc.
   * @param autodoc
   * @param sectionType
   * @param axisID
   */
  public static CommandReader getAutodocReader(ReadOnlyAutodoc autodoc,
      String sectionType, AxisID axisID, VariableList variableList) {
    CommandReader autodocReader = new CommandReader(autodoc, sectionType, null, axisID,
        variableList);
    assertFalse("syntax error in autodoc - " + autodoc.getName(), autodoc.isError());
    return autodocReader;
  }

  /**
   * Create a reader that reads one section in autodoc.
   * @param autodoc
   * @param sectionType
   * @param sectionName
   */
  public static CommandReader getSectionReader(ReadOnlyAutodoc autodoc,
      String sectionType, String sectionName, AxisID axisID, VariableList variableList) {
    CommandReader sectionReader = new CommandReader(autodoc, sectionType, sectionName,
        axisID, variableList);
    assertFalse("syntax error in autodoc - " + autodoc.getName(), autodoc.isError());
    return sectionReader;
  }

  /**
   * Create a reader that reads a subset of commands in one subsection in a
   * section.  Only reads commands where the first attribute equals
   * subsetAttribute.
   * @param autodoc
   * @param sectionType
   * @param sectionName
   * @param subsectionType
   * @param subSectionName
   * @param subsetAttribute
   * @param axisID
   * @return
   */
  public static CommandReader getSubsectionReader(ReadOnlySectionList sectionList,
      ReadOnlySection subsection, AxisID axisID, VariableList variableList) {
    return new CommandReader(sectionList, subsection, axisID, variableList);
  }

  /**
   * Sets sectionList to either an autodoc or a section.  Sets statementList to
   * a section.  sectionLocation is null if only one section is being used.
   * @param autodoc
   * @param sectionType
   * @param sectionName
   * @param axisID
   * @param variableList
   */
  private CommandReader(ReadOnlyAutodoc autodoc, String sectionType, String sectionName,
      AxisID axisID, VariableList variableList) {
    this.axisID = axisID;
    this.variableList = variableList;
    this.sectionList = autodoc;
    this.sectionType = sectionType;
    //Get a section from the autodoc.
    // Sets sectionLocation to the first section in sectionList, if sectionName is
    // empty.  Sets it to null if sectionName is set.
    if (sectionName == null) {
      //reading an autodoc
      sectionLocation = sectionList.getSectionLocation(sectionType);
      statementList = sectionList.nextSection(sectionLocation);
    }
    else {
      //reading a section
      sectionLocation = null;
      statementList = sectionList.getSection(sectionType, sectionName);
    }
    if (statementList == null) {
      done = true;
    }
    else {
      statementLocation = statementList.getStatementLocation();
    }
  }

  /**
   * Sets statementList to the subsection that was passed in.  SectionLocation
   * is null.  SectionList is passed in only so its name can be used in print
   * statements.
   * @param subsection
   * @param axisID
   * @param variableList
   */
  private CommandReader(ReadOnlySectionList sectionList, ReadOnlySection subsection,
      AxisID axisID, VariableList variableList) {
    this.axisID = axisID;
    this.variableList = variableList;
    this.sectionList = sectionList;
    this.sectionType = null;
    //The passed in subsection is the statementList. SectionList is only used
    //for printing.
    sectionLocation = null;
    statementList = subsection;
    if (statementList == null) {
      done = true;
    }
    else {
      statementLocation = statementList.getStatementLocation();
    }
  }

  /**
   * Get the next section in the sectionList.
   *
   */
  void nextSection() {
    if (debug) {
      Thread.dumpStack();
    }
    statementList = sectionList.nextSection(sectionLocation);
    if (statementList == null) {
      if (debug) {
        System.out.println("nextSection:statementList is null");
      }
      done = true;
      return;
    }
    statementLocation = statementList.getStatementLocation();
    statementLocation.setDebug(debug);
  }

  String getSectionName() {
    return statementList.getName();
  }

  /**
   * Gets the current statement and set command to it.  If command is null, will
   * construct a new ActionCommand.  Otherwise it used the one that was passed
   * to it.
   * @param command
   * @return
   */
  Command nextCommand(Command command) {
    ReadOnlyStatement statement = nextStatement();
    if (debug) {
      System.out.println("nextCommand:statement=" + statement);
    }
    if (statement == null) {
      return null;
    }
    if (command == null) {
      command = new Command(axisID);
    }
    command.set(statement, variableList);
    if (debug) {
      System.out.println("nextCommand:command.isKnown()=" + command.isKnown());
    }
    return command;
  }

  /**
   * Gets the current statement from statementList and increments
   * statementLocation.  If there are no more statements, or statementList is
   * null, returns null.
   * @return
   */
  private ReadOnlyStatement nextStatement() {
    if (statementList == null) {
      //NextSection or getSection failed, so there is nothing left to read.
      if (debug) {
        System.out.println("nextStatement:statementList is null");
      }
      done = true;
    }
    else {
      ReadOnlyStatement statement = statementList.nextStatement(statementLocation);
      if (statement == null) {
        if (sectionLocation == null) {
          //Only reading one section and finished with it, so done.
          if (debug) {
            System.out
                .println("nextStatement:statement is null & sectionLocation is null");
          }
          done = true;
        }
      }
      else {
        System.err.println("### " + sectionList.getName() + ":"
            + (sectionType != null ? sectionType : "") + ":" + statementList.getName()
            + ":" + (axisID != null ? axisID.getExtension() : "") + ":"
            + statement.getString());
        return statement;
      }
    }
    return null;
  }

  /**
   * True when reader runs out of things to read.  When this happens depends on
   * how it was constructed.  If the reader was constructed with a section type
   * and a section name, it will set done to true after finishing the one
   * section refered to by type and name.  If the reader was contructed with
   * just a section type, then it will set done to true after reading all the
   * section of the section type set in the constructor.
   * @return
   */
  boolean isDone() {
    return done;
  }

  /**
   * Force the reader to be done.
   */
  void setDone() {
    done = true;
  }

  void setDebug() {
    debug = true;
    if (statementLocation != null) {
      statementLocation.setDebug(debug);
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.4  2009/09/01 03:18:33  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.3  2009/01/20 20:46:35  sueh
 * <p> bug# 1102 Class to read uitest commands.
 * <p>
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
