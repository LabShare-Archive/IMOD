package etomo.uitest;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import junit.framework.Assert;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.UITestActionType;
import etomo.type.UITestSubjectType;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.7  2010/02/17 05:04:48  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up
 * <p> messages.
 * <p>
 * <p> Revision 1.6  2009/10/23 19:49:47  sueh
 * <p> bug# 1275 No default manager anymore so don't assume that manager is
 * <p> available.
 * <p>
 * <p> Revision 1.5  2009/03/17 00:46:33  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.4  2009/02/04 23:37:32  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.3  2009/01/20 20:47:38  sueh
 * <p> bug# 1102 Class to store an interface section.
 * <p>
 * <p> Revision 1.2  2008/05/30 22:37:18  sueh
 * <p> bug# 1102 Isolating the etomo.uitest package so it is not needed for
 * <p> running EtomoDirector.
 * <p>
 * <p> Revision 1.1  2008/05/30 21:36:33  sueh
 * <p> bug# 1102 Class representing the Interface sections in uitest.adoc.
 * <p> </p>
 */
final class InterfaceSection extends Assert {
  public static final String rcsid = "$Id$";

  private final String sectionName;

  Map commandMap = null;

  InterfaceSection(String sectionName) {
    this.sectionName = sectionName;
  }

  /**
   * Get the unique panel command from the section.  Used to get the main frame
   * for the current axis.
   * @return
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   */
  Command getGotoFrameCommand(AxisID axisID) throws FileNotFoundException, IOException,
      LogFile.LockException {
    readSection();
    return (Command) commandMap.get(UITestActionType.GOTO.toString()
        + UITestSubjectType.FRAME.toString() + axisID.getExtension());
  }

  /**
   * Get the open = interface subsection.
   * @return
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   */
  Command getOpenInterfaceCommand() throws FileNotFoundException, IOException,
      LogFile.LockException {
    readSection();
    Command command = (Command) commandMap.get(UITestActionType.OPEN.toString()
        + UITestSubjectType.INTERFACE.toString());
    return command;
  }

  /**
   * Get the open.dialog command for dialogName.
   * @param dialogName
   * @return
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   */
  Command getOpenDialogCommand(String dialogName) throws FileNotFoundException,
      IOException, LogFile.LockException {
    readSection();
    return (Command) commandMap.get(UITestActionType.OPEN.toString()
        + UITestSubjectType.DIALOG.toString() + dialogName);
  }

  /**
   * Reads the entire section and stores each command in a hash map, key by
   * action, subject, axis, and modifier (when they appear in the command).
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   */
  private void readSection() throws FileNotFoundException, IOException,
      LogFile.LockException {
    //Perform this command only one time.
    if (commandMap != null) {
      return;
    }
    commandMap = new HashMap();
    BaseManager manager = EtomoDirector.INSTANCE.getCurrentManagerForTest();
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(manager, AutodocFactory.UITEST,
        AxisID.ONLY);
    CommandReader reader = CommandReader.getSectionReader(autodoc, SectionType.INTERFACE
        .toString(), sectionName, null, null);
    Command command = null;
    while (!reader.isDone()) {
      command = reader.nextCommand(command);
      if (command != null && command.isKnown()) {
        //Build key for hash map
        StringBuffer key = new StringBuffer();
        //actionType
        UITestActionType actionType = command.getActionType();
        Subject subject = command.getSubject();
        assertNotNull("action type required for all interface section commands ("
            + command + ")", actionType);
        key.append(actionType.toString());
        //subject type
        UITestSubjectType subjectType = command.getSubjectType();
        if (subjectType != null) {
          key.append(subjectType.toString());
        }
        //subject name
        if (subject != null) {
          String name = subject.getName();
          if (name != null) {
            key.append(name);
          }
          //subject axis
          AxisID axisID = subject.getAxisID();
          if (axisID != AxisID.ONLY) {
            key.append(axisID.getExtension());
          }
        }
        if (command.isSubsection()) {
          key.append(command.getValue());
        }
        commandMap.put(key.toString(), command);
        //Each command must be saved in commandMap.  Set the command variable to
        //null to prevent the command reader from reusing it.
        command = null;
      }
    }
  }
}
