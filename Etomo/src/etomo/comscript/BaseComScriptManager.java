package etomo.comscript;

import java.io.File;

import javax.swing.JOptionPane;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.FileType;
import etomo.ui.UIHarness;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2010</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$ </p>
 */
public abstract class BaseComScriptManager {
  public static final String rcsid = "$Id$";

  private final BaseManager manager;

  BaseComScriptManager(BaseManager manager) {
    this.manager = manager;
  }

  ComScript loadComScript(FileType scriptFileType, AxisID axisID,
      boolean parseComments, boolean caseInsensitive, boolean separateWithASpace) {
    return loadComScript(scriptFileType.getFileName(manager, axisID), axisID,
        parseComments, true, caseInsensitive, separateWithASpace);
  }

  /**
   * Load the comscript file in commandComScript.
   * @param commandComScript - file name
   * @param axisID
   * @param parseComments
   * @param required
   * @param caseInsensitive
   * @param separateWithASpace
   * @return
   */
  ComScript loadComScript(String commandComScript, AxisID axisID,
      boolean parseComments, boolean required, boolean caseInsensitive,
      boolean separateWithASpace) {
    Utilities.timestamp("load", commandComScript, Utilities.STARTED_STATUS);
    File comFile = new File(manager.getPropertyUserDir(), commandComScript);
    //If the file isn't there and its not required then just return null without
    //any error messages.
    if (!required && !comFile.exists()) {
      return null;
    }
    ComScript comScript = new ComScript(comFile);
    try {
      comScript.setParseComments(parseComments);
      comScript.readComFile(caseInsensitive, separateWithASpace);
    }
    catch (Exception except) {
      except.printStackTrace();
      String[] errorMessage = new String[2];
      errorMessage[0] = "Com file: " + comScript.getComFileName();
      errorMessage[1] = except.getMessage();
      JOptionPane.showMessageDialog(null, errorMessage, "Can't parse "
          + commandComScript + axisID.getExtension() + ".com file: "
          + comScript.getComFileName(), JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("load", commandComScript, Utilities.FAILED_STATUS);
      return null;
    }
    Utilities.timestamp("load", commandComScript, Utilities.FINISHED_STATUS);
    return comScript;
  }

  /**
   * Initialize the CommandParam object from the specified command in the
   * comscript.  True is returned if the initialization is successful, false if
   * the initialization fails.
   * 
   * @param param
   * @param comScript
   * @param command
   * @param axisID
   * @return boolean
   */
  boolean initialize(CommandParam param, ComScript comScript, String command,
      AxisID axisID, boolean caseInsensitive, boolean separateWithASpace) {
    return initialize(param, comScript, command, axisID, false,
        caseInsensitive, separateWithASpace);
  }

  boolean initialize(CommandParam param, ComScript comScript, String command,
      AxisID axisID, boolean optionalCommand, boolean caseInsensitive,
      boolean separateWithASpace) {
    if (comScript == null) {
      return false;
    }
    Utilities.timestamp("initialize", command, comScript,
        Utilities.STARTED_STATUS);
    if (!comScript.isCommandLoaded()) {
      param.initializeDefaults();
    }
    else {
      if (optionalCommand) {
        if (comScript.getScriptCommandIndex(command, caseInsensitive,
            separateWithASpace) == -1) {
          Utilities.timestamp("initialize", command, comScript,
              Utilities.FAILED_STATUS);
          return false;
        }
      }
      try {
        param.parseComScriptCommand(comScript.getScriptCommand(command,
            caseInsensitive, separateWithASpace));
      }
      catch (Exception except) {
        except.printStackTrace();
        String[] errorMessage = new String[4];
        errorMessage[0] = "Com file: " + comScript.getComFileName();
        errorMessage[1] = "Command: " + command;
        errorMessage[2] = except.getClass().getName();
        errorMessage[3] = except.getMessage();
        JOptionPane.showMessageDialog(null, errorMessage,
            "Com Script Command Parse Error", JOptionPane.ERROR_MESSAGE);
        Utilities.timestamp("initialize", command, comScript,
            Utilities.FAILED_STATUS);
        return false;
      }
    }
    Utilities.timestamp("initialize", command, comScript,
        Utilities.FINISHED_STATUS);
    return true;
  }

  /**
   * Update the specified comscript with 
   * @param script
   * @param params
   * @param command
   * @param axisID
   */
  void modifyCommand(ComScript script, CommandParam params, String command,
      AxisID axisID, boolean caseInsensitive, boolean separateWithASpace) {
    modifyCommand(script, params, command, axisID, false, false,
        caseInsensitive, separateWithASpace);
  }

  /**
   * Modify and/or add command (depending on addNew boolean)
   * May treate command as optional, depending on optional boolean
   * @param script
   * @param params
   * @param command
   * @param axisID
   * @param addNew
   * @param optional
   * @return
   */
  boolean modifyCommand(ComScript script, CommandParam params, String command,
      AxisID axisID, boolean addNew, boolean optional, boolean caseInsensitive,
      boolean separateWithASpace) {
    if (script == null) {
      (new IllegalStateException()).printStackTrace();
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to update comscript.";
      errorMessage[1] = "\nscript=" + script + "\ncommand=" + command;
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage,
          "ComScriptManager Error", axisID);
      return false;
    }
    Utilities.timestamp("update", command, script, Utilities.STARTED_STATUS);

    //  Update the specified com script command from the CommandParam object
    ComScriptCommand comScriptCommand = null;
    int commandIndex = script.getScriptCommandIndex(command, addNew,
        caseInsensitive, separateWithASpace);
    //optional return false if failed
    if (optional && commandIndex == -1) {
      Utilities.timestamp("updateComScript", command, script,
          Utilities.FAILED_STATUS);
      return false;
    }
    try {
      comScriptCommand = script.getScriptCommand(command, caseInsensitive,
          separateWithASpace);
      params.updateComScriptCommand(comScriptCommand);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(null, errorMessage, "Can't update "
          + command + " in " + script.getComFileName(),
          JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("update", command, script, Utilities.FAILED_STATUS);
      return false;
    }

    // Replace the specified command by the updated comScriptCommand
    script.setScriptComand(commandIndex, comScriptCommand);

    //  Write the script back out to disk
    try {
      script.writeComFile();
    }
    catch (Exception except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane
          .showMessageDialog(null, except.getMessage(), "Can't write "
              + command + axisID.getExtension() + ".com",
              JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("update", command, script, Utilities.FAILED_STATUS);
      return false;
    }
    Utilities.timestamp("update", command, script, Utilities.FINISHED_STATUS);
    return true;
  }
}
