package etomo.comscript;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.process.ProcessMessages;
import etomo.process.SystemProgram;
import etomo.storage.DirectiveFile;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.ProcessName;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public class BatchruntomoParam {
  public static final String rcsid = "$Id:$";

  private static final int VALIDATION_TYPE_BATCH_DIRECTIVE = 1;
  private static final int VALIDATION_TYPE_TEMPLATE = 2;

  private final List<String> command = new ArrayList<String>();
  private final EtomoNumber validationType = new EtomoNumber();
  private final List<String> directiveList = new ArrayList<String>();

  private final BaseManager manager;

  private StringBuffer commandLine = null;
  private SystemProgram batchruntomo = null;
  private int exitValue = -1;

  public BatchruntomoParam(BaseManager manager) {
    this.manager = manager;
  }

  public boolean setup() {
    // Create a new SystemProgram object for copytomocom, set the
    // working directory and stdin array.
    // Do not use the -e flag for tcsh since David's scripts handle the failure
    // of commands and then report appropriately. The exception to this is the
    // com scripts which require the -e flag. RJG: 2003-11-06
    command.add("python");
    command.add("-u");
    command.add(ApplicationManager.getIMODBinPath() + ProcessName.BATCHRUNTOMO);
    command.add("-validation");
    command.add(validationType.toString());
    Iterator<String> i = directiveList.iterator();
    while (i.hasNext()) {
      command.add("-directive");
      command.add(i.next());
    }
    batchruntomo = new SystemProgram(manager, manager.getPropertyUserDir(), command,
        AxisID.ONLY);
    batchruntomo.setMessagePrependTag("Beginning to process template file");
    return true;
  }

  public void setDirective(final DirectiveFile directiveFile) {
    if (directiveFile != null) {
      directiveList.add(directiveFile.getFile().getAbsolutePath());
    }
  }

  public boolean isValid() {
    return (validationType.equals(VALIDATION_TYPE_BATCH_DIRECTIVE) || validationType
        .equals(VALIDATION_TYPE_TEMPLATE)) && !directiveList.isEmpty();
  }

  public void setValidationType(final boolean directiveDrivenAutomation) {
    if (directiveDrivenAutomation) {
      validationType.set(VALIDATION_TYPE_BATCH_DIRECTIVE);
    }
    else {
      validationType.set(VALIDATION_TYPE_TEMPLATE);
    }
  }

  /**
   * Return the current command line string
   * 
   * @return
   */
  public String getCommandLine() {
    if (batchruntomo == null) {
      return "";
    }
    return batchruntomo.getCommandLine();
  }

  /**
   * Execute the copytomocoms script
   * 
   * @return @throws
   *         IOException
   */
  public int run() {
    if (batchruntomo == null) {
      return -1;
    }
    int exitValue;

    // Execute the script
    batchruntomo.run();
    exitValue = batchruntomo.getExitValue();
    return exitValue;
  }

  public String getStdErrorString() {
    if (batchruntomo == null) {
      return "ERROR: Batchruntomo is null.";
    }
    return batchruntomo.getStdErrorString();
  }

  public String getStdOutputString() {
    if (batchruntomo == null) {
      return "ERROR: Batchruntomo is null.";
    }
    return batchruntomo.getStdOutputString();
  }

  public String[] getStdError() {
    if (batchruntomo == null) {
      return new String[] { "ERROR: Batchruntomo is null." };
    }
    return batchruntomo.getStdError();
  }

  /**
   * returns a String array of warnings - one warning per element
   * make sure that warnings get into the error log
   * @return
   */
  public ProcessMessages getProcessMessages() {
    if (batchruntomo == null) {
      return null;
    }
    return batchruntomo.getProcessMessages();
  }
}
