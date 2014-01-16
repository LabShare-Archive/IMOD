package etomo.comscript;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import etomo.ApplicationManager;
import etomo.process.ProcessMessages;
import etomo.process.SystemProgram;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.ProcessName;
import etomo.ui.swing.UIExpertUtilities;

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
public class MakecomfileParam {
  public static final String rcsid = "$Id:$";

  private final List<String> command = new ArrayList<String>();
  private final EtomoNumber beadSize = new EtomoNumber(EtomoNumber.Type.DOUBLE);

  private final ApplicationManager manager;
  private final AxisID axisID;
  private final FileType fileType;

  private StringBuffer commandLine = null;
  private SystemProgram makecomfile = null;
  private int exitValue = -1;

  public MakecomfileParam(final ApplicationManager manager, final AxisID axisID,
      final FileType fileType) {
    this.manager = manager;
    this.axisID = axisID;
    this.fileType = fileType;
  }

  public boolean setup() {
    command.add("python");
    command.add("-u");
    command.add(ApplicationManager.getIMODBinPath() + ProcessName.MAKECOMFILE);
    if (fileType == FileType.GOLD_ERASER_COMSCRIPT
        || fileType == FileType.PATCH_TRACKING_COMSCRIPT) {
      command.add("-root");
      command.add(manager.getName() + axisID.getExtension());
      if (fileType == FileType.PATCH_TRACKING_COMSCRIPT) {
        command.add("-input");
        command.add(FileType.CROSS_CORRELATION_COMSCRIPT.getFileName(manager, axisID));
        command.add("-binning");
        command.add(String.valueOf(UIExpertUtilities.INSTANCE.getStackBinning(manager, axisID, ".preali")));
      }
      else if (fileType == FileType.GOLD_ERASER_COMSCRIPT) {
        if (beadSize.isNull()) {
          return false;
        }
        command.add("-bead");
        command.add(beadSize.toString());
      }
    }
    else if (fileType != FileType.AUTOFIDSEED_COMSCRIPT
        && fileType != FileType.SIRTSETUP_COMSCRIPT) {
      return false;
    }
    File file = FileType.LOCAL_SCOPE_TEMPLATE.getFile(manager, axisID);
    if (file.exists()) {
      command.add("-change");
      command.add(file.getName());
    }
    file = FileType.LOCAL_SYSTEM_TEMPLATE.getFile(manager, axisID);
    if (file.exists()) {
      command.add("-change");
      command.add(file.getName());
    }
    file = FileType.LOCAL_USER_TEMPLATE.getFile(manager, axisID);
    if (file.exists()) {
      command.add("-change");
      command.add(file.getName());
    }
    file = FileType.LOCAL_BATCH_DIRECTIVE_FILE.getFile(manager, axisID);
    if (file.exists()) {
      command.add("-change");
      command.add(file.getName());
    }
    command.add(fileType.getFileName(manager, axisID));
    makecomfile = new SystemProgram(manager, manager.getPropertyUserDir(), command,
        AxisID.ONLY);
    return true;
  }

  public void setBeadSize(final String input) {
    beadSize.set(input);
  }

  /**
   * Return the current command line string
   * 
   * @return
   */
  public String getCommandLine() {
    if (makecomfile == null) {
      return "";
    }
    return makecomfile.getCommandLine();
  }

  /**
   * Execute the copytomocoms script
   * 
   * @return @throws
   *         IOException
   */
  public int run() {
    if (makecomfile == null) {
      return -1;
    }
    int exitValue;

    // Execute the script
    makecomfile.run();
    exitValue = makecomfile.getExitValue();
    return exitValue;
  }

  public String getStdErrorString() {
    if (makecomfile == null) {
      return "ERROR: makecomfile is null.";
    }
    return makecomfile.getStdErrorString();
  }

  public String[] getStdError() {
    if (makecomfile == null) {
      return new String[] { "ERROR: makecomfile is null." };
    }
    return makecomfile.getStdError();
  }

  /**
   * returns a String array of warnings - one warning per element
   * make sure that warnings get into the error log
   * @return
   */
  public ProcessMessages getProcessMessages() {
    if (makecomfile == null) {
      return null;
    }
    return makecomfile.getProcessMessages();
  }
}
