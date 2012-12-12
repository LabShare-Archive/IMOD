package etomo.comscript;

import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.process.SystemProgram;
import etomo.type.AxisID;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoVersion;

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
 * <p> Revision 1.3  2010/04/28 16:06:37  sueh
 * <p> Fixed the location of a comment.
 * <p>
 * <p> Revision 1.2  2010/02/17 04:47:53  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.1  2009/04/13 22:27:50  sueh
 * <p> bug# 1207 Class to represent the ssh command.  Builds ssh command with
 * <p> ConnectTimeout option when possible.
 * <p> </p>
 */
final class SshParam {
  public static final String rcsid = "$Id$";

  static final SshParam INSTANCE = new SshParam();

  private EtomoBoolean2 timeoutAvailable = null;

  final List<String> getCommand(BaseManager manager, boolean useTimeoutIfPossible, String computer) {
    List<String> command = new ArrayList<String>();
    command.add("ssh");
    command.add("-x");
    //prevents ssh from waiting for an answer when connecting to a computer for
    //the first time
    //see man ssh_config
    command.add("-o");
    command.add("StrictHostKeyChecking=no");
    if (useTimeoutIfPossible && isTimeoutAvailable(manager)) {
      //Timeout doesn't work with older versions of Redhat (see bug# 1043).
      //maximum connection timeout for a down computer
      command.add("-o");
      command.add("ConnectTimeout=5");
    }
    command.add("-o");
    //prevents password prompts when the publickey authentication fails
    command.add("PreferredAuthentications=publickey");
    command.add("-v");
    command.add(computer);
    return command;
  }

  /**
   * Sets timeoutAvailable based on the result of running "ssh -v".  Only sets
   * timeoutAvailable once.  If running ssh -v fails, timeoutAvailable is set to
   * false.
   * @return timeoutAvailable
   */
  synchronized boolean isTimeoutAvailable(BaseManager manager) {
    if (timeoutAvailable != null) {
      return timeoutAvailable.is();
    }
    //Set timeoutAvailable from "ssh -v".  OpenSSH with a version of 3.9 or
    //greater will understand the ConnectTimeout option.
    timeoutAvailable = new EtomoBoolean2();
    //Run ssh -v.
    SystemProgram systemProgram = new SystemProgram(manager, System
        .getProperty("user.dir"), new String[] { "ssh", "-v" }, AxisID.ONLY);
    systemProgram.run();
    //Find and parse the OpenSSH version.
    String[] stderr = systemProgram.getStdError();
    if (stderr != null && stderr.length > 0) {
      String appString = "openssh";
      int i = 0;
      while (stderr[i].toLowerCase().indexOf(appString) == -1) {
        i++;
      }
      if (i < stderr.length) {
        //Find and store the version of OpenSSH (OpenSSH_version, ...).
        String[] versionInfoArray = stderr[i].toLowerCase().trim().split("[_,\\s]+");
        if (versionInfoArray != null && versionInfoArray.length > 0) {
          i = 0;
          while (versionInfoArray[i++].toLowerCase().indexOf(appString) == -1) {
          }
          if (i < versionInfoArray.length) {
            EtomoVersion openSshVersion = EtomoVersion
                .getDefaultInstance(versionInfoArray[i]);
            if (openSshVersion.ge(EtomoVersion.getDefaultInstance("3.9"))) {
              timeoutAvailable.set(true);
              return true;
            }
          }
        }
      }
    }
    timeoutAvailable.set(false);
    return false;
  }
}
