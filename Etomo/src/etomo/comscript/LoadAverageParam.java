package etomo.comscript;

import java.util.ArrayList;
import java.util.Hashtable;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.util.EnvironmentVariable;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class LoadAverageParam implements IntermittentCommand {
  public static final String rcsid = "$Id$";

  private static Hashtable instances = new Hashtable();//one instance per computer

  private final String computer;
  private final BaseManager manager;
  private String[] localCommandArray = null;
  private String[] remoteCommandArray = null;
  private String intermittentCommand = null;
  private String endCommand = null;

  public final static LoadAverageParam getInstance(String computer,
      BaseManager manager) {
    LoadAverageParam loadAverageParam = (LoadAverageParam) instances
        .get(computer);
    if (loadAverageParam != null) {
      return loadAverageParam;
    }
    synchronized (instances) {
      loadAverageParam = (LoadAverageParam) instances.get(computer);
      if (loadAverageParam != null) {
        return loadAverageParam;
      }
      loadAverageParam = new LoadAverageParam(computer, manager);
      instances.put(computer, loadAverageParam);
      return loadAverageParam;
    }
  }

  private LoadAverageParam(String computer, BaseManager manager) {
    this.computer = computer;
    this.manager = manager;
  }

  public final String[] getLocalCommand() {
    if (localCommandArray == null) {
      buildLocalCommand();
    }
    return localCommandArray;
  }

  public final String[] getRemoteCommand() {
    if (remoteCommandArray == null) {
      buildRemoteCommand();
    }
    return remoteCommandArray;
  }

  public String getIntermittentCommand() {
    if (intermittentCommand == null) {
      buildIntermittentCommand();
    }
    return intermittentCommand;
  }

  public String getEndCommand() {
    if (endCommand == null) {
      buildEndCommand();
    }
    return endCommand;
  }

  public int getInterval() {
    return 5000;
  }

  private final void buildLocalCommand() {
    ArrayList command = new ArrayList();
    //If the user is a bash user, a bad .cshrc might cause local load average to
    //fail without causing any other symptoms.  So its safer to use the bash
    //shell for a bash user.
    String bashShell = "bash";
    String shell = EnvironmentVariable.INSTANCE.getValue(manager
        .getPropertyUserDir(), "SHELL", AxisID.ONLY);
    if (shell != null && shell.equals(bashShell)) {
      command.add(bashShell);
    }
    else {
      command.add("tcsh");
    }
    int commandSize = command.size();
    localCommandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      localCommandArray[i] = (String) command.get(i);
    }
  }

  private final void buildRemoteCommand() {
    ArrayList command = new ArrayList();
    command.add("ssh");
    //prevents ssh from waiting for an answer when connecting to a computer for
    //the first time
    //see man ssh_config
    command.add("-x");
    command.add("-o");
    command.add("StrictHostKeyChecking=no");
    command.add("-o");
    //maximum connection timeout for a down computer
    command.add("ConnectTimeout=5");
    command.add("-o");
    //prevents password prompts when the publickey authentication fails
    command.add("PreferredAuthentications=publickey");
    command.add("-v");
    command.add(computer);

    int commandSize = command.size();
    remoteCommandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      remoteCommandArray[i] = (String) command.get(i);
    }
  }

  public boolean notifySentIntermittentCommand() {
    return true;
  }

  private final void buildIntermittentCommand() {
    if (Utilities.isWindowsOS()) {
      intermittentCommand = "imodwincpu";
    }
    else {
      intermittentCommand = "w";
    }
  }

  private final void buildEndCommand() {
    endCommand = "exit";
  }

  public final String getComputer() {
    return computer;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.8  2007/02/22 20:34:58  sueh
 * <p> bug# 964 In buildLocalCommand, matching the shell to the $SHELL variable if
 * <p> $SHELL equals bash.
 * <p>
 * <p> Revision 1.7  2006/02/08 03:34:40  sueh
 * <p> bug# 796 Use imodwindcpu instead of w for windows.
 * <p>
 * <p> Revision 1.6  2005/12/01 00:23:09  sueh
 * <p> bug# 775   The command that makes the connection can be different
 * <p> depending on whether is it talking to local or remote computer.  Removed
 * <p> getCommand and added getLocalCommand and getRemoteCommand.
 * <p> This is interface is also about distributing commands across multiple
 * <p> computers.  Remove getKey and added getComputer.
 * <p>
 * <p> Revision 1.5  2005/10/27 00:21:57  sueh
 * <p> bug# 745 ssh should be run with -x.  Hoping this will solve the XServer
 * <p> problem.
 * <p>
 * <p> Revision 1.4  2005/09/14 20:20:40  sueh
 * <p> bug# 532 Added notifySentIntermittentCommand() so that notifying the
 * <p> monitor that the intermittent command was sent can be optional.
 * <p>
 * <p> Revision 1.3  2005/09/09 21:21:01  sueh
 * <p> bug# 532 Made LoadAverageParam an n'ton (one for each computer) so
 * <p> that there aren't IntermittentSystemPrograms then computers.  This allows
 * <p> IntermittentSystemProgram to be used for other things and conforms to
 * <p> it definition of having one instance per IntermittentCommand, instead of
 * <p> one instance per computer.
 * <p>
 * <p> Revision 1.2  2005/08/24 00:19:36  sueh
 * <p> bug #532 Added getEndCommand().  The exit command used in
 * <p> IntermittentSystemProgram should be generic.
 * <p>
 * <p> Revision 1.1  2005/08/22 16:04:33  sueh
 * <p> bug# 532 Param object for getting this load average.  Currently tested
 * <p> only on Linux.  Needs to work for all three OSs.
 * <p> </p>
 */
