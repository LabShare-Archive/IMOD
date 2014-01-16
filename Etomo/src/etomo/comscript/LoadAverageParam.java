package etomo.comscript;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import etomo.Arguments.DebugLevel;
import etomo.BaseManager;
import etomo.EtomoDirector;
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

  private static Hashtable instances = new Hashtable();// one instance per computer

  private final String computer;
  private final BaseManager manager;
  private String[] localStartCommandArray = null;
  private String[] remoteStartCommandArray = null;
  private String intermittentCommand = null;
  private String endCommand = null;
  private DebugLevel debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();

  public final static LoadAverageParam getInstance(String computer, BaseManager manager) {
    LoadAverageParam loadAverageParam = (LoadAverageParam) instances.get(computer);
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

  public final String[] getLocalStartCommand() {
    if (localStartCommandArray == null) {
      buildLocalStartCommand();
    }
    return localStartCommandArray;
  }

  public final String[] getRemoteStartCommand() {
    if (remoteStartCommandArray == null) {
      buildRemoteStartCommand();
    }
    return remoteStartCommandArray;
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

  private final void buildLocalStartCommand() {
    ArrayList command = new ArrayList();
    if (Utilities.isWindowsOS()) {
      command.add("cmd");
    }
    else {
      // If the user is a bash user, a bad .cshrc might cause local load average to
      // fail without causing any other symptoms. So its safer to use the bash
      // shell for a bash user.
      // Use bash as the default. Use tcsh only when it is set in $SHELL.
      String tcshShell = "tcsh";
      String shell = EnvironmentVariable.INSTANCE.getValue(manager,
          manager.getPropertyUserDir(), "SHELL", AxisID.ONLY);
      if (shell != null && shell.indexOf(tcshShell) != -1) {
        command.add(tcshShell);
      }
      else {
        command.add("bash");
      }
    }
    int commandSize = command.size();
    localStartCommandArray = new String[commandSize];
    if (debug.isVerbose()) {
      System.err.print("local start command:");
    }
    for (int i = 0; i < commandSize; i++) {
      localStartCommandArray[i] = (String) command.get(i);
      if (debug.isVerbose()) {
        System.err.print((String) command.get(i) + " ");
      }
    }
    if (debug.isVerbose()) {
      System.err.println();
    }
  }

  private final void buildRemoteStartCommand() {
    List<String> command = SshParam.INSTANCE.getCommand(manager, false, computer);
    int commandSize = command.size();
    remoteStartCommandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      remoteStartCommandArray[i] = command.get(i);
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
 * <p> Revision 1.15  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.14  2009/04/13 22:23:13  sueh
 * <p> bug# 1207 Moved the building of the ssh command to SshParam.
 * <p>
 * <p> Revision 1.13  2009/03/17 00:32:02  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.12  2007/09/27 19:22:17  sueh
 * <p> bug# 1044 Changed command to startCommand.
 * <p>
 * <p> Revision 1.11  2007/09/14 16:01:09  sueh
 * <p> bug# 1043
 * <p>
 * <p> Revision 1.10  2007/09/14 15:56:17  sueh
 * <p> bug# 1043 Removed ConnectTimeout from ssh command.
 * <p>
 * <p> Revision 1.9  2007/05/24 23:39:11  sueh
 * <p> bug# 994 Added ConnectTimeout and PreferredAuthentications options to
 * <p> buildRemoteCommand().
 * <p>
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
