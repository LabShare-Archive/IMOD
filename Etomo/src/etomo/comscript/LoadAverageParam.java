package etomo.comscript;

import java.util.ArrayList;
import java.util.Hashtable;

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
  public static  final String  rcsid =  "$Id$";
  
  private static Hashtable instances = new Hashtable();//one instance per computer
  
  private final String computer;
  private String[] commandArray = null;
  private String intermittentCommand = null;
  private String endCommand = null;
  
  public final static LoadAverageParam getInstance(String computer) {
    LoadAverageParam loadAverageParam = (LoadAverageParam) instances.get(computer);
    if (loadAverageParam != null) {
      return loadAverageParam;
    }
    synchronized (instances) {
      loadAverageParam = (LoadAverageParam) instances.get(computer);
      if (loadAverageParam != null) {
        return loadAverageParam;
      }
      loadAverageParam = new LoadAverageParam(computer);
      instances.put(computer, loadAverageParam);
      return loadAverageParam;
    }
  }
  
  private LoadAverageParam(String computer) {
    this.computer = computer;
  }
  
  public final String[] getCommand() {
    if (commandArray == null) {
      buildCommand();
    }
    return commandArray;
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
  
  private final void buildCommand() {
    ArrayList command = new ArrayList();
    command.add("ssh");
    //prevents ssh from waiting for an answer when connecting to a computer for
    //the first time
    //see man ssh_config
    command.add("-o");
    command.add("StrictHostKeyChecking=no");
    command.add(computer);
    
    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
  }
  
  private final void buildIntermittentCommand() {
    intermittentCommand = "w";
  }
  
  private final void buildEndCommand() {
    endCommand = "exit";
  }

  public final String getKey() {
    return computer;
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2005/08/24 00:19:36  sueh
* <p> bug #532 Added getEndCommand().  The exit command used in
* <p> IntermittentSystemProgram should be generic.
* <p>
* <p> Revision 1.1  2005/08/22 16:04:33  sueh
* <p> bug# 532 Param object for getting this load average.  Currently tested
* <p> only on Linux.  Needs to work for all three OSs.
* <p> </p>
*/