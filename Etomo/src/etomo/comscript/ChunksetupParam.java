package etomo.comscript;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.type.EtomoNumber;
import etomo.type.ProcessName;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
public final class ChunksetupParam {
  public static final String rcsid = "$Id$";

  public static final int MEMORY_TO_VOXEL = 36;

  private final List command = new ArrayList();
  private final EtomoNumber megavoxelsPerChunk = new EtomoNumber();

  private String subdirName = null;
  private String commandFile = null;
  private String inputFile = "";
  private String outputFile = "";
  private boolean debug = true;

  public void setMemoryPerChunk(Number memory) {
    megavoxelsPerChunk.set(memory.intValue() / MEMORY_TO_VOXEL);
  }

  public void setSubdirName(String input) {
    subdirName = input;
  }

  public void setCommandFile(String input) {
    commandFile = input;
  }

  public void setInputFile(String input) {
    inputFile = input;
  }

  public void setOutputFile(String input) {
    outputFile = input;
  }

  private void buildCommand() {
    command.clear();
    command.add("tcsh");
    command.add("-f");
    command.add(BaseManager.getIMODBinPath()
        + ProcessName.CHUNKSETUP.toString());
    command.add("-m");
    command.add(megavoxelsPerChunk.toString());
    File subdir = new File(subdirName);
    command.add(new File(subdir, commandFile).getPath());
    command.add(".." + File.separator + inputFile);
    command.add(".." + File.separator + outputFile);
    if (debug) {
      for (int i = 0;i<command.size();i++) {
        System.err.print(command.get(i)+" ");
      }
      System.err.println();
    }
  }

  public String[] getCommandArray() {
    if (command.isEmpty()) {
      buildCommand();
    }
    if (command.isEmpty()) {
      return new String[0];
    }
    if (command.size() == 1) {
      return new String[] { (String) command.get(0) };
    }
    return (String[]) command.toArray(new String[command.size()]);
  }
}
