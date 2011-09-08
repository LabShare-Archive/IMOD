package etomo.comscript;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.type.EtomoNumber;
import etomo.type.ProcessName;

/**
 * <p>Description: Chunksetup for NAD.</p>
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
 * <p> $Log$
 * <p> Revision 1.4  2011/06/27 20:46:12  sueh
 * <p> Bug# 1500 Chunksetup converted to python.
 * <p>
 * <p> Revision 1.3  2011/02/21 21:11:47  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2010/01/21 21:29:13  sueh
 * <p> bug# 1305 Added -p, overlap, and -no.
 * <p>
 * <p> Revision 1.1  2007/11/06 19:05:45  sueh
 * <p> bug# 1047 Represents the parameters of chunksetup.
 * <p> </p>
 */
public final class ChunksetupParam {
  public static final String rcsid = "$Id$";

  public static final int MEMORY_TO_VOXEL = 36;
  private static final int OVERLAP_MIN = 8;

  private final List command = new ArrayList();
  private final EtomoNumber megavoxelsPerChunk = new EtomoNumber();

  private String subdirName = null;
  private String commandFile = null;
  private String inputFile = "";
  private String outputFile = "";
  private boolean debug = true;
  private int overlap = OVERLAP_MIN;
  private boolean overlapTimesFour = false;

  public ChunksetupParam() {
  }

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

  /**
   * @param input should be an Integer
   */
  public void setOverlap(Number input) {
    overlap = input.intValue();
  }

  public void setOverlapTimesFour(boolean input) {
    overlapTimesFour = input;
  }

  private void buildCommand() {
    command.clear();
    command.add("python");
    command.add("-u");
    command.add(BaseManager.getIMODBinPath() + ProcessName.CHUNKSETUP.toString());
    command.add("-p");
    command.add("0");
    command.add("-o");
    //Set -o to overlap or 4 times overlap.  Minimum is OVERLAP_MIN.
    int calcOverlap = overlap;
    if (overlapTimesFour) {
      calcOverlap *= 4;
    }
    if (calcOverlap < OVERLAP_MIN) {
      command.add(String.valueOf(OVERLAP_MIN));
    }
    else {
      command.add(String.valueOf(calcOverlap));
    }
    command.add("-m");
    command.add(megavoxelsPerChunk.toString());
    command.add("-no");
    File subdir = new File(subdirName);
    command.add(new File(subdir, commandFile).getPath());
    command.add(".." + File.separator + inputFile);
    command.add(".." + File.separator + outputFile);
    if (debug) {
      for (int i = 0; i < command.size(); i++) {
        System.err.print(command.get(i) + " ");
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
