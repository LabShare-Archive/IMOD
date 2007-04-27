package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.util.EnvironmentVariable;

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
 * <p> $Log$
 * <p> Revision 1.1  2007/04/26 02:43:17  sueh
 * <p> bug# 964 Represents the prmParser command.
 * <p> </p>
 */
public final class PeetParserParam {
  public static final String rcsid = "$Id$";

  private boolean debug = true;
  private ArrayList command = null;

  private final File prmFile;
  private final BaseManager manager;

  public PeetParserParam(BaseManager manager, File prmFile) {
    this.prmFile = prmFile;
    this.manager = manager;
  }

  public ArrayList getCommand() {
    if (command != null) {
      return command;
    }
    command = new ArrayList();
    command.add("sh");
    File commandFile = new File(new File(EnvironmentVariable.INSTANCE.getValue(
        manager.getPropertyUserDir(), "PARTICLE_DIR", AxisID.ONLY), "bin"),
        ProcessName.PEET_PARSER.toString());
    command.add(commandFile.getAbsolutePath());
    command.add(prmFile.getName());
    if (debug) {
      for (int i = 0; i < command.size(); i++) {
        if (i > 0) {
          System.err.print(" ");
        }
        System.err.print(command.get(i));
      }
      System.err.println();
    }
    return command;
  }
  
  public File getLogFile() {
    return new File(prmFile.getAbsolutePath()+".log");
  }
}
