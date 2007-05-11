package etomo.comscript;

import java.io.File;
import java.util.Hashtable;

import etomo.BaseManager;
import etomo.storage.MatlabParam;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
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
 * <p> Revision 1.1  2007/04/27 23:38:09  sueh
 * <p> bug# 964 Changed prmParser() to peetParser.
 * <p>
 * <p> Revision 1.1  2007/04/26 02:43:17  sueh
 * <p> bug# 964 Represents the prmParser command.
 * <p> </p>
 */
public final class PeetParserParam implements CommandDetails{
  public static final String rcsid = "$Id$";

  private final File prmFile;
  private final BaseManager manager;
  
  private boolean debug = true;
  private String[] command = null;
  private int iterationListSize;
  private String[] lstThresholdsArray;

  public PeetParserParam(BaseManager manager, File prmFile) {
    this.prmFile = prmFile;
    this.manager = manager;
  }

  public String[] getCommandArray() {
    if (command != null) {
      return command;
    }
    command = new String[3];
    command[0]="sh";
    File commandFile = new File(new File(EnvironmentVariable.INSTANCE.getValue(
        manager.getPropertyUserDir(), "PARTICLE_DIR", AxisID.ONLY), "bin"),
        ProcessName.PEET_PARSER.toString());
    command[1]=commandFile.getAbsolutePath();
    command[2]=prmFile.getName();
    if (debug) {
      for (int i = 0; i < command.length; i++) {
        if (i > 0) {
          System.err.print(" ");
        }
        System.err.print(command[i]);
      }
      System.err.println();
    }
    return command;
  }
  
  public File getLogFile() {
    return new File(prmFile.getAbsolutePath()+".log");
  }
  
  public void getParameters(MatlabParam matlabParam) {
    iterationListSize=matlabParam.getIterationListSize();
    lstThresholdsArray=matlabParam.getLstThresholdsArray();
  }
  
  public CommandMode getCommandMode() {
    return null;
  }
  
  public File getCommandOutputFile() {
    return null;
  }
  
  public String getCommandName() {
    return null;
  }
  
  public String getCommandLine() {
    return getCommand();
  }
  
  public AxisID getAxisID() {
    return AxisID.ONLY;
  }
  
  public String getCommand() {
    String command=ProcessName.PEET_PARSER.getComscript(AxisID.ONLY);
    if (debug) {
      System.err.println(command);
    }
    return command;
  }
  
  public int getIntValue(etomo.comscript.Fields field) {
    if (field == Fields.ITERATION_LIST_SIZE) {
      return iterationListSize;
    }
    throw new IllegalArgumentException("field=" + field);
  }
  
  public String[] getStringArray(etomo.comscript.Fields field) {
    if (field==Fields.LST_THRESHOLDS_ARRAY) {
      return lstThresholdsArray;
    }
    throw new IllegalArgumentException("field=" + field);
  }
  
  public boolean getBooleanValue(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }
  public double getDoubleValue(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }
  public Hashtable getHashtable(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }
  public ConstEtomoNumber getEtomoNumber(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }
  public ConstIntKeyList getIntKeyList(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }
  public String getString(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }
  
  public static final class Fields implements etomo.comscript.Fields {
    private Fields() {
    }
    
    public static final Fields ITERATION_LIST_SIZE = new Fields();
    public static final Fields LST_THRESHOLDS_ARRAY = new Fields();
  }
}
