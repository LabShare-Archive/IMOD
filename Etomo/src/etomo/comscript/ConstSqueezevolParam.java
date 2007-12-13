package etomo.comscript;

import java.io.File;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Properties;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.ConstMetaData;
import etomo.type.EtomoNumber;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.18  2007/11/06 19:07:16  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.17  2007/05/11 15:25:57  sueh
 * <p> bug# 964 Added getStringArray().
 * <p>
 * <p> Revision 1.16  2007/02/05 21:40:58  sueh
 * <p> bug# 962  Put EtomoNumber type info into an inner class.
 * <p>
 * <p> Revision 1.15  2006/05/22 22:36:01  sueh
 * <p> bug# 577 Added getCommand().
 * <p>
 * <p> Revision 1.14  2006/05/11 19:39:30  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 1.13  2006/04/06 18:49:18  sueh
 * <p> bug# 808 Implementing ProcessDetails.  Added Fields to pass requests to
 * <p> the generic gets.
 * <p>
 * <p> Revision 1.12  2006/01/20 20:46:08  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.11  2005/11/19 01:51:54  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 1.10  2005/10/27 00:10:07  sueh
 * <p> bug# 708 Squeezevol gets it input file only from TrimvolParam's output
 * <p> file.
 * <p>
 * <p> Revision 1.9  2005/07/29 00:44:36  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.8  2005/07/26 17:27:25  sueh
 * <p> bug# 701 Get the PID from squeezevol
 * <p>
 * <p> Revision 1.7  2005/04/25 20:38:57  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.6  2005/01/25 21:27:05  sueh
 * <p> Renaming EtomoNumber.resetValue to displayValue.
 * <p>
 * <p> Revision 1.5  2005/01/08 01:33:40  sueh
 * <p> bug# 578 Changed the names of the statics used to make variables
 * <p> available in the Command interface.  Add GET_.
 * <p>
 * <p> Revision 1.4  2004/12/16 02:12:28  sueh
 * <p> bug# 564 Implemented Command.  Saved flipped status.
 * <p>
 * <p> Revision 1.3  2004/12/14 21:32:29  sueh
 * <p> bug# 557 Made separate variables for x and y reduction factors to handle
 * <p> an unflipped tomogram.
 * <p>
 * <p> Revision 1.2  2004/12/08 21:19:54  sueh
 * <p> bug# 564 Changed TrimvolParam set and get, input and output File
 * <p> functions to ...FileName to avoid confusion with the new getOutputFile()
 * <p> function.
 * <p>
 * <p> Revision 1.1  2004/12/02 18:24:23  sueh
 * <p> bug# 557 Manages squeezevol parameters.  Creates command line.
 * <p> Stores parameters.
 * <p> </p>
 */
public abstract class ConstSqueezevolParam implements CommandDetails, Storable {
  public static final String rcsid = "$Id$";

  protected static final String groupString = "Squeezevol";
  protected static final String linearInterpolationString = "LinearInterpolation";
  protected static final boolean defaultLinearInterpolation = false;
  private static final int commandSize = 3;
  private static final String commandName = "squeezevol";

  protected EtomoNumber reductionFactorX = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, "ReductionFactorX");
  protected EtomoNumber reductionFactorY = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, "ReductionFactorY");
  protected EtomoNumber reductionFactorZ = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, "ReductionFactorZ");
  protected boolean linearInterpolation;
  protected boolean flipped = false;
  private String[] commandArray = null;
  private File outputFile;
  private final ApplicationManager manager;

  public ConstSqueezevolParam(ApplicationManager manager) {
    this.manager = manager;
    reductionFactorX.setDisplayValue(1.25);
    reductionFactorY.setDisplayValue(1.25);
    reductionFactorZ.setDisplayValue(1.25);
    reset();
  }

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  protected void reset() {
    reductionFactorX.reset();
    reductionFactorY.reset();
    reductionFactorZ.reset();
    linearInterpolation = defaultLinearInterpolation;
  }

  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    options.add("-x");
    options.add(reductionFactorX.toString());
    options.add("-y");
    options.add(reductionFactorY.toString());
    options.add("-z");
    options.add(reductionFactorZ.toString());
    if (linearInterpolation) {
      options.add("-l");
    }
    options.add("-P");
    //create input file name
    String inputFileName;
    ConstMetaData metaData = manager.getMetaData();
    String datasetName = metaData.getDatasetName();
    //try to take the trimvol output file as input
    inputFileName = getInputFileName(datasetName);
    options.add(inputFileName);
    //output is dataset.sqz
    outputFile = new File(manager.getPropertyUserDir(), datasetName + ".sqz");
    options.add(outputFile.getName());
    return options;
  }

  private final String getInputFileName(String datasetName) {
    return TrimvolParam.getOutputFileName(datasetName);
  }

  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";

    reductionFactorX.store(props, prepend);
    reductionFactorY.store(props, prepend);
    reductionFactorZ.store(props, prepend);
    props.setProperty(group + linearInterpolationString, Boolean
        .toString(linearInterpolation));
  }

  protected static String createPrepend(String prepend) {
    if (prepend == "") {
      return groupString;
    }
    return prepend + "." + groupString;
  }

  private void createCommand() {
    ArrayList options = genOptions();
    commandArray = new String[options.size() + commandSize];
    commandArray[0] = "tcsh";
    commandArray[1] = "-f";
    commandArray[2] = BaseManager.getIMODBinPath() + commandName;
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
  }

  /**
   * Get command array used to run command.  Not for running the command
   * @return
   */
  public String getCommandLine() {
    if (commandArray == null) {
      return "";
    }
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }

  /**
   * Get command array to run
   * @return
   */
  public String[] getCommandArray() {
    createCommand();
    return commandArray;
  }

  public boolean getBooleanValue(etomo.comscript.Field field) {
    if (field == Fields.FLIPPED) {
      return flipped;
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public float getFloatValue(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public Hashtable getHashtable(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public int getIntValue(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getCommandName() {
    return commandName;
  }

  public String getCommand() {
    return commandName;
  }

  public static String getName() {
    return commandName;
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public File getCommandOutputFile() {
    return outputFile;
  }

  public ConstEtomoNumber getReductionFactorX() {
    return reductionFactorX;
  }

  public ConstEtomoNumber getReductionFactorY() {
    return reductionFactorY;
  }

  public ConstEtomoNumber getReductionFactorZ() {
    return reductionFactorZ;
  }

  public boolean isLinearInterpolation() {
    return linearInterpolation;
  }

  public boolean isFlipped() {
    return flipped;
  }

  public boolean equals(ConstSqueezevolParam that) {
    if (!reductionFactorX.equals(that.reductionFactorX)) {
      return false;
    }
    if (!reductionFactorY.equals(that.reductionFactorY)) {
      return false;
    }
    if (!reductionFactorZ.equals(that.reductionFactorZ)) {
      return false;
    }
    if (linearInterpolation != that.linearInterpolation) {
      return false;
    }
    return true;
  }

  public static final class Fields implements etomo.comscript.Field {
    private Fields() {
    }

    public static final Fields FLIPPED = new Fields();
  }
}
