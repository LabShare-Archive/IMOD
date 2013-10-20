package etomo.comscript;

import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.TaskInterface;
import etomo.type.AxisID;
import etomo.type.FileType;
import etomo.type.ProcessName;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
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
public final class TomodataplotsParam {
  public static final String rcsid = "$Id:$";

  private Task task = null;

  public void setTask(final TaskInterface input) {
    if (input instanceof Task) {
      task = (Task) input;
    }
    else {
      task = null;
    }
  }

  public List<String> getCommandArray(final BaseManager manager, final AxisID axisID) {
    List<String> command = new ArrayList<String>();
    command.add("python");
    command.add("-u");
    command.add(BaseManager.getIMODBinPath() + ProcessName.TOMODATAPLOTS.toString());
    if (task != null) {
      if (task.typeOfDataToPlot != null) {
        command.add("-TypeOfDataToPlot");
        command.add(task.typeOfDataToPlot.value);
      }
      if (task.inputFile != null) {
        command.add("-InputFile");
        command.add(task.inputFile.getFileName(manager, axisID));
      }
      if (task.xaxisLabel != null) {
        command.add("-XaxisLabel");
        command.add(task.xaxisLabel);
      }
    }
    return command;
  }

  private static final class TypeOfDataToPlot {
    private static final TypeOfDataToPlot MEAN_MAX = new TypeOfDataToPlot("4");
    private static final TypeOfDataToPlot ROTATION = new TypeOfDataToPlot("5");
    private static final TypeOfDataToPlot TILT_SKEW = new TypeOfDataToPlot("6");
    private static final TypeOfDataToPlot MAG = new TypeOfDataToPlot("7");
    private static final TypeOfDataToPlot XSTRETCH = new TypeOfDataToPlot("8");
    private static final TypeOfDataToPlot RESID = new TypeOfDataToPlot("9");
    private static final TypeOfDataToPlot AVER_RESID = new TypeOfDataToPlot("10");
    private static final TypeOfDataToPlot MIN_MAX = new TypeOfDataToPlot("13");

    private final String value;

    private TypeOfDataToPlot(final String value) {
      this.value = value;
    }
  }

  public static final class Task implements TaskInterface {
    public static final Task MIN_MAX = new Task("Plot min/max",
        TomodataplotsParam.TypeOfDataToPlot.MIN_MAX, FileType.STATS_LOG,
        "View number in raw stack ");
    public static final Task FIXED_MIN_MAX = new Task("Plot fixed min/max",
        TomodataplotsParam.TypeOfDataToPlot.MIN_MAX, FileType.FIXED_STATS_LOG,
        "View number in fixed stack ");
    public static final Task COARSE_MEAN_MAX = new Task("Plot edge errors",
        TomodataplotsParam.TypeOfDataToPlot.MEAN_MAX, FileType.CROSS_CORRELATION_LOG);
    public static final Task ROTATION = new Task("Plot rotation",
        TomodataplotsParam.TypeOfDataToPlot.ROTATION, FileType.ALIGN_SOLUTION_LOG);
    public static final Task TILT_SKEW = new Task("Plot delta tilt and skew",
        TomodataplotsParam.TypeOfDataToPlot.TILT_SKEW, FileType.ALIGN_SOLUTION_LOG);
    public static final Task MAG = new Task("Plot magnification",
        TomodataplotsParam.TypeOfDataToPlot.MAG, FileType.ALIGN_SOLUTION_LOG);
    public static final Task XSTRETCH = new Task("Plot X-stretch (dmag)",
        TomodataplotsParam.TypeOfDataToPlot.XSTRETCH, FileType.ALIGN_SOLUTION_LOG);
    public static final Task RESID = new Task("Plot global mean residual",
        TomodataplotsParam.TypeOfDataToPlot.RESID, FileType.ALIGN_SOLUTION_LOG);
    public static final Task AVER_RESID = new Task(
        "Plot average of local mean residual",
        TomodataplotsParam.TypeOfDataToPlot.AVER_RESID, FileType.ALIGN_SOLUTION_LOG);
    public static final Task SERIAL_SECTIONS_MEAN_MAX = new Task("Plot edge errors",
        TomodataplotsParam.TypeOfDataToPlot.MEAN_MAX, FileType.PREBLEND_LOG);

    private final String label;
    private final TomodataplotsParam.TypeOfDataToPlot typeOfDataToPlot;
    private final FileType inputFile;
    private final String xaxisLabel;

    private Task(final String label,
        final TomodataplotsParam.TypeOfDataToPlot typeOfDataToPlot,
        final FileType inputFile) {
      this.label = label;
      this.typeOfDataToPlot = typeOfDataToPlot;
      this.inputFile = inputFile;
      this.xaxisLabel = null;
    }

    private Task(final String label,
        final TomodataplotsParam.TypeOfDataToPlot typeOfDataToPlot,
        final FileType inputFile, final String xaxisLabel) {
      this.label = label;
      this.typeOfDataToPlot = typeOfDataToPlot;
      this.inputFile = inputFile;
      this.xaxisLabel = xaxisLabel;
    }

    public boolean okToDrop() {
      return true;
    }

    public String toString() {
      return label;
    }
    
    public boolean isAvailable(final BaseManager manager,final AxisID axisID) {
    return  inputFile.exists(manager,axisID);
    }
  }
}
