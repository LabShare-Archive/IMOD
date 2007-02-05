package etomo.comscript;

import java.util.ArrayList;

import etomo.JoinManager;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstJoinState;
import etomo.type.IntKeyList;
import etomo.type.ProcessName;
import etomo.type.Transform;
import etomo.util.DatasetFiles;

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
public final class XfjointomoParam {
  public static final String rcsid = "$Id$";

  static final String COMMAND_NAME = ProcessName.XFJOINTOMO.toString();
  
  private static final boolean debug = true;
  private final JoinManager manager;
  private Transform transform = null;
  private String boundariesToAnalyze = null;
  private String pointsToFit = null;
  private String gapStartEndInc = null;
  private String objectsToInclude = null;
  private String[] commandArray = null;
  private final boolean trial;

  public XfjointomoParam(JoinManager manager, boolean trial) {
    this.manager = manager;
    this.trial = trial;
  }

  public String[] getCommandArray() {
    createCommandArray();
    return commandArray;
  }

  private void createCommandArray() {
    if (commandArray != null) {
      return;
    }
    ArrayList options = genOptions();
    commandArray = new String[options.size() + 1];
    commandArray[0] = COMMAND_NAME;
    int index = 1;
    for (int i = 0; i < options.size(); i++) {
      commandArray[index++] = (String) options.get(i);
    }
    if (debug) {
      StringBuffer buffer = new StringBuffer();
      for (int i=0;i< commandArray.length;i++) {
        buffer.append(commandArray[i]);
        if (i<commandArray.length-1) {
          buffer.append(' ');
        }
      }
      System.err.println(buffer.toString());
    }
  }

  public void setBoundariesToAnalyze(String boundariesToAnalyze) {
    this.boundariesToAnalyze = boundariesToAnalyze;
  }

  public void setObjectsToInclude(String objectsToInclude) {
    this.objectsToInclude = objectsToInclude;
  }

  public void setGapStartEndInc(String start, String end, String inc) {
    if (isNull(start) || isNull(end) || isNull(inc)) {
      gapStartEndInc = null;
    }
    else {
      gapStartEndInc = start + ',' + end + ',' + inc;
    }
  }

  public void setTransform(Transform transform) {
    this.transform = transform;
  }

  public void setPointsToFit(String min, String max) {
    if (isNull(min) || isNull(max)) {
      pointsToFit = null;
    }
    else {
      pointsToFit = min + ',' + max;
    }
  }

  private ArrayList genOptions() {
    ArrayList command = new ArrayList();
    command.add("-InputFile");
    command.add(DatasetFiles.getRefineModelFileName(manager));
    command.add("-FOutputFile");
    command.add(DatasetFiles.getRefineXfFileName(manager));
    command.add("-GOutputFile");
    command.add(DatasetFiles.getRefineJoinXgFileName(manager));
    command.add("-EditExistingFile");
    command.add("-SizesOfSections");
    ConstJoinState state = manager.getState();
    StringBuffer sizesOfSections = new StringBuffer();
    IntKeyList.Walker startListWalker = state.getJoinStartListWalker(trial);
    IntKeyList.Walker endListWalker = state.getJoinEndListWalker(trial);
    //check for valid lists
    if (startListWalker.size() == endListWalker.size()) {
      while (startListWalker.hasNext()) {
        ConstEtomoNumber start = startListWalker.nextNumeric();
        ConstEtomoNumber end = endListWalker.nextNumeric();
        if (start.gt(end)) {
          sizesOfSections.append(start.getLong() - end.getLong() + 1);
        }
        else {
          sizesOfSections.append(end.getLong() - start.getLong() + 1);
        }
        if (startListWalker.hasNext()) {
          sizesOfSections.append(',');
        }
      }
      command.add(sizesOfSections.toString());
    }
    command.add("-OffsetOfJoin");
    command.add(calcOffset(state.getJoinShiftInX(trial)) + ','
        + calcOffset(state.getJoinShiftInY(trial)));
    if (trial) {
    command.add("-BinningOfJoin");
    command.add(state.getJoinTrialBinning().toString());
    }
    if (!state.getJoinAlignmentRefSection(trial).isNull()) {
      command.add("-ReferenceSection");
      command.add(state.getJoinAlignmentRefSection(trial).toString());
    }
    if (transform == Transform.TRANSLATION) {
      command.add("-TranslationOnly");
    }
    else if (transform == Transform.ROTATION_TRANSLATION) {
      command.add("-RotationTranslation");
    }
    else if (transform == Transform.ROTATION_TRANSLATION_MAGNIFICATION) {
      command.add("-MagRotTrans");
    }
    if (!isNull(boundariesToAnalyze)) {
      command.add("-BoundariesToAnalyze");
      command.add(boundariesToAnalyze);
    }
    if (!isNull(pointsToFit)) {
      command.add("-PointsToFit");
      command.add(pointsToFit);
    }
    if (!isNull(gapStartEndInc)) {
      command.add("-GapStartEndInc");
      command.add(gapStartEndInc);
    }
    if (!isNull(objectsToInclude)) {
      command.add("-ObjectsToInclude");
      command.add(objectsToInclude);
    }
    return command;
  }

  private boolean isNull(String string) {
    if (string == null || string.equals("") || string.matches("\\s+")) {
      return true;
    }
    return false;
  }

  private String calcOffset(ConstEtomoNumber shift) {
    return String.valueOf(shift.getInt() * -1);
  }
}
