package etomo.comscript;

import java.util.Vector;

import etomo.type.EtomoNumber;
import etomo.type.ScriptParameter;

/**
 * <p>Description: A read only model of the parameter interface for the
 *  tiltxcorr program</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * Univeristy of Colorado</p>
 *
 * @author $$Author$$
 *
 * @version $$Revision$$
 *
 * <p> $$Log&& </p>
 */

public class ConstTomopitchParam {
  public static final String rcsid = "$$Id$$";

  public static final String COMMAND = "tomopitch";
  public static final String MODEL_FILE = "ModelFile";
  public static final String EXTRA_THICKNESS = "ExtraThickness";
  public static final String SPACING_IN_Y = "SpacingInY";
  public static final String SCALE_FACTOR = "ScaleFactor";
  public static final String PARAMETER_FILE = "ParameterFile";

  protected Vector modelFiles;
  protected double extraThickness;
  protected double spacingInY;
  protected double scaleFactor;
  protected String parameterFile;
  protected ScriptParameter angleOffsetOld = new ScriptParameter(EtomoNumber.Type.DOUBLE,
      "AngleOffsetOld");
  protected ScriptParameter zShiftOld = new ScriptParameter(EtomoNumber.Type.DOUBLE,
      "ZShiftOld");
  protected ScriptParameter xAxisTiltOld = new ScriptParameter(EtomoNumber.Type.DOUBLE,
      "XAxisTiltOld");

  public ConstTomopitchParam() {
    reset();
  }

  protected void reset() {
    modelFiles = new Vector();
    extraThickness = Double.NaN;
    spacingInY = Double.NaN;
    scaleFactor = Double.NaN;
    parameterFile = new String();
    angleOffsetOld.reset();
    zShiftOld.reset();
    xAxisTiltOld.reset();
  }

  public int getModelFilesSize() {
    return modelFiles.size();
  }

  public String getModelFile(int index) {
    return (String) modelFiles.get(index);
  }

  public String getExtraThicknessString() {
    return ParamUtilities.valueOf(extraThickness);
  }

  public String getSpacingInYString() {
    return ParamUtilities.valueOf(spacingInY);
  }

  public String getScaleFactorString() {
    return ParamUtilities.valueOf(scaleFactor);
  }

  public String getParameterFile() {
    return parameterFile;
  }

  /**
   * Return a multiline string describing the class attributes.
   */
  public String toString() {
    StringBuffer string = new StringBuffer("\n");
    string.append("ModelFilesSize:" + getModelFilesSize() + "\n");
    for (int i = 0; i < getModelFilesSize(); i++) {
      string.append(MODEL_FILE + ":" + getModelFile(i) + "\n");
    }
    string.append(EXTRA_THICKNESS + ":" + getExtraThicknessString() + "\n");
    string.append(SCALE_FACTOR + ":" + getScaleFactorString() + "\n");
    string.append(SPACING_IN_Y + ":" + getSpacingInYString() + "\n");
    string.append(PARAMETER_FILE + ":" + getParameterFile() + "\n");
    return string.toString();

  }
}
