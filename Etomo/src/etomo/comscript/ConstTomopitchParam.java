package etomo.comscript;

import java.util.Vector;

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
  public static final String rcsid =
    "$$Id$$";

  protected Vector modelFiles;
  protected double extraThickness;
  protected double spacingInY;
  protected double scaleFactor;
  protected String parameterFile;
  protected int numberOfModelFiles; //old-style comscript
  
  public ConstTomopitchParam() {
    reset();
  }
  
  protected void reset() {
    modelFiles = new Vector();
    extraThickness = Double.NaN;
    spacingInY = Double.NaN;
    scaleFactor = Double.NaN;
    parameterFile = new String();
    numberOfModelFiles = Integer.MIN_VALUE;
  }

  public int getModelFilesSize() {
    return modelFiles.size();
  }
  public String getModelFile(int index) {
    return (String) modelFiles.get(index);
  }
  public String getExtraThicknessString() {
    return ParamUtilities.getString(extraThickness);
  }
  public String getSpacingInYString() {
    return ParamUtilities.getString(spacingInY);
  }
  public String getScaleFactorString() {
    return ParamUtilities.getString(scaleFactor);
  }
  public String getParameterFile() {
    return parameterFile;
  }
  public int getNumberOfModelFiles() {
    return numberOfModelFiles;
  }
  
}
